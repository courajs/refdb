use std::{convert::TryInto, fmt, fmt::Display};

use hex_literal::hex;
use nom::{
    bytes::complete::take,
    call,
    combinator::map,
    length_count,
    number::complete::{be_u64, be_u8},
    sequence::tuple,
    switch, IResult,
};

use crate::core::*;
use crate::error::MonsterError;

pub const BLOB_TYPE_HASH: Hash = Hash(hex!(
    "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
));
pub const RADT_TYPE_HASH: Hash = Hash(hex!(
    "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001"
));

pub const BLOB_TYPE_REF: TypeRef = TypeRef {
    definition: BLOB_TYPE_HASH,
    item: 0,
};
pub const RADT_TYPE_REF: TypeRef = TypeRef {
    definition: RADT_TYPE_HASH,
    item: 0,
};

pub struct TypeSpec<'a> {
    pub definition: &'a RADT,
    pub item: usize,
}

impl TypeSpec<'_> {
    pub fn item(&self) -> &RADTItem {
        &self.definition.items[self.item]
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedValue {
    pub kind: TypeRef,
    pub value: RADTValue,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TypeRef {
    pub definition: Hash,
    pub item: usize,
}
impl Serializable for TypeRef {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        v.extend_from_slice(&self.definition.0);
        v.extend_from_slice(&self.item.to_be_bytes());
    }
}
impl Decodable for TypeRef {
    fn decode(bytes: &[u8]) -> IResult<&[u8], TypeRef> {
        map(
            tuple((Hash::decode, usize::decode)),
            |(definition, item)| TypeRef { definition, item },
        )(bytes)
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.definition {
            BLOB_TYPE_HASH => write!(f, "<blobref>"),
            RADT_TYPE_HASH => write!(f, "<type>"),
            _ => {
                write!(f, "#")?;
                for c in &self.definition.0[..4] {
                    write!(f, "{:02x}", c)?;
                }
                write!(f, ":{}", self.item)
            }
        }
    }
}

impl Serializable for usize {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        v.extend_from_slice(&self.to_be_bytes());
    }
}
impl Decodable for usize {
    fn decode(bytes: &[u8]) -> IResult<&[u8], usize> {
        map(take(8u8), from_sure_be_bytes)(bytes)
    }
}

fn from_sure_be_bytes(bytes: &[u8]) -> usize {
    let mut a = [0; 8];
    a.copy_from_slice(bytes);
    usize::from_be_bytes(a)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Typing {
    // Either this typing represents a type or a value.
    // If it represents a type, then type_hash will be a special value
    // that means type (1 for adts, 2 for custom types, etc)
    // Otherwise, if it's a value, it will be the hash of a typing
    // which represents a type.
    // If this contained the hash of a blob, it would be invalid.
    pub kind: TypeRef,
    // the actual data blob
    pub data: Hash,
}

impl Serializable for Typing {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        self.kind.bytes_into(v);
        self.data.bytes_into(v);
    }
}

impl Decodable for Typing {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Typing> {
        map(tuple((TypeRef::decode, Hash::decode)), |(kind, data)| {
            Typing { kind, data }
        })(bytes)
    }
}

// same structure as Typing, but the Hash fields mean different things.
// For typing, that's the hash of the blob containing the value. For this
// one, it's a hash which should point to a typing of the same type.
#[derive(Debug, PartialEq, Eq)]
pub struct ExpectedTyping {
    pub reference: Hash,
    pub kind: TypeRef,
}

fn parse_hash(bytes: &[u8]) -> Result<Hash, MonsterError> {
    if bytes.len() >= 32 {
        Ok(Hash::sure_from(&bytes[..32]))
    } else {
        Err(MonsterError::Incomplete("a hash"))
    }
}

// recursive algebraic data type
// allows cyclical references
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RADT {
    pub uniqueness: [u8; 16],
    pub items: Vec<RADTItem>,
}

impl Serializable for RADT {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        v.extend_from_slice(&self.uniqueness);
        v.extend_from_slice(&self.items.len().to_be_bytes());
        for item in self.items.iter() {
            item.bytes_into(v);
        }
    }
}

impl Decodable for RADT {
    fn decode(bytes: &[u8]) -> IResult<&[u8], RADT> {
        let (more, uniq) = take(16u8)(bytes)?;
        let (rest, items) = length_count!(more, be_u64, RADTItem::decode)?;

        let mut uniqueness = [0; 16];
        uniqueness.copy_from_slice(uniq);
        Ok((rest, RADT { uniqueness, items }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RADTItem {
    // Reference to a separate RADT - the hash and cycle index.
    ExternalType(TypeRef),
    Sum(Vec<RADTItem>),
    Product(Vec<RADTItem>),
    CycleRef(usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RADTValue {
    Hash(Hash),
    Sum { kind: u8, value: Box<RADTValue> },
    Product(Vec<RADTValue>),
}

impl Serializable for RADTValue {
    fn bytes_into(&self, v: &mut Vec<u8>) {
        match self {
            RADTValue::Hash(h) => v.extend_from_slice(&h.0[..]),
            RADTValue::Sum { kind, value } => {
                v.push(*kind);
                value.bytes_into(v);
            }
            RADTValue::Product(subs) => {
                for sub in subs {
                    sub.bytes_into(v);
                }
            }
        }
    }
}

// We don't need to return the expected typings here because we only interpret bytes as an instance
// if there's already a typing, and we only make the typing if all the expected typings check out.
pub fn validate_radt_instance_bytes(
    t: &RADT,
    idx: usize,
    bytes: &[u8],
) -> Result<RADTValue, MonsterError> {
    let (value, rest) = inner_validate_radt_instance_bytes(&t.items, &t.items[idx], bytes)?;
    if rest.len() > 0 {
        Err(MonsterError::Excess(rest.len(), bytes.len()))
    } else {
        Ok(value)
    }
}

pub fn inner_validate_radt_instance_bytes<'a, 'b>(
    base_items: &'a [RADTItem],
    t: &'a RADTItem,
    bytes: &'b [u8],
) -> Result<(RADTValue, &'b [u8]), MonsterError> {
    match t {
        RADTItem::ExternalType(_) => Ok((RADTValue::Hash(parse_hash(bytes)?), &bytes[32..])),
        RADTItem::Sum(variants) => {
            if bytes.len() == 0 {
                return Err(MonsterError::Incomplete("a sum variant tag"));
            }
            let variant = bytes[0];
            if variant as usize >= variants.len() {
                return Err(MonsterError::InvalidSumVariant(
                    variants.len(),
                    variant as usize,
                ));
            }
            let (inner, rest) = inner_validate_radt_instance_bytes(
                base_items,
                &variants[variant as usize],
                &bytes[1..],
            )?;
            Ok((
                RADTValue::Sum {
                    kind: variant,
                    value: Box::new(inner),
                },
                rest,
            ))
        }
        RADTItem::Product(fields) => {
            let mut values = Vec::new();
            let mut rest = bytes;
            for field in fields {
                let (val, more) = inner_validate_radt_instance_bytes(base_items, field, rest)?;
                values.push(val);
                rest = more;
            }
            Ok((RADTValue::Product(values), rest))
        }
        RADTItem::CycleRef(idx) => {
            inner_validate_radt_instance_bytes(base_items, &base_items[*idx], bytes)
        }
    }
}

pub fn validate_radt_instance(
    t: &RADT,
    index: usize,
    value: &RADTValue,
) -> Result<Vec<ExpectedTyping>, MonsterError> {
    if index >= t.items.len() {
        Err(MonsterError::InvalidCycleRef(t.items.len(), index))
    } else {
        inner_validate_radt_instance(&t.items, &t.items[index], value)
    }
}

fn inner_validate_radt_instance(
    base_items: &[RADTItem],
    current_item: &RADTItem,
    value: &RADTValue,
) -> Result<Vec<ExpectedTyping>, MonsterError> {
    match current_item {
        RADTItem::ExternalType(TypeRef {
            definition: def,
            item: idx,
        }) => match value {
            RADTValue::Sum { .. } => Err(MonsterError::Mismatch("hash", "sum")),
            RADTValue::Product(_) => Err(MonsterError::Mismatch("hash", "product")),
            RADTValue::Hash(val) => Ok(vec![ExpectedTyping {
                reference: *val,
                kind: TypeRef {
                    definition: *def,
                    item: *idx,
                },
            }]),
        },
        RADTItem::Sum(subs) => match value {
            RADTValue::Hash(_) => Err(MonsterError::Mismatch("sum", "hash")),
            RADTValue::Product(_) => Err(MonsterError::Mismatch("sum", "product")),
            RADTValue::Sum { kind, value } => {
                if (*kind as usize) >= subs.len() {
                    Err(MonsterError::InvalidSumVariant(subs.len(), *kind as usize))
                } else {
                    inner_validate_radt_instance(base_items, &subs[*kind as usize], value)
                }
            }
        },
        RADTItem::Product(subs) => match value {
            RADTValue::Hash(_) => Err(MonsterError::Mismatch("product", "hash")),
            RADTValue::Sum { .. } => Err(MonsterError::Mismatch("product", "sum")),
            RADTValue::Product(values) => {
                if subs.len() != values.len() {
                    Err(MonsterError::InvalidProductFieldCount(
                        subs.len(),
                        values.len(),
                    ))
                } else {
                    let mut results = Vec::new();
                    for i in 0..subs.len() {
                        let prereqs =
                            inner_validate_radt_instance(base_items, &subs[i], &values[i])?;
                        results.extend(prereqs);
                    }
                    Ok(results)
                }
            }
        },
        RADTItem::CycleRef(idx) => {
            if *idx >= base_items.len() {
                Err(MonsterError::InvalidCycleRef(base_items.len(), *idx))
            } else {
                inner_validate_radt_instance(base_items, &base_items[*idx], value)
            }
        }
    }
}

fn radt_decode_external(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
    map(
        tuple((
            map(take(32u8), Hash::sure_from),
            map(take(8u8), |b: &[u8]| {
                usize::from_be_bytes(b.try_into().unwrap())
            }),
        )),
        |(h, idx)| {
            RADTItem::ExternalType(TypeRef {
                definition: h,
                item: idx,
            })
        },
    )(bytes)
}
fn radt_decode_sum(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
    map(radt_decode_items, |items| RADTItem::Sum(items))(bytes)
}
fn radt_decode_product(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
    map(radt_decode_items, |items| RADTItem::Product(items))(bytes)
}
fn radt_decode_items(bytes: &[u8]) -> IResult<&[u8], Vec<RADTItem>> {
    length_count!(bytes, be_u64, RADTItem::decode)
}
fn radt_decode_cycle_ref(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
    let (rest, i) = be_u64(bytes)?;
    Ok((rest, RADTItem::CycleRef(i as usize)))
}

impl Decodable for RADTItem {
    fn decode(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
        switch!(
            bytes,
            be_u8,
            0 => call!(radt_decode_external)
            | 1 => call!(radt_decode_sum)
            | 2 => call!(radt_decode_product)
            | 3 => call!(radt_decode_cycle_ref)
        )
    }
}
impl RADT {
    fn normalize(&mut self) -> Vec<usize> {
        // theoretically there should be a much better impl with no clones and simple swaps
        // but that is for another day
        let len = self.items.len();

        let mut sort_mapping: Vec<usize> = (0..len).collect();
        sort_mapping.sort_by_cached_key(|i| Hash::of(&self.items[*i].zero_bytes()));
        transpose(&mut sort_mapping);

        // dbg!(&sort_mapping);

        let orig = self.items.clone();
        for (i, item) in orig.into_iter().enumerate() {
            self.items[sort_mapping[i]] = item;
        }

        for item in self.items.iter_mut() {
            item.update_refs(&sort_mapping);
        }

        sort_mapping
    }
}

fn transpose(v: &mut Vec<usize>) {
    let copy = v.clone();
    for (i, val) in copy.into_iter().enumerate() {
        v[val] = i
    }
}

impl Serializable for RADTItem {
    fn bytes_into(&self, result: &mut Vec<u8>) {
        match self {
            RADTItem::ExternalType(TypeRef {
                definition: h,
                item: idx,
            }) => {
                result.push(0);
                result.extend_from_slice(&h.0[..]);
                result.extend_from_slice(&idx.to_be_bytes());
            }
            RADTItem::Sum(items) => {
                // TODO: maybe sort these somehow for easier structural comparison?
                result.push(1);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    item.bytes_into(result);
                }
            }
            RADTItem::Product(items) => {
                result.push(2);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    item.bytes_into(result);
                }
            }
            RADTItem::CycleRef(index) => {
                result.push(3);
                result.extend_from_slice(&index.to_be_bytes());
            }
        }
    }
}

impl RADTItem {
    fn zero_bytes(&self) -> Vec<u8> {
        let mut v = Vec::new();
        self.zero_bytes_into(&mut v);
        v
    }

    fn zero_bytes_into(&self, result: &mut Vec<u8>) {
        match self {
            RADTItem::ExternalType(TypeRef {
                definition: h,
                item: idx,
            }) => {
                result.push(0);
                result.extend_from_slice(&h.0[..]);
                result.extend_from_slice(&idx.to_be_bytes());
            }
            RADTItem::Sum(items) => {
                // TODO: maybe sort these somehow for easier structural comparison?
                result.push(1);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    item.zero_bytes_into(result);
                }
            }
            RADTItem::Product(items) => {
                result.push(2);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    item.zero_bytes_into(result);
                }
            }
            // here's where the zeroing comes in
            RADTItem::CycleRef(_) => {
                result.push(3);
                result.extend_from_slice(&(0 as usize).to_be_bytes());
            }
        }
    }

    fn update_refs(&mut self, map: &[usize]) {
        match self {
            RADTItem::ExternalType(_) => {}
            RADTItem::CycleRef(n) => {
                *n = map[*n];
            }
            RADTItem::Sum(items) => {
                for sub in items {
                    sub.update_refs(map);
                }
            }
            RADTItem::Product(items) => {
                for sub in items {
                    sub.update_refs(map);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typeref_display() {
        let mut t = TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        };
        assert_eq!(t.to_string(), "<blobref>");

        t = TypeRef {
            definition: RADT_TYPE_HASH,
            item: 0,
        };
        assert_eq!(t.to_string(), "<type>");

        t = TypeRef {
            definition: Hash(hex!(
                "ba5eba11 cafebabe 00000000 00000000 00000000 00000000 00000000 00000000"
            )),
            item: 22,
        };
        assert_eq!(t.to_string(), "#ba5eba11:22");

        t = TypeRef {
            definition: Hash(hex!(
                "01010101 cafebabe 00000000 00000000 00000000 00000000 00000000 00000000"
            )),
            item: 12,
        };
        assert_eq!(t.to_string(), "#01010101:12");
    }

    #[test]
    fn test_validate() {
        let list = RADTItem::Sum(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(2)]);
        let nil = RADTItem::Product(Vec::new());
        let cons = RADTItem::Product(vec![
            RADTItem::ExternalType(TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 12,
            }),
            RADTItem::CycleRef(0),
        ]);
        let blob_list = RADT {
            uniqueness: [0; 16],
            items: vec![list, nil, cons],
        };

        let value = RADTValue::Sum {
            kind: 1,
            value: Box::new(RADTValue::Product(vec![
                RADTValue::Hash(RADT_TYPE_HASH),
                RADTValue::Sum {
                    kind: 0,
                    value: Box::new(RADTValue::Product(Vec::new())),
                },
            ])),
        };

        let prereqs = validate_radt_instance(&blob_list, 0, &value).expect("should validate");

        assert_eq!(
            prereqs,
            vec![ExpectedTyping {
                kind: TypeRef {
                    definition: BLOB_TYPE_HASH,
                    item: 12,
                },
                reference: RADT_TYPE_HASH,
            }]
        );
    }

    #[test]
    fn test_radt_item_recode() {
        let item = RADTItem::Product(vec![RADTItem::CycleRef(12), RADTItem::CycleRef(12)]);
        let bytes = item.bytes();
        let (_empty, item2) = RADTItem::decode(&bytes).expect("hey");
        assert_eq!(item, item2);
    }

    #[test]
    fn test_radt_recode() {
        let blob_list = RADT {
            uniqueness: [0; 16],
            items: vec![
                RADTItem::Product(vec![]),
                RADTItem::Product(vec![
                    RADTItem::ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                    RADTItem::CycleRef(0),
                ]),
            ],
        };

        let bytes = blob_list.bytes();
        let (_, rehydrated) = RADT::decode(&bytes).expect("should parse the encoded radt");
        assert_eq!(rehydrated, blob_list);
    }

    #[test]
    fn test_normalize() {
        use RADTItem::*;
        // dbg!(hash_slice(&CycleRef(0).zero_bytes()));
        // > sha-256:53d4918ee44c2cb4ce8ba669bee35ff4f39b53e91bd79af80a841f63f8578faa
        // dbg!(hash_slice(&Product(vec![ExternalType(BLOB_TYPE_HASH, 0), CycleRef(2)]).zero_bytes()));
        // > sha-256:d785756371cd213bc86a7924489907934c5ff5f5f03568ac5deb457f80d2c196
        // dbg!(hash_slice(&Product(vec![CycleRef(0), ExternalType(BLOB_TYPE_HASH, 0)]).zero_bytes()));
        // > sha-256:089f61699620a1897360213f2f96626563bbb49c6c6235b32b9ac0c1f74bec16
        // dbg!(hash_slice(&Product(vec![CycleRef(1), CycleRef(2)]).zero_bytes()));
        // > sha-256:b5a18419a727b19bdcd967f99b0de7997da3646dd3afa878e43dd856249ad5db
        //
        // 2, 0, 3, 1

        let mut r = RADT {
            uniqueness: [0; 16],
            items: vec![
                CycleRef(0),
                Product(vec![
                    ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                    CycleRef(2),
                ]),
                Product(vec![
                    CycleRef(0),
                    ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                ]),
                Product(vec![CycleRef(1), CycleRef(2)]),
            ],
        };
        let expected = RADT {
            uniqueness: [0; 16],
            items: vec![
                Product(vec![
                    CycleRef(1),
                    ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                ]),
                CycleRef(1),
                Product(vec![CycleRef(3), CycleRef(0)]),
                Product(vec![
                    ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                    CycleRef(0),
                ]),
            ],
        };

        r.normalize();
        assert_eq!(r, expected);
    }

    #[test]
    fn test_radt_mapping() {
        let mut r = RADTItem::Product(vec![
            RADTItem::CycleRef(0),
            RADTItem::CycleRef(1),
            RADTItem::CycleRef(2),
            RADTItem::CycleRef(3),
        ]);

        let expected = RADTItem::Product(vec![
            RADTItem::CycleRef(2),
            RADTItem::CycleRef(1),
            RADTItem::CycleRef(3),
            RADTItem::CycleRef(0),
        ]);
        r.update_refs(&vec![2, 1, 3, 0]);
        assert_eq!(r, expected);
    }

    #[test]
    fn test_transpose() {
        let mut v = vec![3, 1, 0, 2];
        transpose(&mut v);
        assert_eq!(v, vec![2, 1, 3, 0])
    }
}
