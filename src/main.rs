#![allow(dead_code,unused_variables,unused_imports)]

// use std::env;
// use num::{BigUint};

use std::{
    fmt,
    path::Path,
    convert::{From, TryInto},
};

use failure::{Fail, Error, ResultExt};
use hex_literal::hex;
use rkv::{Value, Manager, Rkv, SingleStore, StoreOptions};
use sha3::{Digest, Sha3_256};
use nom::{
    IResult,
    switch, map, call, length_count,
    dbg as nom_dbg,
    bytes::complete::{take, tag},
    number::complete::{be_u8, be_u64},
    sequence::{preceded, tuple},
    combinator::{rest, rest_len, verify, map, all_consuming},
};

// Basic persistence primitives
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Hash(pub [u8; 32]);
impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl Hash {
    pub fn sure_from(value: &[u8]) -> Hash {
        let mut val = [0; 32];
        val.copy_from_slice(value);
        Hash(val)
    }
}

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sha-256:{}", hex::encode(self.0))
    }
}

pub trait Storable {
    fn bytes(&self) -> Vec<u8>;
    fn hash(&self) -> Hash {
        let mut val: [u8; 32] = Default::default();
        let mut hasher = Sha3_256::new();
        hasher.input(&self.bytes());
        val.copy_from_slice(hasher.result().as_ref());
        Hash(val)
    }
}

pub trait Decodable: Sized {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Self>;
}

pub struct Blob {
    pub bytes: Vec<u8>,
}
impl fmt::Debug for Blob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        if self.bytes.len() <= 32 {
            write!(f, "Blob(len={})<0x", self.bytes.len())?;
            for byte in &self.bytes {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, ">")?;
        } else {
            write!(f, "Blob(len={})<0x", self.bytes.len())?;
            for byte in &self.bytes[..16] {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, " ... ")?;
            for byte in &self.bytes[self.bytes.len() - 16..] {
                write!(f, "{:02x}", byte)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Storable for Blob {
    fn bytes(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(&self.bytes.capacity() + 1);
        // Blobs all have a leading 0 byte
        v.push(0);
        v.extend_from_slice(&self.bytes);
        v
    }
}

impl Decodable for Blob {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Blob> {
        Ok((&[], Blob { bytes: bytes.into() }))
    }
}

// Algebraic Data Type
pub struct ADT {
    // This is for types to be more like nominal types and less like
    // structural types.
    // Maybe see https://www.unisonweb.org/docs/language-reference/type-declarations#unique-types
    // Presumably this is generated randomly to minimize collisions with other types.
    pub uniqueness: [u8; 16],
    pub value: ADTItem,
}

// blob zero byte + uniqueness + ADTItem tag + a single hash (smallest variant)
const MIN_ADT_SIZE: usize = 1 + 16 + 1 + 32;
impl Decodable for ADT {
    fn decode(bytes: &[u8]) -> IResult<&[u8], ADT> {
        let (more, (uniq, value)) = tuple((take(16u8), ADTItem::decode))(bytes)?;

        let mut uniqueness = [0; 16];
        uniqueness.copy_from_slice(uniq);
        Ok((more, ADT { uniqueness, value }))
    }
}

impl fmt::Debug for ADT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "ADT {{ \n    uniqueness: 0x{},\n    value: {:#?} }}",
            &hex::encode(self.uniqueness),
            &self.value
        )
    }
}

#[derive(Debug)]
pub enum ADTItem {
    Hash(Hash),
    Sum(Vec<ADTItem>),
    Product(Vec<ADTItem>),
}
impl ADTItem {
    fn bytes(&self) -> Vec<u8> {
        let mut result = Vec::new();
        match self {
            ADTItem::Hash(h) => {
                result.push(0);
                result.extend_from_slice(&h.0[..]);
                result
            }
            ADTItem::Sum(items) => {
                // TODO: maybe sort these somehow for easier structural comparison?
                result.push(1);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    result.extend_from_slice(&item.bytes());
                }
                result
            }
            ADTItem::Product(items) => {
                result.push(2);
                result.extend_from_slice(&items.len().to_be_bytes());
                for item in items {
                    result.extend_from_slice(&item.bytes());
                }
                result
            }
        }
    }
}
impl Storable for ADT {
    fn bytes(&self) -> Vec<u8> {
        let mut v = Vec::new();
        // Blobs all have a leading 0 byte
        v.push(0);
        v.extend_from_slice(&self.uniqueness);
        v.extend_from_slice(&self.value.bytes());
        v
    }
}

fn decode_hash(bytes: &[u8]) -> IResult<&[u8], ADTItem> {
    map(take(32u8), |b| { ADTItem::Hash(Hash::sure_from(b)) })(bytes)
}
fn decode_sum(bytes: &[u8]) -> IResult<&[u8], ADTItem> {
    map(decode_items, |items| { ADTItem::Sum(items) })(bytes)
}
fn decode_product(bytes: &[u8]) -> IResult<&[u8], ADTItem> {
    map(decode_items, |items| { ADTItem::Product(items) })(bytes)
}
fn decode_items(bytes: &[u8]) -> IResult<&[u8], Vec<ADTItem>> {
    length_count!(bytes, be_u64, ADTItem::decode)
}

impl Decodable for ADTItem {
    fn decode(bytes: &[u8]) -> IResult<&[u8], ADTItem> {
        switch!(
            bytes,
            be_u8,
            0 => call!(decode_hash)
            | 1 => call!(decode_sum)
            | 2 => call!(decode_product)
        )
    }
}

pub static BLOB_TYPE_HASH: Hash = Hash(hex!(
    "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
));
pub static ADT_TYPE_HASH: Hash = Hash(hex!(
    "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001"
));

#[derive(Debug)]
pub struct Typing {
    // Either this typing represents a type or a value.
    // If it represents a type, then type_hash will a special value
    // that means type (1 for adts, 2 for custom types, etc)
    // Otherwise, if it's a value, it will be the hash of a typing
    // which represents a type.
    // If this contained the hash of a blob, it would be invalid.
    pub type_hash: Hash,
    // the actual data blob
    pub data_hash: Hash,
}

impl Storable for Typing {
    fn bytes(&self) -> Vec<u8> {
        let mut v = Vec::new();
        // Typings all have a leading 1 byte
        v.push(1);
        v.extend_from_slice(&self.type_hash.0[..]);
        v.extend_from_slice(&self.data_hash.0[..]);
        v
    }
}

// leading 1 to indicate a typing, then two hashes
const TYPING_SIZE: usize = 1 + 32 + 32;
impl Decodable for Typing {
    fn decode(bytes: &[u8]) -> IResult<&[u8], Typing> {
        let (rest, (type_hash, data_hash)) =
            tuple((map(take(32u8), Hash::sure_from), map(take(32u8), Hash::sure_from)))(bytes)?;
        Ok((rest, Typing { type_hash, data_hash }))
    }
}

#[derive(Debug)]
pub enum ADTValue {
    Hash(Hash),
    Sum {
        kind: u8,
        value: Box<ADTValue>,
    },
    Product(Vec<ADTValue>),
}

impl ADTValue {
    fn just_bytes(&self, v: &mut Vec<u8>) {
        match self {
            ADTValue::Hash(h) => {
                v.extend_from_slice(&h.0[..])
            },
            ADTValue::Sum {
                kind, value
            } => {
                v.push(*kind);
                value.just_bytes(v);
            },
            ADTValue::Product(subs) => {
                for sub in subs {
                    sub.just_bytes(v);
                }
            }
        }
    }

    pub fn hydrate(kind: &ADT, bytes: &[u8]) -> Result<ADTValue, BinaryADTInstiationError> {
        validate_adt_instance_bytes(kind, bytes)
    }
}

impl Storable for ADTValue {
    fn bytes(&self) -> Vec<u8> {
        let mut result = Vec::new();
        // Blob byte
        result.push(0);
        self.just_bytes(&mut result);
        result
    }
}


#[derive(Debug)]
pub struct ExpectedTyping {
    pub reference: Hash,
    pub kind: Hash,
}

#[derive(Debug)]
pub struct MaybeValid {
    pub typing: Typing,
    pub prereqs: Vec<ExpectedTyping>,
}

#[derive(Debug, Fail)]
pub enum BinaryADTInstiationError {
    #[fail(display = "Reached end of blob while parsing {}", _0)]
    Incomplete(&'static str),
    #[fail(display = "Invalid sum variant. There are {} options, but found variant tag {}", _0, _1)]
    InvalidSumVariant(usize, usize),
    #[fail(display = "Excess data at end of blob. Finished parsing with {} bytes remaining out of {} total", _0, _1)]
    Excess(usize, usize),
    #[fail(display = "A type's data_hash must point to a blob")]
    WrongType,
}

pub fn validate_adt_instance_bytes(t: &ADT, bytes: &[u8]) -> Result<ADTValue, BinaryADTInstiationError> {
    let (value, rest) = inner_validate_adt_instance_bytes(&t.value, bytes)?;
    if rest.len() > 0 {
        Err(BinaryADTInstiationError::Excess(rest.len(), bytes.len()))
    } else {
        Ok(value)
    }
}

pub fn inner_validate_adt_instance_bytes<'a, 'b>(t: &'a ADTItem, bytes: &'b [u8]) -> Result<(ADTValue, &'b [u8]), BinaryADTInstiationError> {
    match t {
        ADTItem::Hash(type_hash) => Ok((ADTValue::Hash(parse_hash(bytes)?), &bytes[32..])),
        ADTItem::Sum(variants) => {
            if bytes.len() == 0 {
                return Err(BinaryADTInstiationError::Incomplete("a sum variant tag"))
            }
            let variant = bytes[0];
            if variant as usize >= variants.len() {
                return Err(BinaryADTInstiationError::InvalidSumVariant(variants.len(), variant as usize))
            }
            let (inner, rest) = inner_validate_adt_instance_bytes(&variants[variant as usize], &bytes[1..])?;
            Ok((ADTValue::Sum {kind: variant, value: Box::new(inner)}, rest))
        },
        ADTItem::Product(fields) => {
            let mut values = Vec::new();
            let mut rest = bytes;
            for field in fields {
                let (val, more) = inner_validate_adt_instance_bytes(field, rest)?;
                values.push(val);
                rest = more;
            }
            Ok((ADTValue::Product(values), rest))
        }
    }
}

fn parse_hash(bytes: &[u8]) -> Result<Hash, BinaryADTInstiationError> {
    if bytes.len() >= 32 {
        Ok(Hash::sure_from(&bytes[..32]))
    } else {
        Err(BinaryADTInstiationError::Incomplete("a hash"))
    }
}

#[derive(Debug, Fail)]
pub enum StructuredADTInstantiationError {
    #[fail(display = "Expected a {}, found a {}", _0, _1)]
    Mismatch(&'static str, &'static str),
    #[fail(display = "Invalid sum variant. There are {} options, but found variant tag {}", _0, _1)]
    InvalidSumVariant(usize, usize),
    #[fail(display = "Invalid number of product fields. Expected {}, found {}", _0, _1)]
    InvalidProductFieldCount(usize, usize),
}

pub fn validate_adt_instance(t: &ADT, value: &ADTValue) -> Result<Vec<ExpectedTyping>, StructuredADTInstantiationError> {
    inner_validate_adt_instance(&t.value, value)
}

fn inner_validate_adt_instance(t: &ADTItem, value: &ADTValue) -> Result<Vec<ExpectedTyping>, StructuredADTInstantiationError> {
    match t {
        ADTItem::Hash(t) => {
            match value {
                ADTValue::Sum {..} => Err(StructuredADTInstantiationError::Mismatch("hash", "sum")),
                ADTValue::Product(_) => Err(StructuredADTInstantiationError::Mismatch("hash", "product")),
                ADTValue::Hash(v) => Ok(vec![ExpectedTyping {
                    reference: *v,
                    kind: *t,
                }]),
            }
        }
        ADTItem::Sum(subs) => {
            match value {
                ADTValue::Hash(_) => Err(StructuredADTInstantiationError::Mismatch("sum", "hash")),
                ADTValue::Product(_) => Err(StructuredADTInstantiationError::Mismatch("sum", "product")),
                ADTValue::Sum { kind, value: v } => {
                    if *kind as usize >= subs.len() {
                        Err(StructuredADTInstantiationError::InvalidSumVariant(subs.len(), *kind as usize))
                    } else {
                        inner_validate_adt_instance(&subs[*kind as usize], v)
                    }
                },
            }
        }
        ADTItem::Product(field_types) => {
            match value {
                ADTValue::Hash(_) => Err(StructuredADTInstantiationError::Mismatch("product", "hash")),
                ADTValue::Sum {..} => Err(StructuredADTInstantiationError::Mismatch("product", "sum")),
                ADTValue::Product(field_values) => {
                    if field_types.len() != field_values.len() {
                        Err(StructuredADTInstantiationError::InvalidProductFieldCount(field_types.len(), field_values.len()))
                    } else {
                        let num = field_types.len();
                        let mut hashes: Vec<ExpectedTyping> = Vec::with_capacity(num);
                        for i in 0..num {
                            let mut maybes = inner_validate_adt_instance(&field_types[i], &field_values[i])?;
                            hashes.append(&mut maybes);
                        }
                        Ok(hashes)
                    }
                },
            }
        }
    }
}

pub struct Db<'a> {
    pub env: &'a rkv::Rkv,
    pub store: &'a rkv::SingleStore,
}

#[derive(Debug)]
pub enum Item {
    Blob(Blob),
    BlobRef(Hash),
    Type(ADT),
    Value(ADT, ADTValue),
}

#[derive(Debug)]
pub enum LiteralItem {
    Blob(Blob),
    Typing(Typing),
}

#[derive(Debug, Fail)]
pub enum DBFailure {
    #[fail(display = "Error parsing {:?} from store: {:?}", _0, _1)]
    ParseError(Hash, String),
    #[fail(display = "RKV store error: {:?}", _0)]
    RkvError(#[cause] rkv::error::StoreError),
    #[fail(display = "Non-blob found in rkv store under hash {:?}", _0)]
    NonBlob(Hash),
    #[fail(display = "{:?} wasn't found in the store", _0)]
    NotFound(Hash),
    #[fail(display = "A type definition didn't point directly to bytes")]
    BrokenTypedef,
    #[fail(display = "A typing's type hash doesn't point to a type")]
    UntypedTyping,
    #[fail(display = "The typing {:?} couldn't be interpreted as a {:?}:\n{}", hash, target_type, err)]
    BrokenTyping {
        hash: Hash,
        target_type: Hash,
        #[cause] err: BinaryADTInstiationError,
    },
}

pub fn decode_item(bytes: &[u8]) -> IResult<&[u8], LiteralItem> {
    switch!(
        bytes,
        be_u8,
        0 => map!(call!(Blob::decode), |b| LiteralItem::Blob(b))
        | 1 => map!(call!(all_consuming(Typing::decode)), |t| LiteralItem::Typing(t))
    )
}

impl<'a> Db<'a> {
    pub fn put(&self, item: &impl Storable) -> Result<(), DBFailure> {
        // FIXME - handle errors properly here
        let mut writer = self.env.write().unwrap();
        if let Err(e) = self.store.put(&mut writer, &item.hash(), &Value::Blob(&item.bytes())) {
            return Err(DBFailure::RkvError(e));
        }
        if let Err(e) = writer.commit() {
            return Err(DBFailure::RkvError(e));
        }
        Ok(())
    }

    pub fn get_bytes(&self, hash: Hash) -> Result<Vec<u8>, DBFailure> {
        let reader = self.env.read().expect("reader");
        let r = self.store.get(&reader, &hash).map_err(|e| { DBFailure::RkvError(e) })?;
        match r {
            Some(Value::Blob(bytes)) => Ok(bytes.into()),
            Some(_)                  => Err(DBFailure::NonBlob(hash)),
            None                     => Err(DBFailure::NotFound(hash)),
        }
    }

    pub fn get(&self, hash: Hash) -> Result<Item, DBFailure> {
        let bytes = self.get_bytes(hash)?;
        match decode_item(&bytes) {
            Err(e) => Err(DBFailure::ParseError(hash, format!("{:?}", e))),
            Ok((more, LiteralItem::Blob(b))) => Ok(Item::Blob(b)),
            Ok((more, LiteralItem::Typing(typing))) => {
                if typing.type_hash == BLOB_TYPE_HASH {
                    return Ok(Item::BlobRef(typing.data_hash));
                } else if typing.type_hash == ADT_TYPE_HASH {
                    let definition_bytes = self.get_bytes(typing.data_hash)?;
                    let definition_blob = match decode_item(&definition_bytes).map_err(|e| DBFailure::ParseError(hash, format!("{:?}", e)))? {
                        (_, LiteralItem::Typing(_)) => Err(DBFailure::BrokenTypedef),
                        (_, LiteralItem::Blob(b)) => Ok(b),
                    }?;
                    let (more, kind) = all_consuming(ADT::decode)(&definition_blob.bytes).map_err(|e| DBFailure::ParseError(hash, format!("{:?}", e)))?;
                    return Ok(Item::Type(kind));
                } else {
                    match self.get(typing.type_hash)? {
                        Item::Blob(_) | Item::BlobRef(_) | Item::Value(_,_) => Err(DBFailure::UntypedTyping),
                        Item::Type(kind) => {
                            let instance_bytes = self.get_bytes(typing.data_hash)?;
                            let instance = ADTValue::hydrate(&kind, &instance_bytes[1..]).map_err(|e| DBFailure::BrokenTyping {
                                hash,
                                target_type: typing.type_hash,
                                err: e,
                            })?;
                            Ok(Item::Value(kind, instance))
                        },
                    }
                }
            }
        }
    }

    pub fn confirm_typings(&self, typings: &[ExpectedTyping]) -> Result<(), Error> {
        for expected in typings {
            let bytes = self.get_bytes(expected.reference)?;
            match decode_item(&bytes[..]) {
                Err(e) => {
                    Err(TypingApplicationFailure::ParseError(expected.reference))?;
                },
                Ok((_,LiteralItem::Blob(_))) => {
                    Err(TypingApplicationFailure::UntypedReference(expected.reference))?;
                },
                Ok((_,LiteralItem::Typing(typing))) => {
                    if typing.type_hash != expected.kind {
                        Err(TypingApplicationFailure::MistypedReference {
                            reference: expected.reference,
                            expected_type: expected.kind,
                            actual_type: typing.type_hash,
                        })?;
                    }
                }
            }
        }
        Ok(())
    }
}


#[derive(Debug, Fail)]
enum TypingApplicationFailure {
    #[fail(display = "error parsing sub-field {:?}", _0)]
    ParseError(Hash),
    #[fail(display = "sub-field referencing non-existant value {:?}", _0)]
    DanglingReference(Hash),
    #[fail(display = "found blob instead of typing for sub-field ({:?})", _0)]
    UntypedReference(Hash),
    #[fail(display = "prereq {:?} is of wrong type. Expected {:?}, found {:?}", reference, expected_type, actual_type)]
    MistypedReference {
        reference: Hash,
        expected_type: Hash,
        actual_type: Hash,
    }
}


fn main() -> Result<(), Error>{
    // let args: Vec<String> = env::args().collect();
    // println!("{:?}", args);

    let arc = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(Path::new("/Users/aaron/dev/rf0/data"), Rkv::new)
        .unwrap();
    let env = arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let db = Db {
        env: &env,
        store: &store,
    };

    let blob1 = Blob {
        bytes: b"abc"[..].into(),
    };
    let blob2 = Blob {
        bytes: b"xyz"[..].into(),
    };

    let t1 = Typing {
        type_hash: BLOB_TYPE_HASH,
        data_hash: blob1.hash(),
    };
    let t2 = Typing {
        type_hash: BLOB_TYPE_HASH,
        data_hash: blob2.hash(),
    };

    let mut uniq = [0; 16];
    uniq[0] = 254;
    uniq[15] = 239;
    let double_ref_type = ADT {
        uniqueness: uniq,
        value: ADTItem::Product(vec![
                    ADTItem::Hash(BLOB_TYPE_HASH),
                    ADTItem::Hash(BLOB_TYPE_HASH)
        ]),
    };

    let double_ref_typing = Typing {
        type_hash: ADT_TYPE_HASH,
        data_hash: double_ref_type.hash(),
    };

    let double_instance = ADTValue::Product(
        vec![ADTValue::Hash(t1.hash()), ADTValue::Hash(t2.hash())],
    );
    

    db.put(&blob1)?;
    db.put(&blob2)?;
    db.put(&t1)?;
    db.put(&t2)?;
    db.put(&double_ref_type)?;
    db.put(&double_ref_typing)?;
    db.put(&double_instance)?;

    let confirmations = validate_adt_instance(&double_ref_type, &double_instance)?;
    db.confirm_typings(&confirmations)?;

    let typing = Typing {
        type_hash: double_ref_typing.hash(),
        data_hash: double_instance.hash(),
    };

    db.put(&typing)?;

    // let typing = apply_typing_and_store(&db, &double_ref_type, &double_instance)?;

    // dbg!(typing.hash());
    // let bytes = db.get_bytes(typing.hash())?;
    // dbg!(decode_item(&bytes));
    // dbg!(double_ref_typing.hash());
    // dbg!(double_instance.hash());
    // dbg!(typing);

    if let Item::Value(kind, ADTValue::Product(subs)) = db.get(typing.hash())? {
        dbg!(kind);
        for b in subs {
            if let ADTValue::Hash(h) = b {
                if let Item::BlobRef(h2) = db.get(h)? {
                    if let Item::Blob(bb) = db.get(h2)? {
                        dbg!(std::str::from_utf8(&bb.bytes))?;
                    }
                }
            }
        }
        dbg!(blob1.hash());
        dbg!(blob2.hash());
    }

    // match db.get(typing.hash())? {
    //     None => println!("nah"),
    //     Some(bytes) => {
    //         println!("{}", bytes.len());
    //     },
    // }

    Ok(())
}
