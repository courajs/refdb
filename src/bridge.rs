// translations between rust structs and db values

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::any::TypeId;

use crate::core::*;
use crate::types::*;
use crate::labels::*;
use crate::storage::*;
use crate::error::MonsterError;

pub trait Bridged: Sized + 'static {
    fn radt() -> (RADT, TypeRef);
    fn to_value(&self) -> (TypedValue, Vec<Item>);
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError>;
    fn group_ids() -> HashSet<TypeId>;
}

pub trait FetchStrategy: Sized {
    fn hydrate(self_hash: Hash, db: &Db) -> Result<Self, MonsterError>;
}

pub trait DeserializeFromRADTValue: Sized {
    fn deserialize(val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError>;
}
impl DeserializeFromRADTValue for Hash {
    fn deserialize(val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        match val {
            RADTValue::Hash(h) => Ok(*h),
            _ => Err(MonsterError::Todo("mismatch deserializing hash"))
        }
    }
}
impl<T> DeserializeFromRADTValue for Box<T> where T: DeserializeFromRADTValue {
    fn deserialize(val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        Ok(Box::new(T::deserialize(val, deps)?))
    }
}
impl<T> DeserializeFromRADTValue for Vec<T> where T: DeserializeFromRADTValue {
    fn deserialize(mut val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        let mut result = Vec::new();
        while let RADTValue::Sum { kind: 1, value } = val {
            match value.deref() {
                RADTValue::Product(fields) if fields.len() == 2 => {
                    result.push(T::deserialize(&fields[0], deps)?);
                    val = &fields[1];
                },
                _ => return Err(MonsterError::Todo("mismatch deserializing vec cons"))
            }
        }
        match val {
            RADTValue::Sum { kind: 0, value } if **value == RADTValue::Product(Vec::new()) => {
                Ok(result)
            },
            _ => Err(MonsterError::Todo("mismatch deserializing vec nil"))
        }
    }
}
impl<K,V> DeserializeFromRADTValue for BTreeMap<K,V> where K: DeserializeFromRADTValue + Ord, V: DeserializeFromRADTValue {
    fn deserialize(mut val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        let mut result = Self::new();
        while let RADTValue::Sum { kind: 1, value } = val {
            match value.deref() {
                RADTValue::Product(fields) if fields.len() == 2 => { // entry list cons
                    match &fields[0] {
                        RADTValue::Product(fields) if fields.len() == 2 => { // key/val entry
                            result.insert(
                                K::deserialize(&fields[0], deps)?,
                                V::deserialize(&fields[1], deps)?,
                            );
                        },
                        _ => return Err(MonsterError::Todo("mismatch deserializing map entry"))
                    }
                    val = &fields[1];
                },
                _ => return Err(MonsterError::Todo("mismatch deserializing map cons"))
            }
        }
        match val {
            RADTValue::Sum { kind: 0, value } if **value == RADTValue::Product(Vec::new()) => {
                Ok(result)
            },
            _ => Err(MonsterError::Todo("mismatch deserializing map nil"))
        }
    }
}

impl DeserializeFromRADTValue for usize {
    fn deserialize(val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        let (_,t_usize) = Self::radt();
        use std::convert::TryInto;
        let body_hash = sure!(val, RADTValue::Hash(h) => h; return Err(MonsterError::BridgedMistypedDependency));
        let bytes = match deps.get(&body_hash) {
            Some(Item::Value(TypedValue{kind, value})) if *kind == t_usize => return usize::deserialize(value, deps),
            Some(Item::Blob(Blob{bytes})) => &bytes[..],
            None => return Err(MonsterError::BridgedMissingDependency("a usize's bytes")),
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        match bytes.try_into() {
            Ok(ary) => Ok(usize::from_be_bytes(ary)),
            _ => Err(MonsterError::Todo("Only 64-bit numbers can be decoded"))
        }
    }
}

pub trait SerializeToRADTValue {
    fn serialize(&self, deps: &mut Vec<Item>, group: &HashSet<TypeId>) -> RADTValue;
}
impl SerializeToRADTValue for Hash {
    fn serialize(&self, deps: &mut Vec<Item>, group: &HashSet<TypeId>) -> RADTValue {
        RADTValue::Hash(*self)
    }
}
impl<T> SerializeToRADTValue for T where T: Bridged {
    fn serialize(&self, deps: &mut Vec<Item>, group: &HashSet<TypeId>) -> RADTValue {
        let (tv, inner_deps) = self.to_value();
        deps.extend(inner_deps);
        if group.contains(&TypeId::of::<Self>()) {
            return tv.value
        } else {
            let h = tv.typing().hash();
            deps.push(Item::Value(tv));
            RADTValue::Hash(h)
        }
    }
}
impl<T> SerializeToRADTValue for Vec<T> where T: SerializeToRADTValue {
    fn serialize(&self, deps: &mut Vec<Item>, group: &HashSet<TypeId>) -> RADTValue {
        let empty = RADTValue::Sum{kind:0, value: Box::new(RADTValue::Product(Vec::new()))};
        self.iter().rfold(empty, |acc, v| {
            RADTValue::Sum {
                kind: 1,
                value: Box::new(
                    RADTValue::Product(vec![
                        v.serialize(deps, group),
                        acc,
                    ])
                )
            }
        })
    }
}
impl<K,V> SerializeToRADTValue for BTreeMap<K,V> where K: SerializeToRADTValue, V: SerializeToRADTValue {
    fn serialize(&self, deps: &mut Vec<Item>, group: &HashSet<TypeId>) -> RADTValue {
        let empty = RADTValue::Sum{kind:0, value: Box::new(RADTValue::Product(Vec::new()))};
        self.iter().rfold(empty, |acc, (k, v)| {
            RADTValue::Sum { // list
                kind: 1,
                value: Box::new(
                    RADTValue::Product(vec![ // cons
                        RADTValue::Product(vec![k.serialize(deps, group), v.serialize(deps, group)]), // entry
                        acc,
                    ])
                ),
            }
        })
    }
}


pub trait Labeled {
    fn label() -> LabelSet;
}

impl Bridged for String {
    fn group_ids() -> HashSet<TypeId> {
        let mut h = HashSet::new();
        h.insert(TypeId::of::<Self>());
        h
    }
    fn radt() -> (RADT, TypeRef) {
        let t = RADT {
            uniqueness: b"core:utf8-------".to_owned(),
            items: vec![
                RADTItem::ExternalType(BLOB_TYPE_REF),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: t.hash(),
        };
        (t, TypeRef { definition: typing.hash(), item: 0 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        let bytes = Blob {bytes: self.clone().into_bytes()};
        let (rad, t) = Self::radt();
        let val = RADTValue::Hash(bytes.hash());
        debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok());
        let typed = TypedValue { kind: t, value: val };
        (typed, vec![
             Item::Blob(bytes),
        ])
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        let (rad, t) = Self::radt();
        if t != v.kind {
            return Err(MonsterError::BridgedMistypedDependency)
        }
        validate_radt_instance(&rad, t.item, &v.value)?;
        String::deserialize(&v.value, deps)
    }
}

impl DeserializeFromRADTValue for String {
    fn deserialize(val: &RADTValue, deps: &HashMap<Hash,Item>) -> Result<Self, MonsterError> {
        println!("deser string");
        let (_,t_string) = String::radt();
        let body = sure!(val, RADTValue::Hash(h) => h; return Err(MonsterError::BridgedMistypedDependency));
        let bytes = match deps.get(&body) {
            Some(Item::Value(TypedValue{kind, value})) if *kind == t_string => return String::deserialize(value, deps),
            Some(Item::Blob(b)) => b.bytes.clone(),
            None => {
                dbg!("ah", &body, deps.keys().collect::<Vec<_>>());
                return Err(MonsterError::BridgedMissingDependency("string bytes"));
            },
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        String::from_utf8(bytes).map_err(|e| MonsterError::BridgedMistypedDependency)
    }
}

impl FetchStrategy for String {
    fn hydrate(self_hash: Hash, db: &Db) -> Result<Self, MonsterError> {
        let s = db.get(self_hash)?;
        match s {
            Item::Value(val) => {
                let (_,t_string) = String::radt();
                if val.kind != t_string {
                    return Err(MonsterError::Todo("Attempt to String::hydrate a non-string value"));
                }
                let body_hash = sure!(val.value, RADTValue::Hash(h) => h);
                let body = db.get(body_hash)?;
                let bytes = sure!(body, Item::Blob(Blob{bytes}) => bytes);
                Ok(String::from_utf8(bytes).map_err(|e|MonsterError::Todo("Invalid utf8 in string blob"))?)
            },
            _ => Err(MonsterError::Todo("Attempt to String::hydrate a non-value item"))
        }
    }
}
impl FetchStrategy for usize {
    fn hydrate(self_hash: Hash, db: &Db) -> Result<Self, MonsterError> {
        use std::convert::TryInto;
        let s = db.get(self_hash)?;
        match s {
            Item::Value(val) => {
                let (_,t_usize) = usize::radt();
                if val.kind != t_usize {
                    return Err(MonsterError::Todo("Attempt to usize::hydrate a non-uszie value"));
                }
                let body_hash = sure!(val.value, RADTValue::Hash(h) => h);
                let body = db.get(body_hash)?;
                let bytes = sure!(body, Item::Blob(Blob{bytes}) => bytes);
                let ary = bytes[..].try_into().map_err(|e|MonsterError::Todo("wrong number of bytes for a usize body"))?;
                Ok(usize::from_be_bytes(ary))
            },
            _ => Err(MonsterError::Todo("Attempt to usize::hydrate a non-value item")),
        }
    }
}


impl Bridged for RADT {
    fn group_ids() -> HashSet<TypeId> {
        todo!()
    }
    fn radt() -> (RADT, TypeRef) {
        let (_, typeref) = TypeRef::radt();
        let (_, u_size) = usize::radt();
        let r = RADT {
            uniqueness: b"core:radt-------".to_owned(),
            items: vec![
                // 0: nil
                RADTItem::Product(Vec::new()),
                // 1: ExternalType
                RADTItem::ExternalType(typeref),
                // 2: Sum
                RADTItem::CycleRef(7),
                // 3: Product
                RADTItem::CycleRef(7),
                // 4: CycleRef
                RADTItem::ExternalType(u_size),
                // 5: RADTItem
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(1),
                    RADTItem::CycleRef(2),
                    RADTItem::CycleRef(3),
                    RADTItem::CycleRef(4),
                ]),
                // 6: Cons RADTItem
                RADTItem::Product(vec![
                    RADTItem::CycleRef(5),
                    RADTItem::CycleRef(7),
                ]),
                // 7: List RADTItem
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(0),
                    RADTItem::CycleRef(6),
                ]),
                // 8: RADT
                RADTItem::Product(vec![
                    RADTItem::ExternalType(BLOB_TYPE_REF),
                    RADTItem::CycleRef(7),
                ]),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: r.hash(),
        };
        (r, TypeRef { definition: typing.hash(), item: 8 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        fn radt_item_to_value(item: &RADTItem, deps: &mut Vec<Item>) -> RADTValue {
            match item {
                RADTItem::ExternalType(other_type) => {
                    let (other_val, mut ref_deps) = other_type.to_value();
                    deps.append(&mut ref_deps);
                    let other_typing = Item::Value(other_val);
                    let other_hash = other_typing.hash();
                    deps.push(other_typing);
                    RADTValue::Sum { kind: 0, value: Box::new(RADTValue::Hash(other_hash)) }
                },
                RADTItem::Sum(variants) => {
                    RADTValue::Sum {
                        kind: 1,
                        value: Box::new(translate_vec_to_value_list(variants.iter(), |item| radt_item_to_value(item, deps))),
                    }
                },
                RADTItem::Product(fields) => {
                    RADTValue::Sum {
                        kind: 2,
                        value: Box::new(translate_vec_to_value_list(fields.iter(), |item| radt_item_to_value(item, deps))),
                    }
                },
                RADTItem::CycleRef(item) => {
                    let (u_size, mut num_deps) = item.to_value();
                    deps.append(&mut num_deps);
                    let item_typing = Item::Value(u_size);
                    let item_hash = item_typing.hash();
                    deps.push(item_typing);
                    RADTValue::Sum {
                        kind: 3,
                        value: Box::new(RADTValue::Hash(item_hash)),
                    }
                },
            }
        }

        let mut deps = Vec::new();

        let uniq_bytes = self.uniqueness.to_vec();
        let uniq_blob = Blob{bytes: uniq_bytes};
        let uniq_hash = uniq_blob.hash();
        deps.push(Item::Blob(uniq_blob));

        let val = RADTValue::Product(vec![
                    RADTValue::Hash(uniq_hash),
                    translate_vec_to_value_list(self.items.iter(), |item| radt_item_to_value(item, &mut deps)),
        ]);
        let (rad, typeref) = Self::radt();
        debug_assert!(validate_radt_instance(&rad, typeref.item, &val).is_ok(), "{:?}", validate_radt_instance(&rad, typeref.item, &val));
        let typed = TypedValue { kind: typeref, value: val };
        (typed, deps)
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        let (rad, t) = Self::radt();
        if v.kind != t {
            return Err(MonsterError::BridgedMistypedDependency);
        }
        validate_radt_instance(&rad, t.item, &v.value)?;
        let (uniq_hash, items) = sure!(&v.value, RADTValue::Product(fields) => {
            sure!(fields[0], RADTValue::Hash(ref h) => (h, &fields[1]))
        });

        let uniqueness = match deps.get(uniq_hash) {
            Some(Item::Blob(Blob{bytes})) => {
                if bytes.len() == 16 {
                    let mut uniq = [0;16];
                    uniq.copy_from_slice(bytes);
                    uniq
                } else {
                    return Err(MonsterError::BridgedMistypedDependency);
                }
            },
            Some(_) => return Err(MonsterError::BridgedMistypedDependency),
            None => return Err(MonsterError::BridgedMissingDependency("radt uniqueness bytes")),
        };

        fn do_item(item: &RADTValue, deps: &HashMap<Hash, Item>) -> Result<RADTItem, MonsterError> {
            match item {
                // ExternalType
                RADTValue::Sum{kind: 0, value} => {
                    let h = sure!(value.deref(), RADTValue::Hash(h) => h);
                    match deps.get(h) {
                        Some(Item::Value(tr)) => Ok(RADTItem::ExternalType(TypeRef::from_value(tr, deps)?)),
                        Some(_) => Err(MonsterError::BridgedMistypedDependency),
                        None => Err(MonsterError::BridgedMissingDependency("a radt's ExternalType's TypeRef")),
                    }
                },
                // Sum
                RADTValue::Sum{kind: 1, value} => {
                    Ok(RADTItem::Sum(translate_value_list_to_vec(value.deref(), |item| do_item(item, deps))?))
                },
                // Product
                RADTValue::Sum{kind: 2, value} => {
                    Ok(RADTItem::Product(translate_value_list_to_vec(value.deref(), |item| do_item(item, deps))?))
                },
                // CycleRef
                RADTValue::Sum{kind: 3, value} => {
                    let h = sure!(value.deref(), RADTValue::Hash(h) => h);
                    match deps.get(h) {
                        Some(Item::Value(tr)) => Ok(RADTItem::CycleRef(usize::from_value(tr, deps)?)),
                        Some(_) => Err(MonsterError::BridgedMistypedDependency),
                        None => Err(MonsterError::BridgedMissingDependency("a radt's CycleRef's item number")),
                    }
                },
                _ => panic!("It validated, this shouldn't happen")
            }
        }

        let r_items = translate_value_list_to_vec(items, |item| do_item(item, deps))?;

        Ok(RADT { uniqueness, items: r_items }) 
    }
}

// If an iterator has an arbitrary order, (like HashMap entries),
// it might as well be double-ended.
struct ArbitraryOrderDoubleEndedIter<I, T: Iterator<Item=I>>(T);
impl<I, T: Iterator<Item=I>> Iterator for ArbitraryOrderDoubleEndedIter<I, T> {
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
impl<I, T: Iterator<Item=I>> DoubleEndedIterator for ArbitraryOrderDoubleEndedIter<I, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}


fn translate_vec_to_value_list<T>(items: impl DoubleEndedIterator<Item=T>, mut f: impl FnMut(T) -> RADTValue) -> RADTValue {
    items.rfold(RADTValue::Sum { kind: 0, value: Box::new(RADTValue::Product(Vec::new())) }, |acc, item| {
        RADTValue::Sum {
            kind: 1,
            value: Box::new(
                RADTValue::Product(vec![
                   f(item),
                   acc
                ])
            ),
        }
    })
}
use std::ops::Deref;

fn translate_value_list_to_vec<T>(mut v: &RADTValue, mut f: impl FnMut(&RADTValue) -> Result<T, MonsterError>) -> Result<Vec<T>, MonsterError> {
    let mut result = Vec::new();
    while let RADTValue::Sum { kind: 1, value } = v {
        let fields = sure!(value.deref(), RADTValue::Product(f) => f; return Err(MonsterError::BridgedMistypedDependency));
        if fields.len() != 2 {
            return Err(MonsterError::BridgedMistypedDependency);
        }
        result.push(f(&fields[0])?);
        v = &fields[1];
    }

    let value = sure!(v, RADTValue::Sum{kind:0,value:v} => v; return Err(MonsterError::BridgedMistypedDependency));
    let empty = sure!(value.deref(), RADTValue::Product(v) => v; return Err(MonsterError::BridgedMistypedDependency));
    if !empty.is_empty() {
        return Err(MonsterError::BridgedMistypedDependency);
    }

    Ok(result)
}


impl Bridged for usize {
    fn group_ids() -> HashSet<TypeId> {
        let mut h = HashSet::new();
        h.insert(TypeId::of::<Self>());
        h
    }
    fn radt() -> (RADT, TypeRef) {
        let t = RADT {
            uniqueness: b"core:usize------".to_owned(),
            items: vec![
                RADTItem::ExternalType(BLOB_TYPE_REF),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: t.hash(),
        };
        (t, TypeRef { definition: typing.hash(), item: 0 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        let bytes = self.to_be_bytes();
        let blob = Blob {bytes: bytes.to_vec()};
        let (rad, t) = Self::radt();
        let val = RADTValue::Hash(blob.hash());
        debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok());
        let typed = TypedValue { kind: t, value: val };
        (typed, vec![
             Item::Blob(blob),
        ])
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        let (rad, t) = Self::radt();
        if t != v.kind {
            return Err(MonsterError::BridgedMistypedDependency)
        }
        validate_radt_instance(&rad, t.item, &v.value)?;
        usize::deserialize(&v.value, deps)
    }
}




// impl Bridged for Env {
//     fn group_ids() -> HashSet<TypeId> {
//         todo!()
//     }
//     fn radt() -> (RADT, TypeRef) {
//         let (_,radt) = RADT::radt();
//         let (_,labeling) = LabelSet::radt();
//         let (_,utf8) = String::radt();
//         let r = RADT {
//             uniqueness: b"core:Env--------".to_owned(),
//             items: vec![
//                 // 0: nil
//                 RADTItem::Product(Vec::new()),
//                 // 1: labeling entry
//                 RADTItem::Product(vec![
//                     RADTItem::ExternalType(radt),
//                     RADTItem::ExternalType(labeling),
//                 ]),
//                 // 2: labelings cons
//                 RADTItem::Product(vec![
//                     RADTItem::CycleRef(1),
//                     RADTItem::CycleRef(3),
//                 ]),
//                 // 3: labelings list
//                 RADTItem::Sum(vec![
//                     RADTItem::CycleRef(0),
//                     RADTItem::CycleRef(2),
//                 ]),
//                 // 4: variables entry
//                 RADTItem::Product(vec![
//                     RADTItem::ExternalType(utf8),
//                     RADTItem::ExternalType(ANY_TYPE_REF),
//                 ]),
//                 // 5: variables cons
//                 RADTItem::Product(vec![
//                     RADTItem::CycleRef(4),
//                     RADTItem::CycleRef(6),
//                 ]),
//                 // 6: variables list
//                 RADTItem::Sum(vec![
//                     RADTItem::CycleRef(0),
//                     RADTItem::CycleRef(5),
//                 ]),
//                 // 7: Env
//                 RADTItem::Product(vec![
//                     RADTItem::CycleRef(3),
//                     RADTItem::CycleRef(6),
//                 ]),
//             ],
//         };
// 
//         let typing = Typing {
//             kind: RADT_TYPE_REF,
//             data: r.hash(),
//         };
//         (r, TypeRef { definition: typing.hash(), item: 7 })
//     }
//     fn to_value(&self) -> (TypedValue, Vec<Item>) {
//         let (rad,t) = Self::radt();
//         let mut deps = Vec::new();
//         let labels = translate_vec_to_value_list(ArbitraryOrderDoubleEndedIter(self.labelings.iter()), |(type_hash, labels)| {
//             let (label_typed, mut label_deps) = labels.to_value();
//             let label_item = Item::Value(label_typed);
//             let label_hash = label_item.hash();
//             deps.append(&mut label_deps);
//             deps.push(label_item);
//             RADTValue::Product(vec![
//                 RADTValue::Hash(*type_hash),
//                 RADTValue::Hash(label_hash),
//             ])
//         });
//         let vars = translate_vec_to_value_list(ArbitraryOrderDoubleEndedIter(self.variables.iter()), |(name, value_hash)| {
//             let (name_typed, mut name_deps) = name.to_value();
//             let name_item = Item::Value(name_typed);
//             let name_hash = name_item.hash();
//             deps.append(&mut name_deps);
//             deps.push(name_item);
//             RADTValue::Product(vec![
//                 RADTValue::Hash(name_hash),
//                 RADTValue::Hash(*value_hash),
//             ])
//         });
//         let val = RADTValue::Product(vec![
//             labels, vars
//         ]);
//         debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok(), "Env should serialize to RADTValue properly: {}", validate_radt_instance(&rad, t.item, &val).unwrap_err());
//         (TypedValue { kind: t, value: val}, deps)
//     }
//     fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
//         let (rad, t) = Self::radt();
//         // FIXME: since we call from_value recursively, this will get called at every node, giving
//         // us n^2 redundant work. Figure out how to call only once at the top level, or to spread
//         // the checking across the whole recursive process.
//         validate_radt_instance(&rad, t.item, &v.value)?;
//         let v = sure!(&v.value, RADTValue::Product(v) => v);
//         assert!(v.len() == 2);
// 
//         let (_, label_type) = LabelSet::radt();
//         let (_, string_type) = String::radt();
// 
//         let labelings = translate_value_list_to_vec(&v[0], |labeling| {
//             let pair = sure!(labeling, RADTValue::Product(v) => v);
//             let type_hash = sure!(&pair[0], RADTValue::Hash(h) => *h);
//             let label_hash = sure!(&pair[1], RADTValue::Hash(h) => h);
//             let label_typed = match deps.get(label_hash) {
//                 Some(Item::Value(tv)) => tv,
//                 Some(_) => return Err(MonsterError::BridgedMistypedDependency),
//                 None => return Err(MonsterError::BridgedMissingDependency("a labelset in an Env")),
//             };
//             if label_typed.kind != label_type {
//                 return Err(MonsterError::BridgedMistypedDependency);
//             }
//             let label = LabelSet::from_value(&label_typed, deps)?;
//             Ok((type_hash, label))
//         })?.into_iter().collect();
//         let variables = translate_value_list_to_vec(&v[1], |var_def| {
//             let pair = sure!(var_def, RADTValue::Product(v) => v);
//             let name_hash = sure!(&pair[0], RADTValue::Hash(h) => h);
//             let value_hash = sure!(&pair[1], RADTValue::Hash(h) => *h);
//             let name_typed = match deps.get(name_hash) {
//                 Some(Item::Value(tv)) => tv,
//                 Some(_) => return Err(MonsterError::BridgedMistypedDependency),
//                 None => return Err(MonsterError::BridgedMissingDependency("the name string for an Env variable")),
//             };
//             if name_typed.kind != string_type {
//                 return Err(MonsterError::BridgedMistypedDependency);
//             }
//             let name = String::from_value(&name_typed, deps)?;
//             Ok((name, value_hash))
//         })?.into_iter().collect();
// 
//         Ok(Env { labelings, variables })
//     }
// }

use crate::eval::*;

impl Labeled for LabelSet {
    fn label() -> LabelSet {
        // 0: LabelSet
        // 1: Label
        // 2: LabeledItem
        // 3: nil
        // 4: List<Label>
        // 5: Cons<Label>
        LabelSet(vec![
            Label {
                name: "Nil".to_owned(),
                item: LabeledItem::Product(Vec::new()),
            },
            Label {
                name: "Label".to_owned(),
                item: LabeledItem::Product(vec![
                    Label { name: "name".to_owned(), item: LabeledItem::Type },
                    Label { name: "item".to_owned(), item: LabeledItem::Type },
                ]),
            },
            Label {
                name: "LabelCons".to_owned(),
                item: LabeledItem::Product(vec![
                    Label { name: "head".to_owned(), item: LabeledItem::Type },
                    Label { name: "tail".to_owned(), item: LabeledItem::Type },
                ]),
            },
            Label {
                name: "LabelList".to_owned(),
                item: LabeledItem::Sum(vec![
                    Label { name: "nil".to_owned(), item: LabeledItem::Type },
                    Label { name: "cons".to_owned(), item: LabeledItem::Type },
                ]),
            },
            Label {
                name: "LabeledItem::Product".to_owned(),
                item: LabeledItem::Type,
            },
            Label {
                name: "LabeledItem::Sum".to_owned(),
                item: LabeledItem::Type,
            },
            Label {
                name: "LabeledItem".to_owned(),
                item: LabeledItem::Sum(vec![
                    Label { name: "product".to_owned(), item: LabeledItem::Type },
                    Label { name: "sum".to_owned(), item: LabeledItem::Type },
                    Label { name: "type".to_owned(), item: LabeledItem::Type },
                ]),
            },
            Label {
                name: "LabelCons".to_owned(),
                item: LabeledItem::Product(vec![
                    Label { name: "head".to_owned(), item: LabeledItem::Type },
                    Label { name: "tail".to_owned(), item: LabeledItem::Type },
                ]),
            },
            Label {
                name: "LabelList".to_owned(),
                item: LabeledItem::Sum(vec![
                    Label { name: "nil".to_owned(), item: LabeledItem::Type },
                    Label { name: "cons".to_owned(), item: LabeledItem::Type },
                ]),
            },
        ])
    }
}

// impl Bridged for Label {
//     fn group_ids() -> HashSet<TypeId> {
//         todo!()
//     }
//     fn radt() -> (RADT, TypeRef) {
//         let (_,utf8) = String::radt();
//         let r = RADT {
//             uniqueness: b"core:LabelSet---".to_owned(),
//             items: vec![
//                 // 0: nil
//                 RADTItem::Product(Vec::new()),
//                 // 1: single label
//                 RADTItem::Product(vec![
//                     // text label
//                     RADTItem::ExternalType(utf8),
//                     // item it's labeling. Either a deeper labeling, or nil when
//                     // the type bottoms out on an ExternalType
//                     RADTItem::CycleRef(6),
//                 ]),
//                 // 2: label list cons
//                 RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(3)]),
//                 // 3: label list
//                 RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(2)]),
//                 // 4: product field labels
//                 RADTItem::CycleRef(3),
//                 // 5: variant names
//                 RADTItem::CycleRef(3),
//                 // 6: Single type labeling - product, sum, or nil if it refers to an instance of another
//                 // type
//                 RADTItem::Sum(vec![
//                     RADTItem::CycleRef(4),
//                     RADTItem::CycleRef(5),
//                     RADTItem::CycleRef(0),
//                 ]),
//                 // 7: item labelings cons
//                 RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(8)]),
//                 // 8: item labels
//                 RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(7)]),
//             ],
//         };
//         let typing = Typing {
//             kind: RADT_TYPE_REF,
//             data: r.hash(),
//         };
//         (r, TypeRef { definition: typing.hash(), item: 1 })
//     }
//     fn to_value(&self) -> (TypedValue, Vec<Item>) {
//         let (rad, t) = Self::radt();
//         let mut deps = Vec::new();
// 
//         let (name, mut name_deps) = self.name.to_value();
//         deps.append(&mut name_deps);
//         let name_val = Item::Value(name);
//         let name_hash = name_val.hash();
//         deps.push(name_val);
//         let item = match &self.item {
//             LabeledItem::Product(field_labels) => {
//                 RADTValue::Sum {
//                     kind: 0,
//                     value: Box::new(
//                         translate_vec_to_value_list(field_labels.iter(), |field_label| {
//                             let (v, mut field_deps) = field_label.to_value();
//                             deps.append(&mut field_deps);
//                             v.value
//                         })
//                     ),
//                 }
//             },
//             LabeledItem::Sum(variant_labels) => {
//                 RADTValue::Sum {
//                     kind: 1,
//                     value: Box::new(
//                         translate_vec_to_value_list(variant_labels.iter(), |variant_label| {
//                             let (v, mut variant_deps) = variant_label.to_value();
//                             deps.append(&mut variant_deps);
//                             v.value
//                         })
//                     ),
//                 }
//             },
//             LabeledItem::Type => {
//                 RADTValue::Sum {
//                     kind: 2,
//                     value: Box::new(RADTValue::Product(Vec::new())),
//                 }
//             },
//         };
// 
//         let val = RADTValue::Product(vec![RADTValue::Hash(name_hash), item]);
// 
//         debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok(), "Labels should serialize to values properly: {}", validate_radt_instance(&rad, t.item, &val).unwrap_err());
//         (TypedValue { kind: t, value: val }, deps)
//     }
//     fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
//         let (rad, t) = Self::radt();
//         // FIXME: since we call from_value recursively, this will get called at every node, giving
//         // us n^2 redundant work. Figure out how to call only once at the top level, or to spread
//         // the checking across the whole recursive process.
//         validate_radt_instance(&rad, t.item, &v.value)?;
//         let v = sure!(&v.value, RADTValue::Product(v) => v);
//         assert!(v.len() == 2);
// 
//         let name_hash = sure!(&v[0], RADTValue::Hash(h) => h);
//         let name = match deps.get(name_hash) {
//             Some(Item::Value(name_val)) => String::from_value(name_val, deps)?,
//             Some(_) => return Err(MonsterError::BridgedMistypedDependency),
//             None => return Err(MonsterError::BridgedMissingDependency("the name string for a label")),
//         };
// 
//         let item = match &v[1] {
//             RADTValue::Sum{kind: 0, value} => {
//                 LabeledItem::Product(
//                     translate_value_list_to_vec(value.deref(), |field_value| {
//                         Label::from_value(&TypedValue{kind: t, value: field_value.clone()}, deps)
//                     })?
//                 )
//             },
//             RADTValue::Sum{kind: 1, value} => {
//                 LabeledItem::Sum(
//                     translate_value_list_to_vec(value.deref(), |variant_value| {
//                         Label::from_value(&TypedValue{kind: t, value: variant_value.clone()}, deps)
//                     })?
//                 )
//             },
//             RADTValue::Sum{kind: 2, value} => {
//                 let v = sure!(value.deref(), RADTValue::Product(v) => v);
//                 assert!(v.is_empty());
//                 LabeledItem::Type
//             },
//             _ => panic!("this shouldn't happen, we already validated against the type"),
//         };
// 
//         Ok(Label { name, item })
//     }
// }

// impl Bridged for LabelSet {
//     fn group_ids() -> HashSet<TypeId> {
//         todo!()
//     }
//     fn radt() -> (RADT, TypeRef) {
//         let (_,utf8) = String::radt();
//         let r = RADT {
//             uniqueness: b"core:LabelSet---".to_owned(),
//             items: vec![
//                 // 0: nil
//                 RADTItem::Product(Vec::new()),
//                 // 1: single label
//                 RADTItem::Product(vec![
//                     // text label
//                     RADTItem::ExternalType(utf8),
//                     // item it's labeling. Either a deeper labeling, or nil when
//                     // the type bottoms out on an ExternalType
//                     RADTItem::CycleRef(6),
//                 ]),
//                 // 2: label list cons
//                 RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(3)]),
//                 // 3: label list
//                 RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(2)]),
//                 // 4: product field labels
//                 RADTItem::CycleRef(3),
//                 // 5: variant names
//                 RADTItem::CycleRef(3),
//                 // 6: Single type labeling - product, sum, or nil if it refers to an instance of another
//                 // type
//                 RADTItem::Sum(vec![
//                     RADTItem::CycleRef(4), // Product
//                     RADTItem::CycleRef(5), // Sum
//                     RADTItem::CycleRef(0), // Type
//                 ]),
//                 // 7: item labelings cons
//                 RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(8)]),
//                 // 8: item labels
//                 RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(7)]),
//             ],
//         };
//         let typing = Typing {
//             kind: RADT_TYPE_REF,
//             data: r.hash(),
//         };
//         (r, TypeRef { definition: typing.hash(), item: 8 })
//     }
//     fn to_value(&self) -> (TypedValue, Vec<Item>) {
//         let (rad, t) = Self::radt();
//         let mut deps = Vec::new();
//         let val = translate_vec_to_value_list(self.0.iter(), |label| {
//             let (label_val, mut label_deps) = label.to_value();
//             deps.append(&mut label_deps);
//             label_val.value
//         });
//         debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok(), "LabelSets should serialize to values properly: {}", validate_radt_instance(&rad, t.item, &val).unwrap_err());
//         (TypedValue { kind: t, value: val }, deps)
//     }
//     fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
//         let (rad, t) = Self::radt();
//         validate_radt_instance(&rad, t.item, &v.value)?;
//         translate_value_list_to_vec(&v.value, |label_value| {
//             Label::from_value(&TypedValue{kind: t, value: label_value.clone()}, deps)
//         }).map(|v| LabelSet(v))
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    fn test_roundtrip<T: Bridged + Eq + std::fmt::Debug>(input: T) {
        let _ = T::radt();
        let (val, mut deps) = input.to_value();
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let result = T::from_value(&val, &env).unwrap();
        assert_eq!(input, result);
    }

    #[test]
    fn test_str_roundtrip() {
        test_roundtrip(String::from("hello"));
    }

    #[test]
    fn test_usize() {
        test_roundtrip(290usize);
    }

    #[test]
    fn test_typerefs() {
        test_roundtrip(TypeRef {
            definition: Hash::of(b"owl"),
            item: 290,
        });
    }

    #[test]
    fn test_radt() {
        test_roundtrip(RADT {
            uniqueness: b"blob list-------".to_owned(),
            items: vec![
                // nil
                RADTItem::Product(Vec::new()),
                // cons
                RADTItem::Product(vec![
                    RADTItem::ExternalType(BLOB_TYPE_REF),
                    RADTItem::CycleRef(2),
                ]),
                // list
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(0),
                    RADTItem::CycleRef(1),
                ]),
            ],
        });
    }

    #[test]
    fn test_label() {
        test_roundtrip(LabelSet(vec![
                 Label {
                    name: "Option".to_owned(),
                    item: LabeledItem::Sum(vec![
                        Label { name: "Some".to_owned(), item: LabeledItem::Type },
                        Label { name: "None".to_owned(), item: LabeledItem::Product(Vec::new()) },
                    ]),
                },
        ]));
    }

    #[test]
    fn test_env() {
        let mut types = HashMap::<Hash, LabelSet>::new();
        types.insert(Hash::of(b"quilt"), LabelSet(vec![
            Label {
                name: "Value".to_owned(),
                item: LabeledItem::Type,
            },
        ]));

        let mut vars = HashMap::<String, Hash>::new();
        vars.insert("bookmarks".to_owned(), Hash::of(b"grog"));

        let e = Env {
            labelings: types,
            variables: vars,
        };

        test_roundtrip(e);
    }

    #[ignore]
    #[test]
    fn test_function_def() {
        use crate::func::*;
        let def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: BTreeMap::from_iter([
                 (String::from("blob"), FunctionReference::Builtin(0)),
                 (String::from("thing"), FunctionReference::Definition(Hash::of(b"owl"))),
            ].iter().cloned()),
            body: String::from("let b = blob(); thing(arg0); b"),
        };

        test_roundtrip(def);
    }
}

// use crate::func::{ };
impl Bridged for crate::func::FunctionDefinition {
    fn group_ids() -> HashSet<TypeId> {
        todo!()
    }
    fn radt() -> (RADT, TypeRef) {
        let (_,t_string) = String::radt();
        let (_,t_usize) = usize::radt();
        let (_,t_radt) = RADT::radt();
        let t = RADT {
            uniqueness: b"core:func-------".to_owned(),
            items: vec![
                // 0: FunctionReference
                RADTItem::Sum(vec![
                    // Builtin(usize)
                    RADTItem::ExternalType(t_usize),
                    // Definition(Hash)
                    RADTItem::CycleRef(4),
                ]),
                // 1: FullType
                RADTItem::Product(vec![
                    // radt
                    RADTItem::ExternalType(t_radt),
                    // item
                    RADTItem::ExternalType(t_usize),
                ]),
                // 2: Kind
                RADTItem::Sum(vec![
                    // Blob
                    RADTItem::Product(Vec::new()),
                    // Typing
                    RADTItem::Product(Vec::new()),
                    // Value(FullType)
                    RADTItem::CycleRef(1),
                    // Function(FunctionSignature)
                    RADTItem::CycleRef(3),
                ]),
                // 3: FunctionSignature
                RADTItem::Product(vec![
                    // inputs
                    RADTItem::CycleRef(5),
                    // out
                    RADTItem::CycleRef(2),
                ]),
                // 4: FunctionDefinition
                RADTItem::Product(vec![
                    // signature
                    RADTItem::CycleRef(3),
                    // dependencies
                    RADTItem::CycleRef(9),
                    // body
                    RADTItem::ExternalType(t_string),
                ]),
                // 5: List of Kind
                RADTItem::Sum(vec![
                    // cons
                    RADTItem::CycleRef(6),
                    // nil
                    RADTItem::CycleRef(7),
                ]),
                // 6: Cons of Kind
                RADTItem::Product(vec![
                    // head
                    RADTItem::CycleRef(2),
                    // tail
                    RADTItem::CycleRef(5),
                ]),
                // 7: Nil
                RADTItem::Product(Vec::new()),
                // 8: String=>FunctionReference entry
                RADTItem::Product(vec![
                    // name binding
                    RADTItem::ExternalType(t_string),
                    // function value
                    RADTItem::CycleRef(5),
                ]),
                // 9: List of String=>FunctionRef entries (Map<String,FunctionRef>)
                RADTItem::Sum(vec![
                    // cons
                    RADTItem::CycleRef(10),
                    // nil
                    RADTItem::CycleRef(7),
                ]),
                // 10: Cons of entry
                RADTItem::Product(vec![
                    // head
                    RADTItem::CycleRef(8),
                    // tail
                    RADTItem::CycleRef(9),
                ]),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: t.hash(),
        };
        (t, TypeRef { definition: typing.hash(), item: 4 })
    }

    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        todo!()
    }

    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        todo!()
    }
}
