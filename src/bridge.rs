// translations between rust structs and db values

use std::collections::HashMap;

use crate::core::*;
use crate::types::*;
use crate::labels::*;
use crate::storage::*;
use crate::error::MonsterError;

pub trait Bridged: Sized {
    fn radt() -> (RADT, TypeRef);
    fn to_value(&self) -> (TypedValue, Vec<Item>);
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError>;
}

impl Bridged for String {
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
        let body = match v.value {
            RADTValue::Hash(h) => h,
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        let bytes = match deps.get(&body) {
            Some(Item::Blob(b)) => b.bytes.clone(),
            None => return Err(MonsterError::BridgedMissingDependency),
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        String::from_utf8(bytes).map_err(|e| MonsterError::BridgedMistypedDependency)
    }
}

impl Bridged for TypeRef {
    fn radt() -> (RADT, TypeRef) {
        let (_, u_size) = usize::radt();
        let t = RADT {
            uniqueness: b"core:TypeRef----".to_owned(),
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::ExternalType(RADT_TYPE_REF),
                    RADTItem::ExternalType(u_size),
                ]),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: t.hash(),
        };
        (t, TypeRef { definition: typing.hash(), item: 0 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        let (item, mut deps) = self.item.to_value();
        let item_hash = typing_from_typed_val(&item).hash();

        let (rad, t) = Self::radt();
        let val = RADTValue::Product(vec![
                    RADTValue::Hash(self.definition),
                    RADTValue::Hash(item_hash),
        ]);

        debug_assert!(validate_radt_instance(&rad, t.item, &val).is_ok());

        let typed = TypedValue { kind: t, value: val };
        deps.push(Item::Value(item));

        (typed, deps)
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        let (rad, t) = Self::radt();
        if t != v.kind {
            return Err(MonsterError::BridgedMistypedDependency)
        }
        validate_radt_instance(&rad, t.item, &v.value)?;
        let fields = match &v.value {
            RADTValue::Product(fields) => fields,
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        let definition = match fields[0] {
            RADTValue::Hash(h) => h,
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        let item_hash = match fields[1] {
            RADTValue::Hash(h) => h,
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        let u_size = match deps.get(&item_hash) {
            Some(Item::Value(u_size)) => u_size,
            None => return Err(MonsterError::BridgedMissingDependency),
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };

        let item = usize::from_value(&u_size, deps)?;

        Ok(TypeRef { definition, item })
    }
}

impl Bridged for RADT {
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
            None => return Err(MonsterError::BridgedMissingDependency),
        };

        fn do_item(item: &RADTValue, deps: &HashMap<Hash, Item>) -> Result<RADTItem, MonsterError> {
            match item {
                // ExternalType
                RADTValue::Sum{kind: 0, value} => {
                    let h = sure!(value.deref(), RADTValue::Hash(h) => h);
                    match deps.get(h) {
                        Some(Item::Value(tr)) => Ok(RADTItem::ExternalType(TypeRef::from_value(tr, deps)?)),
                        Some(_) => Err(MonsterError::BridgedMistypedDependency),
                        None => Err(MonsterError::BridgedMissingDependency),
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
                        None => Err(MonsterError::BridgedMissingDependency),
                    }
                },
                _ => panic!("It validated, this shouldn't happen")
            }
        }

        let r_items = translate_value_list_to_vec(items, |item| do_item(item, deps))?;

        Ok(RADT { uniqueness, items: r_items }) 
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
        match value.deref() {
            RADTValue::Product(fields) => {
                if fields.len() != 2 {
                    return Err(MonsterError::BridgedMistypedDependency);
                }
                result.push(f(&fields[0])?);
                v = &fields[1];
            },
            _ => return Err(MonsterError::BridgedMistypedDependency)
        }
    }

    if let RADTValue::Sum { kind: 0, value } = v {
        match value.deref() {
            RADTValue::Product(v) => {
                if v.len() != 0 {
                    return Err(MonsterError::BridgedMistypedDependency);
                }
            },
            _ => return Err(MonsterError::BridgedMistypedDependency)
        }
    } else {
        return Err(MonsterError::BridgedMistypedDependency);
    }

    Ok(result)
}


impl Bridged for usize {
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
        let body = match v.value {
            RADTValue::Hash(h) => h,
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        let bytes = match deps.get(&body) {
            Some(Item::Blob(b)) => b.bytes.clone(),
            None => return Err(MonsterError::BridgedMissingDependency),
            _ => return Err(MonsterError::BridgedMistypedDependency),
        };
        assert!(bytes.len() == 8, "Only 64-bit numbers can be decoded");
        let mut b = [0;8];
        b.copy_from_slice(&bytes);
        Ok(usize::from_be_bytes(b))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_roundtrip() {
        let s = String::from("hello");
        let (val, mut deps) = s.to_value();
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let s2 = String::from_value(&val, &env).unwrap();
        assert_eq!(s, s2);
    }

    #[test]
    fn test_usize() {
        let n: usize = 290;
        let _ = usize::radt();
        let (val, mut deps) = n.to_value();
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let n2 = usize::from_value(&val, &env).unwrap();
        assert_eq!(n, n2);
    }

    #[test]
    fn test_typerefs() {
        let r = TypeRef {
            definition: Hash::of(b"owl"),
            item: 290,
        };
        let _ = TypeRef::radt();
        let (val, mut deps) = r.to_value();
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let r2 = TypeRef::from_value(&val, &env).unwrap();
        assert_eq!(r, r2);
    }

    #[test]
    fn test_radt() {
        let r = RADT {
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
        };
        let _ = RADT::radt();
        let (val, mut deps) = r.to_value();
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let r2 = RADT::from_value(&val, &env).unwrap();
        assert_eq!(r, r2);
    }

    #[test]
    fn test_label() {
        let l = LabelSet(vec![
                 Label {
                    name: "Option".to_owned(),
                    item: LabeledItem::Sum(vec![
                        Label { name: "Some".to_owned(), item: LabeledItem::Type },
                        Label { name: "None".to_owned(), item: LabeledItem::Product(Vec::new()) },
                    ]),
                },
        ]);

        let _ = LabelSet::radt();
        let (val, mut deps) = l.to_value();
        println!("encoded");
        let env = deps.into_iter().map(|i| (i.hash(), i)).collect();
        let l2 = LabelSet::from_value(&val, &env).unwrap();
        assert_eq!(l, l2);
    }
}

impl Bridged for Label {
    fn radt() -> (RADT, TypeRef) {
        let (_,utf8) = String::radt();
        let r = RADT {
            uniqueness: b"core:LabelSet---".to_owned(),
            items: vec![
                // 0: nil
                RADTItem::Product(Vec::new()),
                // 1: single label
                RADTItem::Product(vec![
                    // text label
                    RADTItem::ExternalType(utf8),
                    // item it's labeling. Either a deeper labeling, or nil when
                    // the type bottoms out on an ExternalType
                    RADTItem::CycleRef(6),
                ]),
                // 2: label list cons
                RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(3)]),
                // 3: label list
                RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(2)]),
                // 4: product field labels
                RADTItem::CycleRef(3),
                // 5: variant names
                RADTItem::CycleRef(3),
                // 6: Single type labeling - product, sum, or nil if it refers to an instance of another
                // type
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(4),
                    RADTItem::CycleRef(5),
                    RADTItem::CycleRef(0),
                ]),
                // 7: item labelings cons
                RADTItem::Product(vec![RADTItem::CycleRef(6), RADTItem::CycleRef(8)]),
                // 8: item labels
                RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(7)]),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: r.hash(),
        };
        (r, TypeRef { definition: typing.hash(), item: 8 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        let (_, t) = Self::radt();
        let mut deps = Vec::new();

        let (name, mut name_deps) = self.name.to_value();
        deps.append(&mut name_deps);
        let name_val = Item::Value(name);
        let name_hash = name_val.hash();
        deps.push(name_val);
        let item = match &self.item {
            LabeledItem::Product(field_labels) => {
                RADTValue::Sum {
                    kind: 0,
                    value: Box::new(
                        translate_vec_to_value_list(field_labels.iter(), |field_label| {
                            let (v, mut field_deps) = field_label.to_value();
                            deps.append(&mut field_deps);
                            v.value
                        })
                    ),
                }
            },
            LabeledItem::Sum(variant_labels) => {
                RADTValue::Sum {
                    kind: 1,
                    value: Box::new(
                        translate_vec_to_value_list(variant_labels.iter(), |variant_label| {
                            let (v, mut variant_deps) = variant_label.to_value();
                            deps.append(&mut variant_deps);
                            v.value
                        })
                    ),
                }
            },
            LabeledItem::Type => {
                RADTValue::Sum {
                    kind: 2,
                    value: Box::new(RADTValue::Product(Vec::new())),
                }
            },
        };

        let val = RADTValue::Product(vec![RADTValue::Hash(name_hash), item]);
        (TypedValue { kind: t, value: val }, deps)
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        let (rad, t) = Self::radt();
        // FIXME: since we call from_value recursively, this will get called at every node, giving
        // us n^2 redundant work. Figure out how to call only once at the top level, or to spread
        // the checking across the whole recursive process.
        validate_radt_instance(&rad, t.item, &v.value)?;
        let v = sure!(&v.value, RADTValue::Product(v) => v);
        assert!(v.len() == 2);

        let name_hash = sure!(&v[0], RADTValue::Hash(h) => h);
        let name = match deps.get(name_hash) {
            Some(Item::Value(name_val)) => String::from_value(name_val, deps)?,
            Some(_) => return Err(MonsterError::BridgedMistypedDependency),
            None => return Err(MonsterError::BridgedMissingDependency),
        };

        let item = match &v[1] {
            RADTValue::Sum{kind: 0, value} => {
                todo!();
            },
            RADTValue::Sum{kind: 1, value} => {todo!()},
            RADTValue::Sum{kind: 2, value} => {
                let v = sure!(value.deref(), RADTValue::Product(v) => v);
                assert!(v.is_empty());
                LabeledItem::Type
            },
            _ => panic!("this shouldn't happen, we already validated against the type"),
        };

        Ok(Label { name, item })
    }
}
// fn translate_vec_to_value_list<T>(items: impl DoubleEndedIterator<Item=T>, mut f: impl FnMut(T) -> RADTValue)
// -> RADTValue {
// fn translate_value_list_to_vec<T>(mut v: &RADTValue, mut f: impl FnMut(&RADTValue) -> Result<T, MonsterError>)
// -> Result<Vec<T>, MonsterError> {

impl Bridged for LabelSet {
    fn radt() -> (RADT, TypeRef) {
        let (_,utf8) = String::radt();
        let r = RADT {
            uniqueness: b"core:LabelSet---".to_owned(),
            items: vec![
                // 0: nil
                RADTItem::Product(Vec::new()),
                // 1: single label
                RADTItem::Product(vec![
                    // text label
                    RADTItem::ExternalType(utf8),
                    // item it's labeling. Either a deeper labeling, or nil when
                    // the type bottoms out on an ExternalType
                    RADTItem::CycleRef(6),
                ]),
                // 2: label list cons
                RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(3)]),
                // 3: label list
                RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(2)]),
                // 4: product field labels
                RADTItem::CycleRef(3),
                // 5: variant names
                RADTItem::CycleRef(3),
                // 6: Single type labeling - product, sum, or nil if it refers to an instance of another
                // type
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(4),
                    RADTItem::CycleRef(5),
                    RADTItem::CycleRef(0),
                ]),
                // 7: item labelings cons
                RADTItem::Product(vec![RADTItem::CycleRef(6), RADTItem::CycleRef(8)]),
                // 8: item labels
                RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(7)]),
            ],
        };
        let typing = Typing {
            kind: RADT_TYPE_REF,
            data: r.hash(),
        };
        (r, TypeRef { definition: typing.hash(), item: 1 })
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        let (_, t) = Self::radt();
        let mut deps = Vec::new();
        let val = translate_vec_to_value_list(self.0.iter(), |label| {
            let (label_val, mut label_deps) = label.to_value();
            deps.append(&mut label_deps);
            label_val.value
        });
        (TypedValue { kind: t, value: val }, deps)
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        todo!();
    }
}

// fn translate_vec_to_value_list<T>(items: impl DoubleEndedIterator<Item=T>, mut f: impl FnMut(T) -> RADTValue)
// -> RADTValue {
// fn translate_value_list_to_vec<T>(mut v: &RADTValue, mut f: impl FnMut(&RADTValue) -> Result<T, MonsterError>)
// -> Result<Vec<T>, MonsterError> {
