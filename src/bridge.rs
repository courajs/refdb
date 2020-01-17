// translations between rust structs and db values

use std::collections::HashMap;

use crate::core::*;
use crate::types::*;
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

impl Bridged for RADT {
    fn radt() -> (RADT, TypeRef) {
        todo!();
    }
    fn to_value(&self) -> (TypedValue, Vec<Item>) {
        todo!();
    }
    fn from_value(v: &TypedValue, deps: &HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        todo!();
    }
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_roundtrip() {
        let s = String::from("hello");
        // let t = String::radt();
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
        // let (val, mut deps) = n.to_value();
        // let env
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

    // #[test]
    // fn test_radt_roundtrip() {
    //     // dbg!(RADT::radt());
    //     todo!();
    // }
}


/*

impl Bridged for LabelSet {
    fn type() -> RADT {
        RADT {
            uniqueness: b"label-set       ".to_owned(),
            items: vec![
                // 0: nil
                RADTItem::Product(Vec::new()),
                // 1: single label
                RADTItem::Product(vec![
                    // text label
                    RADTItem::ExternalType(TypeRef {
                        definition: utf8hash,
                        item: 0,
                    }),
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
        }
    }

    fn to_value(&self) -> Typing {
        todo!()
    }
    fn from_value(v: &RADTValue) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_value() {
        let l = LabelSet(vec![
            Label {
                name: "Option".to_owned(),
                item: LabeledItem::Sum(vec![
                            Label { name: "Some".to_owned(), item: LabeledItem::Type },
                            Label { name: "None".to_owned(), item: LabeledItem::Product(Vec::new()) },
                ]),
            },
            Label { name: "That".to_owned(), item: LabeledItem::Type },
        ]);

        let expected = 
    }
}
*/
