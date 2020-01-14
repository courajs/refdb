// translations between rust structs and db values

use std::collections::HashMap;

use crate::core::*;
use crate::types::*;
use crate::storage::*;
use crate::error::MonsterError;

pub trait Bridged: Sized {
    fn radt() -> (RADT, TypeRef);
    fn encode(&self) -> (TypedValue, Vec<Item>);
    fn decode(v: &TypedValue, deps: HashMap<Hash, Item>) -> Result<Self, MonsterError>;
}

impl Bridged for String {
    fn radt() -> (RADT, TypeRef) {
        let t = RADT {
            uniqueness: b"utf8 string-----".to_owned(),
            items: vec![
                RADTItem::ExternalType(TypeRef {
                    definition: BLOB_TYPE_HASH,
                    item: 0,
                }),
            ],
        };
        let h = t.hash();
        (t, TypeRef { definition: h, item: 0 })
    }
    fn encode(&self) -> (TypedValue, Vec<Item>) {
        let bytes = Blob {bytes: self.clone().into_bytes()};
        let (rad, t) = Self::radt();
        let val = RADTValue::Hash(bytes.hash());
        let thing = validate_radt_instance(&rad, t.item, &val).expect("this should match");
        let typed = TypedValue { kind: t, value: val };
        (typed, vec![
             Item::Blob(bytes),
        ])
    }
    fn decode(v: &TypedValue, deps: HashMap<Hash, Item>) -> Result<Self, MonsterError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_roundtrip() {
        let s = String::from("hello");
        let t = String::radt();
        let (val, deps) = s.encode();
        // let env = deps.map(
    }
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

    fn encode(&self) -> Typing {
        todo!()
    }
    fn decode(v: &RADTValue) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode() {
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
