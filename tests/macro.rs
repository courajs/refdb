#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::collections::BTreeMap;
use std::collections::HashMap;

use rf0::types::*;
use rf0::core::*;
use rf0::storage::Storable;
use rf0::storage::Item;
use rf0::bridge::*;
use bridged_group::*;

mod t1 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing;
    }

    #[test]
    fn hello_bridged() {
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(Vec::new()),
            ],
        };
        let t = r.item_ref(0);
        assert_eq!(<Thing as Bridged>::radt(), (r, t));
    }
}

mod t2 {
    use super::*;
    
    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing;
        struct Other;
    }

    #[test]
    fn multiple_items() {
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(Vec::new()),
                RADTItem::Product(Vec::new()),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
        assert_eq!(<Other as Bridged>::radt(), (r.clone(), r.item_ref(1)));
    }
}

mod t3 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing {
            x: usize,
        }
    }

    #[test]
    fn external_ref() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
    }
}

mod t4 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing {
            x: Other,
        }
        struct Other;
    }

    #[test]
    fn cycle_ref() {
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::CycleRef(1),
                ]),
                RADTItem::Product(Vec::new()),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
        assert_eq!(<Other as Bridged>::radt(), (r.clone(), r.item_ref(1)));
    }
}

mod t5 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(usize);
    }

    #[test]
    fn tuple_struct() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
    }
}

mod t6 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        enum Thing {
            A {
                x: Other,
                y: usize,
            },
            B(Other),
            C(usize),
            D,
        }
        struct Other;
    }

    #[test]
    fn hello_enum() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Sum(vec![
                    RADTItem::Product(vec![
                        RADTItem::CycleRef(1),
                        RADTItem::ExternalType(t_usize),
                    ]),
                    RADTItem::Product(vec![
                        RADTItem::CycleRef(1),
                    ]),
                    RADTItem::Product(vec![
                        RADTItem::ExternalType(t_usize),
                    ]),
                    RADTItem::Product(Vec::new()),
                ]),
                RADTItem::Product(Vec::new()),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
        assert_eq!(<Other as Bridged>::radt(), (r.clone(), r.item_ref(1)));
    }
}

mod t7 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(Box<usize>);
    }

    #[test]
    fn handle_box() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
    }
}

mod t8 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(Vec<usize>);
    }

    #[test]
    fn handle_vec() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                RADTItem::Product(vec![
                    RADTItem::CycleRef(2),
                ]),
                RADTItem::Product(Vec::new()),
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(1),
                    RADTItem::CycleRef(3),
                ]),
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                    RADTItem::CycleRef(2),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
    }
}

mod t9 {
    use super::*;
    use std::collections::BTreeMap;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(BTreeMap<usize, usize>);
        struct Other(Vec<usize>);
    }

    #[test]
    fn handle_map() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                // 0: thing
                RADTItem::Product(vec![
                    RADTItem::CycleRef(5),
                ]),
                // 1: other
                RADTItem::Product(vec![
                    RADTItem::CycleRef(3),
                ]),
                // 2: nil
                RADTItem::Product(Vec::new()),
                // 3: list usize
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(2),
                    RADTItem::CycleRef(4),
                ]),
                // 4: cons usize
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                    RADTItem::CycleRef(3),
                ]),
                // 5: list map entry
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(2),
                    RADTItem::CycleRef(6),
                ]),
                // 6: cons map entry
                RADTItem::Product(vec![
                    RADTItem::CycleRef(7),
                    RADTItem::CycleRef(5),
                ]),
                // 7: map entry
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                    RADTItem::ExternalType(t_usize),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
        assert_eq!(<Other as Bridged>::radt(), (r.clone(), r.item_ref(1)));
    }
}

mod t10 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(Hash!(usize::radt().1));
        struct Other(std::collections::BTreeMap<usize, Hash!(usize::radt().1)>);
    }

    #[test]
    fn handle_hash() {
        let (_,t_usize) = usize::radt();
        let r = RADT {
            uniqueness: *b"1234567812345678",
            items: vec![
                // Thing
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                ]),
                // Other
                RADTItem::Product(vec![
                    RADTItem::CycleRef(3),
                ]),
                // nil
                RADTItem::Product(Vec::new()),
                // map
                RADTItem::Sum(vec![
                    RADTItem::CycleRef(2),
                    RADTItem::CycleRef(4),
                ]),
                // cons
                RADTItem::Product(vec![
                    RADTItem::CycleRef(5),
                    RADTItem::CycleRef(3),
                ]),
                // entry
                RADTItem::Product(vec![
                    RADTItem::ExternalType(t_usize),
                    RADTItem::ExternalType(t_usize),
                ]),
            ],
        };
        assert_eq!(<Thing as Bridged>::radt(), (r.clone(), r.item_ref(0)));
        assert_eq!(<Other as Bridged>::radt(), (r.clone(), r.item_ref(1)));
    }
}

mod t11 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing(usize);
    }

    #[test]
    fn basic_to_value() {
        let (_,tr) = Thing::radt();
        let (num, mut deps) = (12usize).to_value();
        let h = num.typing().hash();
        deps.push(Item::Value(num));
        let val = TypedValue {
            kind: tr,
            value: RADTValue::Product(vec![
                      RADTValue::Hash(h),
            ]),
        };
        let (v, out_deps) = Thing(12).to_value();
        assert_eq!(out_deps, deps);
        assert_eq!(v, val);
    }
}

mod t12 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        enum Thing {
            A,
            B(usize),
            C(BTreeMap<Other, usize>),
            D { x: usize },
        }
        #[derive(PartialEq, Eq, PartialOrd, Ord)]
        struct Other;
    }

    #[test]
    fn struct_variant_to_value() {
        let (r, tr) = Thing::radt();
        let (_, t_usize) = usize::radt();

        let input = Thing::D { x: 12 };

        let (num, mut deps) = (12usize).to_value();
        let num_hash = num.typing().hash();
        deps.push(Item::Value(num));

        let val = TypedValue {
            kind: tr,
            value: RADTValue::Sum {
                kind: 3,
                value: Box::new(
                    RADTValue::Product(vec![
                        RADTValue::Hash(num_hash),
                    ])
                ),
            },
        };

        assert_eq!(input.to_value(), (val, deps));
    }

    #[test]
    fn unit_variant_to_value() {
        let (r, tr) = Thing::radt();
        let val = TypedValue {
            kind: tr,
            value: RADTValue::Sum {
                kind: 0,
                value: Box::new(RADTValue::Product(Vec::new())),
            },
        };
        assert_eq!(Thing::A.to_value(), (val, Vec::new()));
    }

    #[test]
    fn tuple_variant_to_value() {
        let (r,tr) = Thing::radt();
        let other = r.item_ref(1);
        let (num, mut deps) = (12usize).to_value();
        let num_hash = num.typing().hash();
        deps.push(Item::Value(num));

        let mut map = BTreeMap::new();
        map.insert(Other, 12);
        let input = Thing::C(map);

        let val = TypedValue {
            kind: tr,
            value: RADTValue::Sum { // variant discrim
                kind: 2,
                value: Box::new(
                    RADTValue::Product(vec![ // contents of the variant
                        RADTValue::Sum { // map
                            kind: 1,
                            value: Box::new(
                                RADTValue::Product(vec![ // cons
                                    RADTValue::Product(vec![ // head is an entry
                                        RADTValue::Product(Vec::new()), // Other
                                        RADTValue::Hash(num_hash), // usize
                                    ]),
                                    RADTValue::Sum { // tail is empty list
                                        kind: 0,
                                        value: Box::new(RADTValue::Product(Vec::new())),
                                    },
                                ])
                            )
                        }
                    ])
                )
            },
        };
        let (v, out_deps) = input.to_value();
        assert_eq!(out_deps, deps);
        assert_eq!(v, val);
    }

    bridged_group! {
        #![uniq(*b"9999999999999999")]
        struct Refer {
            x: usize,
            y: Other,
        }
    }

    #[test]
    fn external_split() {
        let (r,tr) = Refer::radt();

        let input = Refer { x: 12, y: Other };

        let (num, mut deps) = (12usize).to_value();
        let num_hash = num.typing().hash();
        deps.push(Item::Value(num));

        let (_,t_other) = Other::radt();
        let other = TypedValue{kind: t_other, value: RADTValue::Product(Vec::new())};
        let other_hash = other.typing().hash();
        deps.push(Item::Value(other));

        let expected = TypedValue {
            kind: tr,
            value: RADTValue::Product(vec![
                      RADTValue::Hash(num_hash),
                      RADTValue::Hash(other_hash),
            ])
        };

        assert_eq!(input.to_value(), (expected, deps))
    }
}

mod t13 {
    use super::*;

    bridged_group! {
        #![uniq(*b"1234567812345678")]
        struct Thing;
    }

    #[test]
    fn hello_deserialize() {
        let val = RADTValue::Product(Vec::new());
        let t = Thing::deserialize(&val, &HashMap::new());
    }

    bridged_group! {
        #![uniq(*b"9999999999999999")]
        struct One {
            a: Thing,
            b: Two,
        }
        enum Two {
            A(Thing),
            B { val: Box<One> },
        }
    }

    #[test]
    fn referential_deserialize() {
        let expected = One {
            a: Thing,
            b: Two::B {
                val: Box::new(One {
                    a: Thing,
                    b: Two::A(Thing),
                })
            }
        };
        let (_,t_thing) = Thing::radt();
        let thing = TypedValue {
            kind: t_thing,
            value: RADTValue::Product(Vec::new()),
        };
        let thing_hash = thing.typing().hash();

        let mut deps = HashMap::new();
        deps.insert(thing_hash, Item::Value(thing));

        let (_,t_one) = One::radt();

        let input = TypedValue {
            kind: t_one,
            value: RADTValue::Product(vec![
                RADTValue::Hash(thing_hash), // a: Thing
                RADTValue::Sum {  // b: Two::B
                    kind: 1,
                    value: Box::new(RADTValue::Product(vec![
                        RADTValue::Product(vec![  // val: Box::new(One
                            RADTValue::Hash(thing_hash), // a: Thing
                            RADTValue::Sum { // b: Two::A
                                kind: 0,
                                value: Box::new(RADTValue::Product(vec![RADTValue::Hash(thing_hash)])) // Two::A(Thing)
                            },
                        ]),
                    ])),
                }
            ]),
        };

        One::from_value(&input, &deps).unwrap();
    }
}

mod t14 {
    use super::*;

    bridged_group!{
        #![uniq(*b"1111222233334444")]
        #[derive(Debug, Clone, PartialEq)]
        struct Thing(usize);
    }

    #[test]
    fn deserialize_usize() {
        let (_,t_usize) = usize::radt();
        let blob = Item::Blob(Blob{bytes:(12usize).to_be_bytes().to_vec()});
        let blob_hash = blob.hash();

        let mut deps = HashMap::new();
        deps.insert(blob_hash, blob);

        let input = TypedValue {
            kind: t_usize,
            value: RADTValue::Hash(blob_hash),
        };
        
        assert_eq!(usize::from_value(&input, &deps).unwrap(), 12usize);
    }

    #[test]
    fn deserialize_contained_usize() {
        let expected = Thing(12);

        let mut deps = HashMap::new();

        let (_,t_usize) = usize::radt();
        let blob = Item::Blob(Blob{bytes:(12usize).to_be_bytes().to_vec()});
        let blob_hash = blob.hash();
        deps.insert(blob_hash, blob);

        let u_size = Item::Value(TypedValue {
            kind: t_usize,
            value: RADTValue::Hash(blob_hash),
        });
        let usize_hash = u_size.hash();
        deps.insert(usize_hash, u_size);

        let (_,t_thing) = Thing::radt();
        let input = TypedValue{
            kind: t_thing,
            value: RADTValue::Product(vec![RADTValue::Hash(usize_hash)]),
        };
        
        assert_eq!(Thing::from_value(&input, &deps).unwrap(), expected);
    }
}
