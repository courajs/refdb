#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::collections::BTreeMap;

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
            // D { x: Third },
        }
        #[derive(PartialEq, Eq, PartialOrd, Ord)]
        struct Other;
        struct Third {
            yes: String,
        }
    }

    #[test]
    fn enum_to_value() {
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
}
