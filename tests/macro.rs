#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use rf0::types::*;
use rf0::core::*;
use rf0::storage::Storable;
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
