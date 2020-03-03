#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use rf0::types::*;
use rf0::core::*;
use rf0::storage::Storable;
use rf0::bridge::*;
use bridged_group::*;

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
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
        let (_,t_usize) = usize::radt();
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
