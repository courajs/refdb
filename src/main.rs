#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::{
    path::Path,
};

use failure::Error;
use hex_literal::hex;
use rkv::{Manager, Rkv, SingleStore, StoreOptions};

pub mod core;
pub mod error;
pub mod labels;
pub mod storage;
pub mod types;

use crate::core::*;
use crate::storage::*;
use crate::types::*;
use crate::labels::*;

fn main() -> Result<(), Error> {
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

    let ref1 = Typing {
        kind: TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        },
        data: blob1.hash(),
    };
    let ref2 = Typing {
        kind: TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        },
        data: blob2.hash(),
    };

    let mut uniq = [0; 16];
    uniq[0] = 254;
    uniq[15] = 239;
    let double_ref_type = RADT {
        uniqueness: uniq,
        items: vec![RADTItem::Product(vec![
            RADTItem::ExternalType(TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 0,
            }),
            RADTItem::ExternalType(TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 0,
            }),
        ])],
    };

    let double_ref_typedef = Typing {
        kind: TypeRef {
            definition: RADT_TYPE_HASH,
            item: 0,
        },
        data: double_ref_type.hash(),
    };

    db.put(&blob1)?;
    db.put(&blob2)?;
    db.put(&ref1)?;
    db.put(&ref2)?;
    db.put(&double_ref_type)?;
    db.put(&double_ref_typedef)?;

    let instance = RADTValue::Product(vec![
        RADTValue::Hash(ref1.hash()),
        RADTValue::Hash(ref2.hash()),
    ]);

    let confirmations = validate_radt_instance(&double_ref_type, 0, &instance)?;
    db.confirm_typings(&confirmations)?;

    let typing = Typing {
        kind: TypeRef {
            definition: double_ref_typedef.hash(),
            item: 0,
        },
        data: instance.hash(),
    };

    db.put(&instance)?;
    db.put(&typing)?;

    let utf8string = RADT {
        uniqueness: hex!("cafebabe ba5eba11 b01dface ca11ab1e"),
        items: vec![RADTItem::ExternalType(TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        })],
    };
    let utf8typedef = Typing {
        kind: TypeRef {
            definition: RADT_TYPE_HASH,
            item: 0,
        },
        data: utf8string.hash(),
    };

    db.put(&utf8string)?;
    db.put(&utf8typedef)?;

    let utf8hash = utf8typedef.hash();
    let utf8 = TypeRef {
        definition: utf8hash,
        item: 0,
    };

    let mut defs = RADT {
        uniqueness: [0; 16],
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
    };

    // Nil;
    // Cons = {head: (nah | bool (yes | no) | val _ | struct {val: _}), tail: List};
    // List = (Cons | Nil);

    /*

    let (typeref, val) = match db.get(typing.hash())? {
        Item::Value(t, v) => (t, v),
        _ => bail!("nah"),
    };

    dbg!(typeref);
    dbg!(val);
    */

    Ok(())
}
