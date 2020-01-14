#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::path::Path;

use failure::Error;
use failure::bail;
use hex_literal::hex;
use rkv::{Manager, Rkv, SingleStore, StoreOptions};

pub mod core;
pub mod error;
pub mod labels;
pub mod storage;
pub mod types;
pub mod lang;
pub mod eval;
pub mod bridge;

use crate::core::*;
use crate::labels::*;
use crate::storage::*;
use crate::types::*;
use crate::bridge::Bridged;
use crate::error::MonsterError;

fn run_app() -> Result<(), Error> {

    let args: Vec<String> = std::env::args().collect();
    println!("{:?}", args);

    if args.len() <= 1 {
        bail!("provide a command!")
    }
    if args.len() <= 2 {
        bail!("provide an argument!")
    }
    if !(args[1] == "store" || args[1] == "fetch") {
        bail!("commands are \"store\" and \"fetch\"")
    }
    println!("{:?}", args[1]);

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

    let (rad, stringtype) = String::radt();
    db.put_item(&Item::TypeDef(rad))?;

    if args[1] == "store" {
        let (val, deps) = args[2].encode();
        for i in deps { db.put_item(&i)?; }
        let h = db.put_item(&Item::Value(val))?;
        println!("{:?}", h);
    }

    if args[1] == "fetch" {
        let bytes = match hex::decode(&args[2]) {
            Err(e) => {
                return Err(MonsterError::Todo("bad hex"))?
            },
            Ok(b) => b
        };
        if bytes.len() != 32 {
            return Err(MonsterError::Todo("wrong hash length"))?
        }
        let h = Hash::sure_from(&bytes);
        // dbg!(decode_item(&db.get_bytes(h)?));
        // dbg!(decode_item(&db.get_bytes(Hash(hex!("fc350f5b37f4e30ad40c4d4da3659982354621b220294e8b3873e581dfc77a7d")))?));
        let s = db.get_string(h)?;
        println!("Here's your string: {:?}", s);
    }

    Ok(())
}

fn main() {
    match run_app() {
        Ok(()) => {},
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    }
}



    /*
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
    */
