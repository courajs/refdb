#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::path::Path;
use std::collections::HashMap;

use failure::Error;
use failure::bail;
use hex_literal::hex;
use rkv::{Manager, Rkv, SingleStore, StoreOptions};

#[macro_use]
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
use crate::eval::Env;

use std::ops::Deref;

fn run_app() -> Result<(), Error> {

    let args: Vec<String> = std::env::args().collect();

    if args.len() <= 1 {
        bail!("provide a command!")
    }
    if args.len() <= 2 {
        bail!("provide an argument!")
    }
    if !(args[1] == "foo" || args[1] == "store_string" || args[1] == "fetch_string" || args[1] == "store" || args[1] == "list_types") {
        bail!("commands are \"store\", \"store_string\" and \"fetch_string\"")
    }

    let arc = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(Path::new("/Users/aaron/dev/rf0/data"), Rkv::new)
        .unwrap();
    let env = arc.read().unwrap();
    let store: SingleStore = env.open_single("main", StoreOptions::create()).unwrap();

    let db = Db {
        env: &env,
        store: &store,
    };

    db.init(1, |db| {
        let (string_rad, stringtype) = String::radt();
        let (labels_rad, labeltype) = LabelSet::radt();
        let (env_rad, _) = Env::radt();
        db.put_item(&Item::TypeDef(string_rad.clone()))?;
        db.put_item(&Item::TypeDef(labels_rad.clone()))?;
        db.put_item(&Item::TypeDef(env_rad.clone()))?;

        let mut env = db.get_default_env()?.unwrap_or_else(||
            Env {
                labelings: HashMap::new(),
                variables: HashMap::new(),
            }
        );
        env.labelings.insert(Item::TypeDef(string_rad).hash(), LabelSet(vec![
                Label {
                    name: "String".to_owned(),
                    item: LabeledItem::Type,
                },
        ]));

        db.update_default_env(&env)
    })?;

    if args[1] == "list_types" {
        let env = db.get_default_env()?.unwrap();

        let mut types: Vec<Hash> = Vec::new();
        {
            let r = db.reader();
            for t in db.iter_typings(&r) {
                if t.kind == RADT_TYPE_REF {
                    types.push(t.hash());
                }
            }
        }
        let (labeled, unlabeled) = types.into_iter().partition::<Vec<Hash>, _>(|t| env.labelings.contains_key(t));

        for t in labeled {
            if let Ok(Item::TypeDef(r)) = db.get(t) {
                let l = env.labelings.get(&t).unwrap();
                let s = labels::print_with_labeling(&r, &l)?;
                println!("{}", s);
            }
        }
        for t in unlabeled {
            if let Ok(Item::TypeDef(r)) = db.get(t) {
                println!("{}", r);
            }
        }
    }

    if args[1] == "store" {
        let input = args[2].deref();
        let defs = match lang::parse_statements(input) {
            Err(e) => {
                bail!("Error parsing input: {:?}", e);
            },
            Ok((_,defs)) => defs,
        };
        let almost = eval::definitions(&defs)?;

        let mut env = db.get_default_env()?.unwrap();

        let existing_names = env.defined_names();

        let dups: Vec<&str> = almost.defined_names().into_iter().filter(|n| existing_names.contains(n)).collect();

        if dups.len() > 0 {
            bail!("The following names are already defined: {:?}", dups);
        }

        let new_defs = almost.resolve(&env.name_resolutions(), &HashMap::new());

        let new_label = new_defs.labels;
        let new_rad = RADT {
            uniqueness: rand::random(),
            items: new_defs.types,
        };
        
        let new_hash = db.put_item(&Item::TypeDef(new_rad))?;

        env.labelings.insert(new_hash, new_label);
        db.update_default_env(&env)?;
    }

    if args[1] == "store_string" {
        let (val, deps) = args[2].to_value();
        for i in deps { db.put_item(&i)?; }
        let h = db.put_item(&Item::Value(val))?;
        println!("{:?}", h);
    }

    if args[1] == "fetch_string" {
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
