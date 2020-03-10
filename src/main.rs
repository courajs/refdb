#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

use std::path::Path;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::collections::HashSet;

use failure::Error;
use failure::bail;
use hex_literal::hex;
use rkv::{Manager, Rkv, SingleStore, StoreOptions};

use rf0::sure;

use rf0::lang;
use rf0::types;
use rf0::labels;
use rf0::eval;
use rf0::core::*;
use rf0::labels::*;
use rf0::storage::*;
use rf0::types::*;
use rf0::bridge::*;
use rf0::error::MonsterError;
use rf0::eval::Env;
use rf0::lang::*;

use std::ops::Deref;

fn run_app() -> Result<(), Error> {

    let args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 {
        bail!("pass a command!");
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

    db.init(3, |db| {
        let (string_rad, stringtype) = String::radt();
        let (labels_rad, labeltype) = LabelSet::radt();
        let (env_rad, _) = Env::radt();
        db.put_item(&Item::TypeDef(string_rad.clone()))?;
        db.put_item(&Item::TypeDef(labels_rad.clone()))?;
        db.put_item(&Item::TypeDef(env_rad.clone()))?;

        let label_label = LabelSet::label();
        let (label_value, label_deps) = label_label.to_value();
        db.put_item(&Item::Value(label_value))?;
        for item in label_deps {
            db.put_item(&item)?;
        }

        let mut env = db.get_default_env()?.unwrap_or_else(||
            Env {
                labelings: BTreeMap::new(),
                variables: BTreeMap::new(),
            }
        );
        env.labelings.insert(Item::TypeDef(string_rad).hash(), LabelSet(vec![
                Label {
                    name: "String".to_owned(),
                    item: LabeledItem::Type,
                },
        ]));
        env.labelings.insert(Item::TypeDef(labels_rad).hash(), label_label);

        db.update_default_env(&env)
    })?;

    match args[1].deref() {
        "foo" => {

        },
        "list_of_type" => {
            if args.len() <= 2 {
                bail!("list what type?");
            }
            let env = db.get_default_env()?.unwrap();
            let names = env.name_resolutions();

            let spec = match lang::parse_typeref(&args[2]) {
                Err(_) => bail!("Specify a valid type"),
                Ok((_,s)) => s
            };

            use lang::TypeReference;
            let kind = match spec {
                TypeReference::Name(n) => {
                    match names.get(n) {
                        Some(t) => *t,
                        None => bail!("No type named \"{}\"", n),
                    }
                },
                TypeReference::Hash(h, item) => {
                    TypeRef {
                        definition: h,
                        item,
                    }
                },
                TypeReference::ShortHash(prefix, item) => {
                    TypeRef {
                        definition: db.resolve_hash_prefix(&db.reader(), &prefix)?,
                        item,
                    }
                },
            };
            
            let mut vals = Vec::new();
            {
                for t in db.iter_typings(&db.reader()) {
                    if t.kind == kind {
                        vals.push(t);
                    }
                }
            }

            let kin = db.get(kind.definition)?;
            let radt = sure!(kin, Item::TypeDef(r) => r);
            let spec = types::TypeSpec {
                definition: &radt,
                item: kind.item,
            };

            for t in vals {
                if t.kind == kind {
                    let val = db.get(t.hash())?;
                    let typed_val = sure!(val, Item::Value(tv) => tv);
                    let h = t.hash();
                    let value = typed_val.value;

                    println!("{}: {}", h, labels::print_val_with_env(&value, &spec, &env)?);
                }
            }
        },

        "list_types" => {
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
                    println!("ref: {}", r.typing().hash());
                    let l = env.labelings.get(&t).unwrap();
                    let s = labels::print_with_env(&r, &env)?;
                    println!("{}", s);
                }
            }
            for t in unlabeled {
                if let Ok(Item::TypeDef(r)) = db.get(t) {
                    println!("{}", r);
                }
            }
        },

        "store" => {
            if args.len() <= 2 {
                bail!("what should I store?");
            }
            let mut env: Env;

            let input = args[2].deref();
            let (defs, assignments) = lang::process_statements(input)?;

            if defs.is_empty() && assignments.is_empty() {
                Err(MonsterError::Formatted("Pass some statements!".to_owned()))?;
            }

            let mut radts: HashMap<Hash, RADT> = HashMap::new();
            let mut new_radt: Option<(Hash, RADT)> = None;
            if !defs.is_empty() {
                let almost = eval::definitions(&defs)?;

                env = db.get_default_env()?.unwrap();
                let existing_names = env.defined_names();

                let dups: Vec<&str> = almost.defined_names().into_iter().filter(|n| existing_names.contains(n)).collect();

                if dups.len() > 0 {
                    bail!("The following names are already defined: {:?}", dups);
                }

                let mut prefix_resolutions: HashMap<&[u8], Hash> = HashMap::new();
                {
                    let reader = db.reader();
                    for pre in almost.hash_prefixes.iter() {
                        prefix_resolutions.insert(
                            pre,
                            db.resolve_hash_prefix(&reader, pre)?
                        );
                    }
                }

                let new_defs = almost.resolve(&env.name_resolutions(), &prefix_resolutions);

                let new_label = new_defs.labels;
                let new_rad = RADT {
                    uniqueness: rand::random(),
                    items: new_defs.types,
                };
                let typing = Typing {
                    kind: RADT_TYPE_REF,
                    data: new_rad.hash(),
                };
                let new_hash = typing.hash();

                env.labelings.insert(new_hash, new_label);
                new_radt = Some((new_hash, new_rad.clone()));
                radts.insert(new_hash, new_rad);
            } else {
                env = db.get_default_env()?.unwrap();
                println!("empty");
            }


            // TODO check for duplicate names within assignments,
            // between assignments and typedefs, and between
            // assignments/typedefs and environment

            let mut type_name_resolutions: HashMap<&str, TypeRef> = HashMap::new();
            let mut type_prefix_resolutions: HashMap<Vec<u8>, Hash> = HashMap::new();
            {
                let mut names: HashSet<&str> = HashSet::new();
                let mut hashes: HashSet<TypeRef> = HashSet::new();
                let mut short_hashes: HashSet<(Vec<u8>, usize)> = HashSet::new();
                for a in assignments.iter() {
                    match &a.val.kind {
                        TypeReference::Name(n) => names.insert(n),
                        TypeReference::Hash(h, item) => hashes.insert(TypeRef{definition: *h, item: *item}),
                        TypeReference::ShortHash(pre, item) => short_hashes.insert((pre.clone(),*item)),
                    };
                }

                let mut radts_to_fetch: HashSet<Hash> = hashes.iter().map(|r| r.definition).collect();


                let name_lookups = env.name_resolutions();
                for n in names {
                    match name_lookups.get(n) {
                        Some(tr) => {
                            type_name_resolutions.insert(n, *tr);
                            radts_to_fetch.insert(tr.definition);
                        },
                        None => Err(MonsterError::Formatted(format!("Name not found: \"{}\"", n)))?,
                    }
                }
                {
                    let reader = db.reader();
                    for (pre,_) in short_hashes {
                        let h = match db.resolve_hash_prefix(&reader, &pre) {
                            Ok(h) => h,
                            Err(MonsterError::HashResolutionNotFound) => {
                                Err(MonsterError::Formatted(format!("No type found for prefix #{}", hex::encode(pre))))?;
                                unreachable!();
                            },
                            Err(e) => {
                                Err(e)?;
                                unreachable!();
                            }
                        };
                        type_prefix_resolutions.insert(pre, h);
                        radts_to_fetch.insert(h);
                    }
                }

                if let Some((h,_)) = &new_radt {
                    radts_to_fetch.remove(h);
                }

                for h in radts_to_fetch {
                    let item = db.get(h)?;
                    if let Item::TypeDef(r) = item {
                        radts.insert(h, r);
                    } else {
                        Err(MonsterError::Formatted(format!("{} was not a type: {:?}", h, item)))?;
                    }
                }
            }

            let mut value_names: HashSet<&str> = HashSet::new();
            let mut value_prefixes: HashSet<&[u8]> = HashSet::new();

            fn get_things<'a>(item: &'a ValueItem, names: &mut HashSet<&'a str>, prefixes: &mut HashSet<&'a [u8]>) {
                match item {
                    ValueItem::NameRef(n) => {names.insert(*n);},
                    ValueItem::ShortHashRef(pre) => {prefixes.insert(pre);},
                    ValueItem::Variant(def) => get_things(def.val.deref(), names, prefixes),
                    ValueItem::Fields(fields) => for (_,inner) in fields {
                        get_things(inner, names, prefixes);
                    }
                    _ => (),
                }
            }
            for item in assignments.iter() {
                get_things(&item.val.val, &mut value_names, &mut value_prefixes);
            }

            // resolve the value's referenced names and prefixes
            let mut value_name_resolutions = HashMap::new();
            for name in value_names {
                if let Some(h) = env.variables.get(name) {
                    value_name_resolutions.insert(name, *h);
                } else {
                    Err(MonsterError::Todo("referenced a thing you don't have"))?;
                    unreachable!();
                }
            }

            let mut value_prefix_resolutions = HashMap::new();
            {
                let reader = db.reader();
                for pre in value_prefixes {
                    let h = db.resolve_hash_prefix(&reader, pre)?;
                    value_prefix_resolutions.insert(pre, h);
                }
            }

            let mut new_values: HashMap<String, Hash> = HashMap::new();
            let mut all_expectations = Vec::new();
            let mut all_deps = Vec::new();
            for assignment in assignments.iter() {
                let kind = match &assignment.val.kind {
                    TypeReference::Hash(h, item) => TypeRef { definition: *h, item: *item },
                    TypeReference::ShortHash(pre, item) => {
                        // we explicitly fetched all names and prefixes earlier and would have
                        // already errored if any weren't found
                        let h = type_prefix_resolutions.get(pre).unwrap();
                        TypeRef { definition: *h, item: *item }
                    },
                    TypeReference::Name(n) => {
                        *type_name_resolutions.get(n).unwrap()
                    }
                };
                
                use rf0::eval::LabeledRADT;
                let labeled_type = if let Some(labeling) = env.labelings.get(&kind.definition) {
                    let radt = radts.get(&kind.definition).unwrap();
                    LabeledRADT::new(&radt, &labeling)
                } else {
                    Err(MonsterError::Todo("only support instantiating labeled types"))?;
                    unreachable!();
                };
                let (val, deps, expectations) = eval::validate_instantiate(
                    &assignment.val.val,
                    &labeled_type,
                    kind.item,
                    &value_name_resolutions,
                    &value_prefix_resolutions,
                )?;

                new_values.insert(assignment.ident.to_string(), val.typing().hash());
                all_deps.push(Item::Value(val));
                all_deps.extend(deps);
                all_expectations.extend(expectations);
            }

            db.confirm_typings(&all_expectations)?;

            // confirm expectations
            // store val and deps
            // put new variables in env

            
            if let Some((_,rad)) = new_radt {
                db.put_item(&Item::TypeDef(rad))?;
            }
            for item in all_deps {
                db.put_item(&item)?;
            }
            env.variables.extend(new_values);
            db.update_default_env(&env)?;
        },

        "list_vars" => {
            let env = db.get_default_env()?.unwrap();
            for (name, _) in env.variables {
                println!("{}", name);
            }
        },

        "inspect" => {
            if args.len() <= 2 {
                bail!("inspect what?");
            }
            let env = db.get_default_env()?.unwrap();

            let object_hash = match lang::as_object_ref(&args[2])? {
                lang::ObjectReference::Hash(h) => h,
                lang::ObjectReference::ShortHash(prefix) => {
                    let reader = db.reader();
                    db.resolve_hash_prefix(&reader, &prefix)?
                },
                lang::ObjectReference::Ident(name) => {
                    env.variables.get(name)
                        .or_else(|| {
                            env.labelings.iter().find_map(|(h, LabelSet(labels))| {
                                if labels.iter().any(|l| l.name == args[2].deref()) {
                                    Some(h)
                                } else {
                                    None
                                }
                            })
                        })
                        .map(|h| *h)
                        .ok_or(MonsterError::Todo("couldn't resolve identifier"))?
                }
            };
            let val = db.get(object_hash)?;
            println!("{:?}", object_hash);

            match val {
                Item::Blob(b) => println!("{:?}", b),
                Item::BlobRef(r) => println!("{}", r),
                Item::TypeDef(rad) => {
                    if let Some(l) = env.labelings.get(&object_hash) {
                        println!("{}", labels::print_with_env(&rad, &env)?);
                    } else {
                        println!("{}", rad)
                    }
                },
                Item::Value(typed_val) => {
                    if let Some(l) = env.labelings.get(&typed_val.kind.definition) {
                        let rad = sure!(db.get(typed_val.kind.definition)?, Item::TypeDef(r) => r);
                        println!("{}", labels::print_val_with_env(
                                &typed_val.value,
                                &types::TypeSpec {
                                    definition: &rad,
                                    item: typed_val.kind.item,
                                },
                                &env
                        )?);
                    } else {
                        println!("{}", &typed_val.value);
                    }
                },
            }
        }

        "store_string" => {
            if args.len() <= 2 {
                bail!("What string should I store?");
            }
            let (val, deps) = args[2].to_value();
            for i in deps { db.put_item(&i)?; }
            let h = db.put_item(&Item::Value(val))?;
            println!("{:?}", h);
        },

        "fetch_string" => {
            if args.len() <= 2 {
                bail!("What string should I fetch?");
            }
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

        _ => {
            bail!("no such command");
        }
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
