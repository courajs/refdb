use failure::Error;
use nom::{call, combinator::all_consuming, map, number::complete::be_u8, switch, IResult};
use rkv::Value;

use crate::core::*;
use crate::error::MonsterError;
use crate::types::*;
use crate::bridge::Bridged;

use std::collections::HashMap;
use std::ops::Deref;

impl Storable for RADTValue {
    const PREFIX: u8 = 0;
}

impl Storable for RADT {
    const PREFIX: u8 = 0;
}

pub trait Storable: Serializable {
    const PREFIX: u8;

    fn all_bytes(&self) -> Vec<u8> {
        let mut v = Vec::new();
        v.push(Self::PREFIX);
        self.bytes_into(&mut v);
        v
    }

    fn hash(&self) -> Hash {
        Hash::of(&self.all_bytes())
    }
}

impl Storable for Blob {
    const PREFIX: u8 = 0;
}

impl Storable for Typing {
    const PREFIX: u8 = 1;
}

pub struct Db<'a> {
    pub env: &'a rkv::Rkv,
    pub store: &'a rkv::SingleStore,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Item {
    Blob(Blob),
    BlobRef(Hash),
    TypeDef(RADT),
    Value(TypedValue),
}

impl Item {
    pub fn hash(&self) -> Hash {
        match self {
            Item::Blob(b) => b.hash(),
            Item::BlobRef(h) => (Typing { kind: BLOB_TYPE_REF, data: *h }).hash(),
            Item::TypeDef(t) => (Typing { kind: RADT_TYPE_REF, data: t.hash() }).hash(),
            Item::Value(val) => {
                let h = val.value.hash();
                return (Typing { kind: val.kind, data: h }).hash()
            },
        }
    }
}

#[derive(Debug)]
pub enum LiteralItem {
    Blob(Blob),
    Typing(Typing),
}

pub fn decode_item(bytes: &[u8]) -> IResult<&[u8], LiteralItem> {
    switch!(
        bytes,
        be_u8,
        0 => map!(call!(Blob::decode), |b| LiteralItem::Blob(b))
        | 1 => map!(call!(all_consuming(Typing::decode)), |t| LiteralItem::Typing(t))
    )
}

pub fn typing_from_typed_val(v: &TypedValue) -> Typing {
    Typing {
        kind: v.kind,
        data: v.value.hash(),
    }
}

use crate::eval::Env;

pub struct TypingIter<'a> {
    _iter: rkv::store::single::Iter<'a>,
}
impl Iterator for TypingIter<'_> {
    type Item = Typing;
    fn next(&mut self) -> Option<Typing> {
        let kv = self._iter.next()?;
        let (key, _val) = kv.unwrap();
        let val = _val.unwrap();
        if let Value::Blob(bytes) = val {
            // match decode_item(bytes) {
            //     Ok((more, literal)) => {
            //         if let LiteralItem::Typing(t) = literal {
            //             return Some(t);
            //         } else {
            //             return self.next();
            //         }
            //     },
            //     Err(e) => {
            //         eprintln!("e,k,v\n{:?}\n{:?}\n{:?}", e, key, val);
            //         panic!();
            //     }
            // }
            let thing = decode_item(bytes).unwrap().1;
            if let LiteralItem::Typing(t) = thing {
                return Some(t);
            } else {
                return self.next();
            }
        } else {
            panic!("non-blob in store");
        }
    }
}

impl<'a> Db<'a> {
    pub fn init(&self, gen: u64, f: impl FnOnce(&Self) -> Result<(), MonsterError>) -> Result<(), MonsterError> {
        let store = self.env.open_single("meta", rkv::StoreOptions::create()).unwrap();
        {
            let reader = self.env.read().expect("reader");
            let r = store.get(&reader, "init").map_err(|e| MonsterError::RkvError(e))?;

            if let Some(Value::U64(current_gen)) = r {
                if gen == current_gen {
                    return Ok(())
                }
            }
        }
        f(self)?;
        {
            let mut writer = self.env.write().expect("writer");
            store.put(&mut writer, "init", &Value::U64(gen)).map_err(|e| MonsterError::RkvError(e))?;
            writer.commit().map_err(MonsterError::RkvError)?;
        }

        Ok(())
    }

    // FIXME handle errors
    pub fn iter_typings<'b>(&self, reader: &'b rkv::Reader) -> TypingIter<'b> {
        let iter = self.store.iter_start(reader).unwrap();
        TypingIter { 
            _iter: iter,
        }
    }

    pub fn reader(&self) -> rkv::Reader {
        self.env.read().expect("reader")
    }

    pub fn put(&self, item: &impl Storable) -> Result<Hash, MonsterError> {
        // FIXME - handle errors properly here
        let hash = item.hash();
        let mut writer = self.env.write().unwrap();
        self.store
            .put(&mut writer, &hash, &Value::Blob(&item.all_bytes()))
            .map_err(|e| MonsterError::RkvError(e))?;

        writer.commit().map_err(|e| MonsterError::RkvError(e))?;

        Ok(hash)
    }

    pub fn put_item(&self, item: &Item) -> Result<Hash, MonsterError> {
        match item {
            Item::Blob(b) => self.put(b),
            Item::TypeDef(t) => {
                let body = self.put(t)?;
                self.put(&Typing { kind: RADT_TYPE_REF, data: body })
            },
            Item::BlobRef(h) => self.put(&Typing { kind: BLOB_TYPE_REF, data: *h}),
            Item::Value(val) => {
                let h = self.put(&val.value)?;
                self.put(&Typing { kind: val.kind, data: h })
            }
        }
    }

    pub fn get_bytes(&self, hash: Hash) -> Result<Vec<u8>, MonsterError> {
        let reader = self.env.read().expect("reader");
        let r = self
            .store
            .get(&reader, &hash)
            .map_err(|e| MonsterError::RkvError(e))?;
        match r {
            Some(Value::Blob(bytes)) => Ok(bytes.into()),
            Some(_) => Err(MonsterError::NonBlob(hash)),
            None => Err(MonsterError::NotFound(hash)),
        }
    }

    pub fn update_default_env_hash(&self, h: Hash) -> Result<(), MonsterError> {
        let store = self.env.open_single("meta", rkv::StoreOptions::create()).unwrap();
        let mut writer = self.env.write().unwrap();
        store.put(&mut writer, "default_env", &Value::Blob(&h.0[..]))
            .map_err(|e| MonsterError::RkvError(e))?;

        writer.commit().map_err(|e| MonsterError::RkvError(e))
    }

    pub fn update_default_env(&self, env: &Env) -> Result<(), MonsterError> {
        let (env_value, env_deps) = env.to_value();

        for item in env_deps {
            self.put_item(&item)?;
        }
        let env_hash = self.put_item(&Item::Value(env_value))?;

        println!("New environment hash is {}:\n{:?}", env_hash, env);
        self.update_default_env_hash(env_hash)
    }

    pub fn get_default_env(&self) -> Result<Option<Env>, MonsterError> {
        let h = self.get_default_env_hash()?;
        if let None = h {
            return Ok(None);
        }
        let hash = h.unwrap();
        self.get_env(hash)
    }

    pub fn get_env(&self, h: Hash) -> Result<Option<Env>, MonsterError> {
        let stored = self.get(h)?;
        let typed = sure!(stored, Item::Value(v) => v; return Err(MonsterError::Todo("env wasn't a value")));
        let deps = crate::bridge::HashMapCachedDb::new(self);
        Env::from_value(&typed, &deps).map(|e|Some(e))


        // println!("1");
        // fn string_hashes_for_label(v: &RADTValue, string_hashes: &mut Vec<Hash>) {
        //     let fields = sure!(v, RADTValue::Product(fields) => fields);
        //     string_hashes.push(sure!(&fields[0], RADTValue::Hash(h) => *h));
        //     match &fields[1] {
        //         RADTValue::Sum{kind:0,value} | RADTValue::Sum{kind:1,value} => {
        //             let mut sub_labels = value.deref();
        //             while let RADTValue::Sum{kind:1,value} = sub_labels {
        //                 let cons = sure!(value.deref(), RADTValue::Product(v) => v);
        //                 string_hashes_for_label(&cons[0], string_hashes);
        //                 sub_labels = &cons[1];
        //             }
        //         },
        //         _ => {}
        //     }
        // }

        // println!("2");
        // let stored = self.get(h)?;
        // let typed = sure!(stored, Item::Value(v) => v; return Err(MonsterError::Todo("env wasn't a value")));
        // let (rad, env_typeref) = Env::radt();
        // if typed.kind != env_typeref {
        //     return Err(MonsterError::Todo("Tried to get_env a non-env"));
        // }

        // println!("3");
        // let mut deps: HashMap<Hash, Item> = HashMap::new();
        // // already verified as it was constructed from bytes by .get()

        // let both = sure!(&typed.value, RADTValue::Product(v) => v);
        // let [mut labels, mut vars] = sure!(&both[..], [a,b]);

        // println!("4");
        // let mut labelset_hashes = Vec::<Hash>::new();
        // while let RADTValue::Sum{kind:1, value} = labels {
        //     let cons = sure!(value.deref(), RADTValue::Product(v) => v);
        //     labels = &cons[1];

        //     let entry = sure!(&cons[0], RADTValue::Product(v) => v);
        //     let label_hash = sure!(&entry[1], RADTValue::Hash(h) => *h);
        //     labelset_hashes.push(label_hash);
        // }

        // println!("5");

        // let mut string_hashes = Vec::<Hash>::new();
        // for h in labelset_hashes {
        //     let labelset = self.get(h)?;
        //     let mut label_pointer = sure!(&labelset, Item::Value(TypedValue{value,..}) => value);
        //     while let RADTValue::Sum{kind:1, value} = label_pointer {
        //         let cons = sure!(value.deref(), RADTValue::Product(v) => v);
        //         label_pointer = &cons[1];

        //         string_hashes_for_label(&cons[0], &mut string_hashes);
        //     }
        //     deps.insert(h, labelset);
        // }

        // println!("6");
        // while let RADTValue::Sum{kind:1, value} = vars {
        //     let cons = sure!(value.deref(), RADTValue::Product(v) => v);
        //     vars = &cons[1];

        //     let entry = sure!(&cons[0], RADTValue::Product(v) => v);
        //     let name_hash = sure!(&entry[0], RADTValue::Hash(h) => *h);
        //     string_hashes.push(name_hash);
        // }

        // println!("7");
        // let mut string_body_hashes = Vec::new();
        // for h in string_hashes {
        //     let s = self.get(h)?;
        //     string_body_hashes.push(sure!(&s, Item::Value(TypedValue{value: RADTValue::Hash(h2), ..}) => *h2));
        //     deps.insert(h, s);
        // }

        // println!("8");
        // for h in string_body_hashes {
        //     let body = self.get(h)?;
        //     deps.insert(h, body);
        // }

        // println!("9");
        // dbg!(&typed, deps.keys().collect::<Vec<_>>());
        // let e = Env::from_value(&typed, &mut deps).map(|e| Some(e));
        // println!("10");
        // e
    }

    pub fn get_default_env_hash(&self) -> Result<Option<Hash>, MonsterError> {
        let store = self.env.open_single("meta", rkv::StoreOptions::create()).unwrap();
        let reader = self.env.read().expect("reader");
        let r = store.get(&reader, "default_env").map_err(|e| MonsterError::RkvError(e))?;
        match r {
            Some(Value::Blob(bytes)) => {
                if bytes.len() == 32 {
                    Ok(Some(Hash::sure_from(bytes)))
                } else {
                    Err(MonsterError::Todo("non-hash bytes in default_env slot"))
                }
            },
            Some(_) => Err(MonsterError::Todo("default_env pointed to non-blob")),
            None => Ok(None),
        }
    }

    pub fn get_string(&self, hash: Hash) -> Result<String, MonsterError> {
        use crate::bridge::FetchStrategy;
        String::hydrate(hash, self)
    }

    pub fn get(&self, hash: Hash) -> Result<Item, MonsterError> {
        let bytes = self.get_bytes(hash)?;
        match decode_item(&bytes) {
            Err(e) => Err(MonsterError::ParseError(hash, format!("{:?}", e))),
            Ok((_, LiteralItem::Blob(b))) => Ok(Item::Blob(b)),
            Ok((_, LiteralItem::Typing(typing))) => {
                if typing.kind.definition == BLOB_TYPE_HASH {
                    return Ok(Item::BlobRef(typing.data));
                } else if typing.kind.definition == RADT_TYPE_HASH {
                    let definition_bytes = self.get_bytes(typing.data)?;
                    let definition_blob = match decode_item(&definition_bytes)
                        .map_err(|e| MonsterError::ParseError(hash, format!("{:?}", e)))?
                    {
                        (_, LiteralItem::Typing(_)) => Err(MonsterError::BrokenTypedef),
                        (_, LiteralItem::Blob(b)) => Ok(b),
                    }?;
                    let (_, def) = all_consuming(RADT::decode)(&definition_blob.bytes)
                        .map_err(|e| MonsterError::ParseError(hash, format!("{:?}", e)))?;
                    return Ok(Item::TypeDef(def));
                } else {
                    match self.get(typing.kind.definition)? {
                        Item::Blob(_) | Item::BlobRef(_) | Item::Value(_) => {
                            Err(MonsterError::UntypedTyping)
                        }
                        Item::TypeDef(radt) => {
                            let instance_bytes = self.get_bytes(typing.data)?;
                            let instance = validate_radt_instance_bytes(
                                &radt,
                                typing.kind.item,
                                &instance_bytes[1..],
                            )
                            .map_err(|e| {
                                MonsterError::BrokenTyping {
                                    hash,
                                    target_type: typing.kind,
                                    err: format!("{}", e),
                                }
                            })?;
                            Ok(Item::Value(TypedValue {kind: typing.kind, value: instance}))
                        }
                    }
                }
            }
        }
    }

    pub fn confirm_typings(&self, typings: &[ExpectedTyping]) -> Result<(), Error> {
        for expected in typings {
            let bytes = self.get_bytes(expected.reference)?;
            match decode_item(&bytes[..]) {
                Err(e) => {
                    Err(MonsterError::ParseError(
                        expected.reference,
                        format!("{:?}", e),
                    ))?;
                }
                Ok((_, LiteralItem::Blob(_))) => {
                    Err(MonsterError::UntypedReference(expected.reference))?;
                }
                Ok((_, LiteralItem::Typing(typing))) => {
                    if typing.kind != expected.kind && expected.kind != ANY_TYPE_REF {
                        Err(MonsterError::MistypedReference {
                            reference: expected.reference,
                            expected_type: expected.kind,
                            actual_type: typing.kind,
                        })?;
                    }
                }
            }
        }
        Ok(())
    }

    pub fn resolve_hash_prefix<'b>(&self, reader: &'b rkv::Reader, prefix: &[u8]) -> Result<Hash, MonsterError> {
        let mut results = Vec::new();
        for item in self.store.iter_from(reader, prefix).map_err(MonsterError::RkvError)? {
            let (key, val) = item.map_err(MonsterError::RkvError)?;
            if key.starts_with(prefix) {
                results.push(Hash::sure_from(key));
            } else {
                break;
            }
        }

        match results.len() {
            0 => Err(MonsterError::HashResolutionNotFound),
            1 => Ok(results[0]),
            _ => Err(MonsterError::HashResolutionConflict(results)),
        }
    }

    pub fn delete(&self, h: Hash) -> Result<(), MonsterError> {
        let mut writer = self.env.write().unwrap();
        self.store.delete(&mut writer, h).map_err(MonsterError::RkvError)?;
        writer.commit().map_err(MonsterError::RkvError)
    }
}
