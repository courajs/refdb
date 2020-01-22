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

#[derive(Debug)]
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

impl<'a> Db<'a> {
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
        let mut writer = self.env.write().unwrap();
        self.store.put(&mut writer, "default_env", &Value::Blob(&h.0[..]))
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
        println!("h {:?}", h);
        if let None = h {
            return Ok(None);
        }
        let hash = h.unwrap();
        println!("Getting environment ({})", hash);
        self.get_env(hash)
    }

    pub fn get_env(&self, h: Hash) -> Result<Option<Env>, MonsterError> {

        fn string_hashes_for_label(v: &RADTValue, string_hashes: &mut Vec<Hash>) {
            let fields = sure!(v, RADTValue::Product(fields) => fields);
            string_hashes.push(sure!(&fields[0], RADTValue::Hash(h) => *h));
            match &fields[1] {
                RADTValue::Sum{kind:0,value} | RADTValue::Sum{kind:1,value} => {
                    let mut sub_labels = value.deref();
                    while let RADTValue::Sum{kind:1,value} = sub_labels {
                        let cons = sure!(value.deref(), RADTValue::Product(v) => v);
                        string_hashes_for_label(&cons[0], string_hashes);
                        sub_labels = &cons[1];
                    }
                },
                _ => {}
            }
        }

        let stored = self.get(h)?;
        let typed = sure!(stored, Item::Value(v) => v; return Err(MonsterError::Todo("env wasn't a value")));
        let (rad, env_typeref) = Env::radt();
        if typed.kind != env_typeref {
            return Err(MonsterError::Todo("Tried to get_env a non-env"));
        }
        println!("hey");

        let mut deps: HashMap<Hash, Item> = HashMap::new();
        // already verified as it was constructed from bytes by .get()

        let both = sure!(&typed.value, RADTValue::Product(v) => v);
        let [mut labels, mut vars] = sure!(&both[..], [a,b]);

        let mut labelset_hashes = Vec::<Hash>::new();
        while let RADTValue::Sum{kind:1, value} = labels {
            let cons = sure!(value.deref(), RADTValue::Product(v) => v);
            labels = &cons[1];

            let entry = sure!(&cons[0], RADTValue::Product(v) => v);
            let label_hash = sure!(&entry[1], RADTValue::Hash(h) => *h);
            labelset_hashes.push(label_hash);
        }


        let mut string_hashes = Vec::<Hash>::new();
        for h in labelset_hashes {
            let labelset = sure!(self.get(h)?, Item::Value(TypedValue{value,..}) => value);
            let mut label_pointer = &labelset;
            while let RADTValue::Sum{kind:1, value} = label_pointer {
                let cons = sure!(value.deref(), RADTValue::Product(v) => v);
                label_pointer = &cons[1];

                string_hashes_for_label(&cons[0], &mut string_hashes);
            }
        }

        while let RADTValue::Sum{kind:1, value} = vars {
            let cons = sure!(value.deref(), RADTValue::Product(v) => v);
            vars = &cons[1];

            let entry = sure!(&cons[0], RADTValue::Product(v) => v);
            let name_hash = sure!(&entry[0], RADTValue::Hash(h) => *h);
            string_hashes.push(name_hash);
        }

        let mut string_body_hashes = Vec::new();
        for h in string_hashes {
            let s = self.get(h)?;
            string_body_hashes.push(sure!(&s, Item::Value(TypedValue{value: RADTValue::Hash(h2), ..}) => *h2));
            deps.insert(h, s);
        }

        for h in string_body_hashes {
            let body = self.get(h)?;
            deps.insert(h, body);
        }

        Env::from_value(&typed, &deps).map(|e| Some(e))
    }

    pub fn get_default_env_hash(&self) -> Result<Option<Hash>, MonsterError> {
        let reader = self.env.read().expect("reader");
        let r = self.store.get(&reader, "default_env").map_err(|e| MonsterError::RkvError(e))?;
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
        use std::collections::HashMap;
        use crate::bridge::Bridged;
        let s = self.get(hash)?;
        match s {
            Item::Value(val) => {
                let (_, t) = String::radt();
                if val.kind != t {
                    return Err(MonsterError::Todo("not a string"));
                }
                match val.value {
                    RADTValue::Hash(h) => {
                        let b = self.get(h)?;
                        let mut deps = HashMap::new();
                        deps.insert(h, b);
                        String::from_value(&val, &deps)
                    },
                    _ => return Err(MonsterError::Todo("not a string 2"))
                }
            },
            _ => return Err(MonsterError::Todo("not a string"))
        }
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
}
