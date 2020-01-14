use failure::Error;
use nom::{call, combinator::all_consuming, map, number::complete::be_u8, switch, IResult};
use rkv::Value;

use crate::core::*;
use crate::error::MonsterError;
use crate::types::*;

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
            Item::TypeDef(t) => self.put(t),
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
                    if typing.kind != expected.kind {
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
