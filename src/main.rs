#![allow(dead_code,unused_variables)]

// use std::env;
// use num::{BigUint};

use rkv::{Manager, Rkv, SingleStore, StoreOptions};
use std::path::Path;
use crate::storage::Storable;
use crate::typings::{ADT, ADTItem, ADTValue, Typing, BLOB_TYPE_HASH, ADT_TYPE_HASH};
use crate::error::Error;
use crate::error::Error::*;

mod error {
    #[derive(Debug)]
    pub enum Error {
        RkvError(rkv::error::StoreError),
        ADTParseError(&'static str),
        TypingParseError(&'static str),
    }
    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            <Self as std::fmt::Debug>::fmt(self, f)
        }
    }

    impl std::error::Error for Error {}
}

// Basic persistence primitives
mod storage {
    use sha3::{Digest, Sha3_256};
    use std::fmt;

    #[derive(Copy, Clone)]
    pub struct Hash(pub [u8; 32]);
    impl AsRef<[u8]> for Hash {
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    impl Hash {
        pub fn sure_from(value: &[u8]) -> Hash {
            let mut val = [0; 32];
            val.copy_from_slice(value);
            Hash(val)
        }
    }

    impl fmt::Debug for Hash {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "sha-256:{}", hex::encode(self.0))
        }
    }

    pub trait Storable {
        fn bytes(&self) -> Vec<u8>;
        fn hash(&self) -> Hash {
            let mut val: [u8; 32] = Default::default();
            let mut hasher = Sha3_256::new();
            hasher.input(&self.bytes());
            val.copy_from_slice(hasher.result().as_ref());
            Hash(val)
        }
    }

    pub struct Blob {
        pub bytes: Vec<u8>,
    }

    impl Storable for Blob {
        fn bytes(&self) -> Vec<u8> {
            let mut v = Vec::with_capacity(&self.bytes.capacity() + 1);
            // Blobs all have a leading 0 byte
            v.push(0);
            v.extend_from_slice(&self.bytes);
            v
        }
    }
}

mod typings {
    use crate::storage::{Hash, Storable};
    use std::convert::TryInto;
    use std::fmt;
    use hex_literal::hex;
    use crate::error::Error;
    use crate::error::Error::*;

    // Algebraic Data Type
    pub struct ADT {
        // This is for types to be more like nominal types and less like
        // structural types.
        // Maybe see https://www.unisonweb.org/docs/language-reference/type-declarations#unique-types
        // Presumably this is generated randomly to minimize collisions with other types.
        pub uniqueness: [u8; 16],
        pub value: ADTItem,
    }

    // blob zero byte + uniqueness + ADTItem tag + a single hash (smallest variant)
    const MIN_ADT_SIZE: usize = 1 + 16 + 1 + 32;
    impl ADT {
        pub fn decode(bytes: &[u8]) -> Result<ADT, Error> {
            if bytes.len() < MIN_ADT_SIZE {
                Err(ADTParseError("too short overall"))
            } else {
                let (value, rest) = ADTItem::decode(&bytes[17..])?;
                if rest.len() == 0 {
                    let mut uniqueness = [0; 16];
                    uniqueness.copy_from_slice(&bytes[1..17]);
                    Ok(ADT { uniqueness, value })
                } else {
                    dbg!(rest);
                    Err(ADTParseError("extra left over"))
                }
            }
        }
    }

    impl fmt::Debug for ADT {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(
                f,
                "ADT {{ \n    uniqueness: 0x{},\n    value: {:#?} }}",
                &hex::encode(self.uniqueness),
                &self.value
            )
        }
    }

    #[derive(Debug)]
    pub enum ADTItem {
        Hash(Hash),
        Sum(Vec<ADTItem>),
        Product(Vec<ADTItem>),
    }
    impl ADTItem {
        fn decode(bytes: &[u8]) -> Result<(ADTItem, &[u8]), Error> {
            if bytes.len() == 0 {
                return Err(ADTParseError("nothing available for item"));
            } else if bytes[0] > 2 {
                return Err(ADTParseError("invalid item disambiguation byte"));
            } else if bytes[0] == 0 {
                if bytes.len() < 33 {
                    return Err(ADTParseError("not long enough for hash"));
                } else {
                    return Ok((ADTItem::Hash(Hash::sure_from(&bytes[1..33])), &bytes[33..]));
                }
            } else if bytes[0] == 1 || bytes[0] == 2 {
                if bytes.len() < 9 {
                    return Err(ADTParseError("not long enough for sum"));
                }
                let num = usize::from_be_bytes(bytes[1..9].try_into().unwrap());
                let mut v = Vec::with_capacity(num);
                let mut rest = &bytes[9..];
                for _ in 0..num {
                    let (next, more) = ADTItem::decode(rest)?;
                    v.push(next);
                    rest = more;
                }
                if bytes[0] == 1 {
                    return Ok((ADTItem::Sum(v), rest));
                } else {
                    return Ok((ADTItem::Product(v), rest));
                }
            }
            return Err(ADTParseError("unknown item type"));
        }
        fn bytes(&self) -> Vec<u8> {
            let mut result = Vec::new();
            match self {
                ADTItem::Hash(h) => {
                    result.push(0);
                    result.extend_from_slice(&h.0[..]);
                    result
                }
                ADTItem::Sum(items) => {
                    // TODO: maybe sort these somehow for easier structural comparison?
                    result.push(1);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        result.extend_from_slice(&item.bytes());
                    }
                    result
                }
                ADTItem::Product(items) => {
                    result.push(2);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        result.extend_from_slice(&item.bytes());
                    }
                    result
                }
            }
        }
    }
    impl Storable for ADT {
        fn bytes(&self) -> Vec<u8> {
            let mut v = Vec::new();
            // Blobs all have a leading 0 byte
            v.push(0);
            v.extend_from_slice(&self.uniqueness);
            v.extend_from_slice(&self.value.bytes());
            v
        }
    }

    pub static BLOB_TYPE_HASH: Hash = Hash(hex!(
        "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    ));
    pub static ADT_TYPE_HASH: Hash = Hash(hex!(
        "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001"
    ));

    #[derive(Debug)]
    pub struct Typing {
        // Either this typing represents a type or a value.
        // If it represents a type, then type_hash will a special value
        // that means type (1 for adts, 2 for custom types, etc)
        // Otherwise, if it's a value, it will be the hash of a typing
        // which represents a type.
        // If this contained the hash of a blob, it would be invalid.
        pub type_hash: Hash,
        // the actual data blob
        pub data_hash: Hash,
    }

    impl Storable for Typing {
        fn bytes(&self) -> Vec<u8> {
            let mut v = Vec::new();
            // Typings all have a leading 1 byte
            v.push(1);
            v.extend_from_slice(&self.type_hash.0[..]);
            v.extend_from_slice(&self.data_hash.0[..]);
            v
        }
    }

    // leading 1 to indicate a typing, then two hashes
    const TYPING_SIZE: usize = 1 + 32 + 32;
    impl Typing {
        pub fn decode(bytes: &[u8]) -> Result<Typing, Error> {
            if bytes.len() != 65 {
                return Err(TypingParseError("Wrong length for a typing object"));
            }
            if bytes[0] == 0 {
                return Err(TypingParseError("This is a blob, not a typing!"));
            }
            if bytes[0] != 1 {
                return Err(TypingParseError("This isn't a typing!"));
            }
            Ok(Typing {
                type_hash: Hash::sure_from(&bytes[1..33]),
                data_hash: Hash::sure_from(&bytes[33..65]),
            })
        }
    }

    pub enum ADTValue {
        Hash(Hash),
        Sum {
            kind: u8,
            value: Box<ADTValue>,
        },
        Product(Vec<ADTValue>),
    }

    impl ADTValue {
        fn just_bytes(&self, v: &mut Vec<u8>) {
            match self {
                ADTValue::Hash(h) => {
                    v.extend_from_slice(&h.0[..])
                },
                ADTValue::Sum {
                    kind, value
                } => {
                    v.push(*kind);
                    value.just_bytes(v);
                },
                ADTValue::Product(subs) => {
                    for sub in subs {
                        sub.just_bytes(v);
                    }
                }
            }
        }
    }

    impl Storable for ADTValue {
        fn bytes(&self) -> Vec<u8> {
            let mut result = Vec::new();
            // Blob byte
            result.push(0);
            self.just_bytes(&mut result);
            result
        }
    }


    #[derive(Debug)]
    pub struct ExpectedTyping {
        pub reference: Hash,
        pub kind: Hash,
    }

    #[derive(Debug)]
    pub struct MaybeValid {
        pub typing: Typing,
        pub prereqs: Vec<ExpectedTyping>,
    }

    #[derive(Debug)]
    pub struct InvalidCustomTypingError(pub &'static str);
    pub fn validate_adt_instance_bytes(t: &ADTItem, bytes: &[u8]) -> Result<Vec<ExpectedTyping>, InvalidCustomTypingError> {
        Err(InvalidCustomTypingError("unimplemented"))
    }

    pub fn validate_adt_instance(t: &ADT, value: &ADTValue) -> Result<MaybeValid, InvalidCustomTypingError> {
        Ok(MaybeValid {
            typing: Typing {
                type_hash: t.hash(),
                data_hash: value.hash(),
            },
            prereqs: inner_validate_adt_instance(&t.value, value)?
        })
    }

    fn inner_validate_adt_instance(t: &ADTItem, value: &ADTValue) -> Result<Vec<ExpectedTyping>, InvalidCustomTypingError> {
        match t {
            ADTItem::Hash(t) => {
                match value {
                    ADTValue::Sum {..} => Err(InvalidCustomTypingError("Expected Hash, found Sum")),
                    ADTValue::Product(_) => Err(InvalidCustomTypingError("Expected Hash, found Product")),
                    ADTValue::Hash(v) => Ok(vec![ExpectedTyping {
                        reference: *v,
                        kind: *t,
                    }]),
                }
            }
            ADTItem::Sum(subs) => {
                match value {
                    ADTValue::Hash(_) => Err(InvalidCustomTypingError("Expected Sum, found Hash")),
                    ADTValue::Product(_) => Err(InvalidCustomTypingError("Expected Sum, found Product")),
                    ADTValue::Sum { kind, value: v } => {
                        if *kind as usize >= subs.len() {
                            Err(InvalidCustomTypingError("Sum variant tag out of range"))
                        } else {
                            inner_validate_adt_instance(&subs[*kind as usize], v)
                        }
                    },
                }
            }
            ADTItem::Product(field_types) => {
                match value {
                    ADTValue::Hash(_) => Err(InvalidCustomTypingError("Expected Hash, found Product")),
                    ADTValue::Sum {..} => Err(InvalidCustomTypingError("Expected Sum, found Product")),
                    ADTValue::Product(field_values) => {
                        if field_types.len() != field_values.len() {
                            Err(InvalidCustomTypingError("Wrong number of product field values"))
                        } else {
                            let num = field_types.len();
                            let mut hashes: Vec<ExpectedTyping> = Vec::with_capacity(num);
                            for i in 0..num {
                                let mut maybes = inner_validate_adt_instance(&field_types[i], &field_values[i])?;
                                hashes.append(&mut maybes);
                            }
                            Ok(hashes)
                        }
                    },
                }
            }
        }
    }
}

mod rkvstorage {
    use crate::storage::{Hash, Storable};
    use rkv::Value;
    use crate::error::Error;
    use crate::error::Error::*;

    pub struct Db<'a> {
        pub env: &'a rkv::Rkv,
        pub store: &'a rkv::SingleStore,
    }

    impl<'a> Db<'a> {
        pub fn put(&self, item: &impl Storable) -> Result<(), Error> {
            // FIXME - handle errors properly here
            let mut writer = self.env.write().unwrap();
            if let Err(e) = self.store.put(&mut writer, &item.hash(), &Value::Blob(&item.bytes())) {
                return Err(RkvError(e));
            }
            if let Err(e) = writer.commit() {
                return Err(RkvError(e));
            }
            Ok(())
        }

        pub fn get(&self, hash: Hash) -> Result<Option<Vec<u8>>, Error> {
            let reader = self.env.read().expect("reader");
            let r = self.store.get(&reader, &hash).map_err(|e| { RkvError(e) })?;
            match r {
                Some(Value::Blob(bytes)) => Ok(Some(bytes.into())),
                Some(_) => {
                    println!("Non-blob retrieved from store...");
                    Ok(None)
                }
                None => {
                    println!("Entry missing from store...");
                    Ok(None)
                }
            }
        }
    }
}

fn confirm_typing_legit(db: &rkvstorage::Db, kind: &ADT, value: &ADTValue) ->
    Result<Typing, Error>
{
    match typings::validate_adt_instance(kind, value) {
        Ok(maybe) => {
            for expected in maybe.prereqs {
                match db.get(expected.reference) {
                    Err(_) => {
                        return Err(TypingParseError("error getting for sub-field"))
                    },
                    Ok(None) => {
                        return Err(TypingParseError("sub-field referencing non-existant value"))
                    },
                    Ok(Some(bytes)) => {
                        Typing::decode(&bytes[..]).and_then(|typing| {
                            if typing.type_hash.0 == expected.kind.0 {
                                Ok(typing)
                            } else {
                                Err(TypingParseError("typing of wrong type as sub-field"))
                            }
                        })?;
                    },
                }
            }
            return Ok(maybe.typing)
        },
        Err(e) => Err(TypingParseError("Failed basic validation")),
    }

        /*
        Err(e) => println!("validation error: {:?}", e),
        Ok(maybe) => {
            db.put(&maybe.typing)?;
            println!("maybe: {:?}", maybe);
        },
    }
    */
}


fn main() -> Result<(), Box<dyn std::error::Error>>{
    // let args: Vec<String> = env::args().collect();
    // println!("{:?}", args);

    let arc = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(Path::new("/Users/aaron/dev/rkv/data"), Rkv::new)
        .unwrap();
    let env = arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let db = rkvstorage::Db {
        env: &env,
        store: &store,
    };

    let blob1 = storage::Blob {
        bytes: b"abc"[..].into(),
    };
    let blob2 = storage::Blob {
        bytes: b"xyz"[..].into(),
    };

    let t1 = Typing {
        type_hash: BLOB_TYPE_HASH,
        data_hash: blob1.hash(),
    };
    let t2 = Typing {
        type_hash: BLOB_TYPE_HASH,
        data_hash: blob2.hash(),
    };

    let mut uniq = [0; 16];
    uniq[0] = 254;
    uniq[15] = 239;
    let double_ref_type = ADT {
        uniqueness: uniq,
        value: ADTItem::Product(vec![
                    ADTItem::Hash(BLOB_TYPE_HASH),
                    ADTItem::Hash(BLOB_TYPE_HASH)
        ]),
    };

    let double_ref_typing = Typing {
        type_hash: ADT_TYPE_HASH,
        data_hash: double_ref_type.hash(),
    };

    let double_instance = ADTValue::Product(
        vec![ADTValue::Hash(t1.hash()), ADTValue::Hash(t2.hash())],
    );
    

    db.put(&blob1)?;
    db.put(&blob2)?;
    db.put(&t1)?;
    db.put(&t2)?;
    db.put(&double_ref_type)?;
    db.put(&double_ref_typing)?;
    db.put(&double_instance)?;

    let typing = confirm_typing_legit(&db, &double_ref_type, &double_instance)?;
    db.put(&typing)?;

    match db.get(typing.hash())? {
        None => println!("nah"),
        Some(bytes) => {
            println!("{}", bytes.len());
        },
    }

    Ok(())
}
