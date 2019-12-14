// use std::env;
// use num::{BigUint};
// use sha3::digest::generic_array::GenericArray;

// use hex_literal::hex;
// #[macro_use]
// extern crate hex_literal;

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
    #[derive(Debug)]
    pub struct InvalidADTParseError(pub &'static str);
    impl ADT {
        pub fn decode(bytes: &[u8]) -> Result<ADT, InvalidADTParseError> {
            if bytes.len() < MIN_ADT_SIZE {
                Err(InvalidADTParseError("too short overall"))
            } else {
                let (value, rest) = ADTItem::decode(&bytes[17..])?;
                if rest.len() == 0 {
                    let mut uniqueness = [0; 16];
                    uniqueness.copy_from_slice(&bytes[1..17]);
                    Ok(ADT { uniqueness, value })
                } else {
                    dbg!(rest);
                    Err(InvalidADTParseError("extra left over"))
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
        fn decode(bytes: &[u8]) -> Result<(ADTItem, &[u8]), InvalidADTParseError> {
            if bytes.len() == 0 {
                return Err(InvalidADTParseError("nothing available for item"));
            } else if bytes[0] > 2 {
                return Err(InvalidADTParseError("invalid item disambiguation byte"));
            } else if bytes[0] == 0 {
                if bytes.len() < 33 {
                    return Err(InvalidADTParseError("not long enough for hash"));
                } else {
                    return Ok((ADTItem::Hash(Hash::sure_from(&bytes[1..33])), &bytes[33..]));
                }
            } else if bytes[0] == 1 || bytes[0] == 2 {
                if bytes.len() < 9 {
                    return Err(InvalidADTParseError("not long enough for sum"));
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
            return Err(InvalidADTParseError("unknown item type"));
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
}

mod rkvstorage {
    use crate::storage::{Hash, Storable};
    use rkv::Value;
    use std::convert::From;
    use std::error::Error;
    use std::fmt::{Display,Debug};

    pub struct Db<'a> {
        pub env: &'a rkv::Rkv,
        pub store: &'a rkv::SingleStore,
    }

    struct DbError(rkv::error::StoreError);
    impl From<rkv::error::StoreError> for DbError {
        fn from(e: rkv::error::StoreError) -> Self {
            DbError(e)
        }
    }
    impl Display for DbError {
        fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            <rkv::error::StoreError as Display>::fmt(&self.0, formatter)
        }
    }
    impl Debug for DbError {
        fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            <rkv::error::StoreError as Debug>::fmt(&self.0, formatter)
        }
    }
    impl Error for DbError { }

    impl<'a> Db<'a> {
        // pub fn new() -> Db {
        //     let arc = Manager::singleton().write().unwrap().get_or_create(Path::new("/Users/aaron/dev/rkv/data"), Rkv::new).unwrap();
        //     let env = arc.read().unwrap();
        //     let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();
        //     Db { arc, store }
        // }

        pub fn put(&self, item: &impl Storable) -> Result<(), impl Error> {
            // FIXME - handle errors properly here
            let mut writer = self.env.write().unwrap();
            self.store
                .put(&mut writer, &item.hash(), &Value::Blob(&item.bytes()))?;
            writer.commit()?;
            Ok(()) as Result<(), DbError>
        }

        pub fn get(&self, hash: Hash) -> Result<Option<Vec<u8>>, impl Error> {
            let reader = self.env.read().expect("reader");
            let r = self.store.get(&reader, &hash)?;
            match r {
                Some(Value::Blob(bytes)) => Ok(Some(bytes.into())),
                Some(_) => {
                    println!("Non-blob retrieved from store...");
                    Ok(None)
                }
                None => {
                    println!("Entry missing from store...");
                    Ok(None) as Result<Option<Vec<u8>>, DbError>
                }
            }
        }
    }
}

use crate::storage::Storable;
use rkv::{Manager, Rkv, SingleStore, StoreOptions};
use rkv::error::StoreError;
use std::path::Path;
use std::error::Error;
use std::fmt::{Debug,Display};

trait MyError: Debug + Display {}
impl Error for MyError {}
impl MyError for StoreError {}

fn main() -> Result<(), Box<dyn Error>>{
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

    let b = storage::Blob {
        bytes: b"abc"[..].into(),
    };

    let t = typings::Typing {
        type_hash: typings::BLOB_TYPE_HASH,
        data_hash: b.hash(),
    };

    // let t = Typing {
    //     type_hash: b.hash(),
    //     data_hash: b.hash(),
    // };

    // let mut uniq = [0; 16];
    // uniq[0] = 254;
    // uniq[15] = 239;
    // let t_blob = ADT {
    //     uniqueness: uniq,
    //     value: ADTItem::Hash(ADT_TYPE_HASH),
    // };
    // let t_blob = ADT {
    //     uniqueness: uniq,
    //     value: ADTItem::Product(vec![
    //                 ADTItem::Hash(BLOB_TYPE_HASH),
    //                 ADTItem::Hash(BLOB_TYPE_HASH)
    //     ]),
    // };

    // dbg!(&t_blob);


    {
        // Use a write transaction to mutate the store via a `Writer`.
        // There can be only one writer for a given environment, so opening
        // a second one will block until the first completes.
        // let mut writer = env.write().unwrap();

        db.put(&b)?;
        db.put(&t)?;
        // db.put(&t_blob);
        // put(&store, &mut writer, &b);
        // put(&store, &mut writer, &t_blob);
        // store.put(&mut writer, &b.hash(), &Value::Blob(&b.serialize()));
    }

    match db.get(b.hash()) {
        Ok(Some(bytes)) => {
            dbg!(bytes.len());
            // dbg!(bytes);
            // match ADT::decode(bytes) {
            //     Ok(adt) => { dbg!(adt); }
            //     Err(e) => { dbg!(e); }
            // };
        }
        Ok(None) => {
            println!("Attempted to fetch something not in store");
        }
        Err(e) => {
            println!("Error fetching from db, {}", e);
        }
    }

    match db.get(t.hash()) {
        Ok(Some(bytes)) => {
            dbg!(bytes.len());
        }
        Ok(None) => println!("Attempted to fetch something not in store"),
        Err(e) => println!("Error fetching from db, {}", e),
    }

    Ok(())
}

/*
pub struct BigValue {
    bytes: Vec<u8>,
}

impl BigValue {
    pub fn value(&self) -> &Value {
        Value::Blob(self.bytes.as_slice())
    }
}
*/

// struct InvalidHashLengthError(());
// impl TryFrom<&[u8]> for Hash {
//     type Error = InvalidHashLengthError;
//     fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
//         if value.len() == 32 {
//             let mut val = [0; 32];
//             val.copy_from_slice(value);
//             Ok(Hash(val))
//         } else {
//             Err(InvalidHashLengthError(()))
//         }
//     }
// }
