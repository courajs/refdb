use std::path::Path;
use std::convert::{TryFrom, TryInto};
use rkv::{Manager, Rkv, SingleStore, Value, StoreOptions};
use num::{BigUint};
use sha3::{Digest, Sha3_256};
use sha3::digest::generic_array::GenericArray;
use std::fmt;

#[macro_use] extern crate hex_literal;

#[derive(Copy,Clone)]
struct Hash([u8; 32]);
impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}
impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sha-256:{}", hex::encode(self.0))
    }
}

trait Storable {
    fn bytes(&self) -> Vec<u8>;
    fn hash(&self) -> Hash {
        let mut val: [u8; 32] = Default::default();
        let mut hasher = Sha3_256::new();
        hasher.input(&self.bytes());
        val.copy_from_slice(hasher.result().as_ref());
        Hash(val)
    }
}

struct Blob {
    bytes: Vec<u8>,
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

// Algebraic Data Type
// #[derive(Debug)]
struct ADT {
    uniqueness: [u8; 16],
    value: ADTItem,
}
impl fmt::Debug for ADT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ADT {{ \n    uniqueness: 0x{},\n    value: {:#?} }}", &hex::encode(self.uniqueness), &self.value)
    }
}

#[derive(Debug)]
enum ADTItem {
    Hash(Hash),
    Sum(Vec<ADTItem>),
    Product(Vec<ADTItem>),
}
impl ADTItem {
    fn bytes(&self) -> Vec<u8> {
        let mut result = Vec::new();
        match self {
            ADTItem::Hash(h) => {
                result.push(0);
                result.extend_from_slice(&h.0[..]);
                result
            }
            ADTItem::Sum(items) => {
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

static BlobTypeHash: Hash = Hash(hex!("00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"));
static ADTypeHash:   Hash = Hash(hex!("00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001"));

struct Typing {
    // Either this typing represents a type or a value.
    // If it represents a type, then type_hash will a special value
    // that means type (1 for adts, 2 for custom types, etc)
    // Otherwise, if it's a value, it will be the hash of a typing
    // which represents a type.
    // If this contained the hash of a blob, it would be invalid.
    type_hash: Hash,
    // the actual data blob
    data_hash: Hash,
    // 128 bits - like uuids.
    // This is for types to be more like nominal types and less like
    // structural types.
    // Maybe see https://www.unisonweb.org/docs/language-reference/type-declarations#unique-types
    // Presumably these would be generated randomly to minimize collision with other people's
    // types. 
    // I guess a reference to a specific type would basically be this whole struct (for when
    // exported.. internally it could just be the hash of this item.
    // But hash of this item only works if you know you have it in the database already.
    // uniqueness: [u8; 16],
}


fn put(store: &SingleStore, writer: &mut rkv::Writer, item: &impl Storable) -> Result<(), rkv::StoreError> {
    store.put(writer, &item.hash(), &Value::Blob(&item.bytes()))
}

fn main() {
    let created_arc = Manager::singleton().write().unwrap().get_or_create(Path::new("/Users/aaron/dev/rkv/data"), Rkv::new).unwrap();
    let env = created_arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let b = Blob { bytes: b"abc"[..].into() };

    let t = Typing {
        type_hash: b.hash(),
        data_hash: b.hash(),
    };

    let t_blob = ADT {
        uniqueness: [0; 16],
        value: ADTItem::Product(vec![
                    ADTItem::Hash(BlobTypeHash),
                    ADTItem::Hash(BlobTypeHash)
        ]),
    };

    dbg!(&t_blob);

    {
        // Use a write transaction to mutate the store via a `Writer`.
        // There can be only one writer for a given environment, so opening
        // a second one will block until the first completes.
        let mut writer = env.write().unwrap();

        put(&store, &mut writer, &b);
        put(&store, &mut writer, &t_blob);
        // store.put(&mut writer, &b.hash(), &Value::Blob(&b.serialize()));

        writer.commit().unwrap();
    }

    {
        let reader = env.read().expect("reader");
        if let Ok(Some(Value::Blob(bytes))) = store.get(&reader, &t_blob.hash()) {
            dbg!(bytes.len());
            dbg!(bytes);
            //println!("Got {}", std::str::from_utf8(bytes).unwrap());
        } else {
            println!("um.");
        }
    }
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
