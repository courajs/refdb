use std::path::Path;
use std::convert::{TryFrom, TryInto};
use rkv::{Manager, Rkv, SingleStore, Value, StoreOptions};
use num::{BigUint};
use sha3::{Digest, Sha3_256};
use sha3::digest::generic_array::GenericArray;

#[macro_use] extern crate hex_literal;

struct Blob {
    bytes: Vec<u8>,
}

impl Blob {
    fn serialize(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(&self.bytes.capacity() + 1);
        v.push(0);
        v.extend_from_slice(&self.bytes);
        v
    }

    fn hash(&self) -> Hash {
        let mut val: [u8; 32] = Default::default();
        let mut hasher = Sha3_256::new();
        hasher.input(&[0]);
        hasher.input(&self.bytes);
        val.copy_from_slice(hasher.result().as_ref());
        Hash { val }
    }
}

#[derive(Clone)]
struct Hash {
    val: [u8; 32],
}

#[derive(Debug)]
pub struct HashLengthError(());

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.val
    }
}

/*
enum ADT {
    Hash(Hash),
    Sum(
}

struct ADT {

}
*/

static BlobTypeHash: Hash = Hash { val: hex!("00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000") };
static ADTypeHash:   Hash = Hash { val: hex!("00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001") };

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
    uniqueness: [u8; 16],
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

    {
        // Use a write transaction to mutate the store via a `Writer`.
        // There can be only one writer for a given environment, so opening
        // a second one will block until the first completes.
        let mut writer = env.write().unwrap();

        store.put(&mut writer, &b.hash(), &Value::Blob(&b.serialize()));

        writer.commit().unwrap();
    }

    {
        let reader = env.read().expect("reader");
        if let Ok(Some(Value::Blob(bytes))) = store.get(&reader, &b.hash()) {
            println!("Got {}", std::str::from_utf8(bytes).unwrap());
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
