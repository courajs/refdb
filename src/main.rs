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

    fn hash(&self) -> GenericArray<u8, <Sha3_256 as Digest>::OutputSize> {
        let mut hasher = Sha3_256::new();
        hasher.input(&[0]);
        hasher.input(&self.bytes);
        return hasher.result()
    }
}

#[derive(Clone)]
struct Hash {
    val: [u8; 32],
}

#[derive(Debug)]
pub struct HashLengthError(());

impl TryFrom<&[u8]> for Hash {
    type Error = HashLengthError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if let Ok(val) = bytes.try_into() {
            Ok(Hash { val })
        } else {
            Err(HashLengthError(()))
        }
    }
}


struct Typing {
    type_hash: Hash,
    data_hash: Hash,
}


fn main() {
    let created_arc = Manager::singleton().write().unwrap().get_or_create(Path::new("/Users/aaron/dev/rkv/data"), Rkv::new).unwrap();
    let env = created_arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let b = Blob { bytes: b"abc"[..].into() };

    let h: Hash = b.hash().as_ref().try_into().expect("round trip");

    let t = Typing {
        type_hash: h.clone(),
        data_hash: h,
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
