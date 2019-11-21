use std::path::Path;
use rkv::{Manager, Rkv, SingleStore, Value, StoreOptions};
use num::{BigUint};
use uuid::Uuid;


fn main() {
    let mut bytes = [0; 16];
    bytes[15] = 12;
    let s = &bytes;
    dbg!(s.len());
    let uuid = Uuid::from_slice(&bytes).unwrap();
    dbg!(uuid);
    return;

    dbg!(uuid::Uuid::new_v4());
    dbg!(uuid::Uuid::new_v4());
    let big: BigUint = BigUint::from(u64::max_value()) + 1u8;
    dbg!(u64::max_value());
    dbg!(big.to_str_radix(10));
    let created_arc = Manager::singleton().write().unwrap().get_or_create(Path::new("/Users/aaron/dev/rkv/data"), Rkv::new).unwrap();
    let env = created_arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let id = Uuid::new_v4();
    dbg!(id);

    {
        // Use a write transaction to mutate the store via a `Writer`.
        // There can be only one writer for a given environment, so opening
        // a second one will block until the first completes.
        let mut writer = env.write().unwrap();
        store.put(&mut writer, "int", &Value::I64(1234)).unwrap();
        store.put(&mut writer, "big", &Value::Blob(&big.to_bytes_be()));

        store.put(&mut writer, "uu", &Value::Blob(id.as_bytes()));

        writer.commit().unwrap();
    }

    {
        let reader = env.read().expect("reader");
        println!("Get int {:?}", store.get(&reader, "int").unwrap());
        println!("Get null {:?}", store.get(&reader, "nah").unwrap());
        let r = store.get(&reader, "uu");
        match r {
            Ok(Some(Value::Blob(blob))) => {
                dbg!(Uuid::from_slice(blob));
            }
            _ => {dbg!(r);}
        }
        // println!("Get uuid {:?}", store.get(&reader, id.as_bytes()).unwrap());
        let big = store.get(&reader, "big").unwrap();
        if let Some(Value::Blob(bytes)) = big {
            let big2 = BigUint::from_bytes_be(bytes);
            dbg!(big2.to_str_radix(10));
        }
        println!("Get big {:?}", store.get(&reader, "big").unwrap());
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
