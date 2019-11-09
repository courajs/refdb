use std::path::Path;
use rkv::{Manager, Rkv, SingleStore, Value, StoreOptions};


fn main() {
    let created_arc = Manager::singleton().write().unwrap().get_or_create(Path::new("/Users/aaron/dev/rkv-play/data"), Rkv::new).unwrap();
    let env = created_arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    {
        // Use a write transaction to mutate the store via a `Writer`.
        // There can be only one writer for a given environment, so opening
        // a second one will block until the first completes.
        let mut writer = env.write().unwrap();
        store.put(&mut writer, "int", &Value::I64(1234)).unwrap();
        writer.commit().unwrap();
    }

    {
        let reader = env.read().expect("reader");
        println!("Get int {:?}", store.get(&reader, "int").unwrap());
        println!("Get null {:?}", store.get(&reader, "nah").unwrap());
    }
}
