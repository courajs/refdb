use rf0::types::*;
use rf0::core::*;
use rf0::storage::Storable;
use rf0::bridge::*;
use bridged_group::*;

bridged_group! {
    #![bridged(uniq = *b"1234567812345678")]
    struct Thing;
}

#[test]
fn hello_bridged() {
    let r = RADT {
        uniqueness: *b"1234567812345678",
        items: vec![
            RADTItem::Product(Vec::new()),
        ],
    };
    let t = r.typing().hash();
    assert_eq!(Thing::radt(), (r, t));
}
