// extern crate bridged_group;
use bridged_group::*;
// use bridged_group::bridged_group;
bridged_group! {
}

#[test]
fn thing() {
    let h = rf0::core::Hash::of(b"owl");
    dbg!(h);
    panic!();
}
