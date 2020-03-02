extern crate bridged_group;
use bridged_group::bridged_group;

bridged_group! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Hey {
        x: u32,
        y: Vec<u32>,
    }

    enum Thing {
        // Yes(#[thing] u8, #[thing] u32),
    }
}

#[test]
fn test_basic_parsing() {
    //let h = Hey { x: 12 };
    //assert_eq!(h, Hey { x: 12});
    // panic!();
    dbg!(Hey::all());
    dbg!(Thing::all());
    panic!()
}
