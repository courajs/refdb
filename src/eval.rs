use std::collections::HashSet;

use crate::lang::AST;
use crate::error::MonsterError;
use crate::core::Hash;
use crate::types::TypeRef;
use crate::lang::TypeDef;
use crate::lang::TypeSpec;

// list of long hashes to confirm
// list of named types to resolve from env
// list of short hashes to expand and resolve
// list of new names
// the new types
// the new labelings
// so, we could just go through and find the list of things to resolve first, then
// later build up the types and labels once we have those.
// Or, we could build up the structure as well as the necessary resolutions,
// and have a lightweight way to slot in the resolutions.
//
// for that later resolution
// could use a private field with a RADT that we poke the refs into.
// but we would need to know the right places to do the swaps somehow.
// Pin<T>????
//
// Or we could use a whole parallel structure with custom types,
// that is easy to iterate over and construct a full one with the proper
// refs.
// Honestly types are't that big so this is probably totally fine
pub fn definitions<'a>(defs: &'a [TypeDef]) -> Result<AlmostLabeledTypeDefinitions<'a>, MonsterError> {
    let mut names = Vec::<&'a str>::new();
    let mut conflicts = HashSet::<&'a str>::new();
    for d in defs {
        if names.contains(&d.0) {
            conflicts.insert(&d.0);
        } else {
            names.push(&d.0);
        }
    }
    if conflicts.len() > 0 {
        let names = conflicts.into_iter().map(str::to_owned).collect();
        return Err(MonsterError::ConflictingDefinitions(names));
    }

    Err(MonsterError::ConflictingDefinitions(vec![String::from("justkidding")]))
}

#[derive(Debug, PartialEq, Eq)]
pub struct AlmostLabeledTypeDefinitions<'a>{
    pub names: Vec<&'a str>,
    pub hash_prefixes: Vec<&'a [u8]>,
    defs: Vec<(&'a str, PendingItem<'a>)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PendingItem<'a> {
    ExternalType(TypeRef),
    Sum(Vec<(&'a str, PendingItem<'a>)>),
    Product(Vec<(&'a str, PendingItem<'a>)>),
    CycleRef(usize),
    ShortHash(&'a [u8], usize),
    Name(&'a str),
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_definition_creation() {
        let prefix: Vec<u8> = vec![8,255];
        let defs = vec![
            TypeDef("one", TypeSpec::Empty),
            TypeDef("two", TypeSpec::Name("one")),
            TypeDef("three", TypeSpec::Hash(Hash::of(b"dog"), 12)),
            TypeDef("four", TypeSpec::Name("value")),
            TypeDef("five", TypeSpec::ShortHash(prefix.clone(), 23)),
            TypeDef("six", TypeSpec::Sum(vec![
                                ("yes", TypeSpec::Name("value")),
                                ("no", TypeSpec::ShortHash(prefix.clone(), 44)),
            ])),
        ];
        let expected = AlmostLabeledTypeDefinitions {
            names: vec!["value"],
            hash_prefixes: vec![&prefix],
            defs: vec![
                ("one", PendingItem::Product(Vec::new())),
                ("two", PendingItem::CycleRef(0)),
                ("three", PendingItem::ExternalType( TypeRef {
                    definition: Hash::of(b"dog"),
                    item: 12,
                })),
                ("four", PendingItem::Name("value")),
                ("five", PendingItem::ShortHash(&prefix, 23)),
                ("six", PendingItem::Sum(vec![
                        ("yes", PendingItem::Name("value")),
                        ("yes", PendingItem::ShortHash(&prefix, 44)),
                ])),
            ]
        };

        assert_eq!(definitions(&defs).unwrap(), expected);
    }

    #[test]
    fn test_err_duplicate_namings() {
        let defs = vec![
            TypeDef("thing", TypeSpec::Empty),
            TypeDef("thing", TypeSpec::Empty),
        ];
        match definitions(&defs) {
            Ok(_) => panic!("shouldn't succeed"),
            Err(e) => {
                assert_eq!(e.to_string(), "The following definitions were made multiple times: [\"thing\"]");
            }
        }
    }
}
