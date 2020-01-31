use std::collections::HashSet;
use std::collections::HashMap;

use crate::lang::AST;
use crate::error::MonsterError;
use crate::core::Hash;
use crate::types::TypeRef;
use crate::lang::TypeDef;
use crate::lang::TypeSpec;
use crate::types::RADTItem;
use crate::labels::LabelSet;
use crate::labels::Label;
use crate::labels::LabeledItem;

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
    let mut defined_names = Vec::<&'a str>::new();
    let mut conflicts = HashSet::<&'a str>::new();
    for d in defs {
        if defined_names.contains(&d.0) {
            conflicts.insert(&d.0);
        } else {
            defined_names.push(&d.0);
        }
    }
    if conflicts.len() > 0 {
        let names = conflicts.into_iter().map(str::to_owned).collect();
        return Err(MonsterError::ConflictingDefinitions(names));
    }

    let refs: HashMap<&'a str, usize> = defined_names.iter().enumerate().map(|(i,name)|(*name,i)).collect();
    let mut external_names = HashSet::<&'a str>::new();
    let mut hash_prefixes = HashSet::<&'a [u8]>::new();
    let mut builts: Vec<(&'a str, PendingItem<'a>)> = Vec::new();

    for d in defs {
        builts.push((&d.0, process_spec(&d.1, &refs, &mut external_names, &mut hash_prefixes)));
    }

    Ok(AlmostLabeledTypeDefinitions {
        names: external_names.into_iter().collect(),
        hash_prefixes: hash_prefixes.into_iter().collect(),
        defs: builts,
    })
}

fn process_spec<'a>(spec: &'a TypeSpec, refs: &HashMap<&str, usize>, ex_names: &mut HashSet<&'a str>, prefixes: &mut HashSet<&'a [u8]>) -> PendingItem<'a> {
    match spec {
        TypeSpec::Name(n) => {
            if let Some(idx) = refs.get(*n) {
                PendingItem::CycleRef(*idx)
            } else {
                ex_names.insert(*n);
                PendingItem::Name(*n)
            }
        },
        TypeSpec::Hash(h, idx) => PendingItem::ExternalType(TypeRef{definition:*h,item:*idx}),
        TypeSpec::ShortHash(pre, idx) => {
            prefixes.insert(pre);
            PendingItem::ShortHash(pre, *idx)
        },
        TypeSpec::Sum(variants) => {
            PendingItem::Sum(variants.into_iter().map(|(name, spec)| {
                (*name, process_spec(spec, refs, ex_names, prefixes))
            }).collect())
        },
        TypeSpec::Product(fields) => {
            PendingItem::Product(fields.into_iter().map(|(name, spec)| {
                (*name, process_spec(spec, refs, ex_names, prefixes))
            }).collect())
        },
        TypeSpec::Unit => PendingItem::Product(Vec::new()),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Env {
    // type -> labeling
    pub labelings: HashMap<Hash, LabelSet>,
    // name -> typed value
    pub variables: HashMap<String, Hash>,
}

use std::ops::Deref;
impl Env {
    pub fn defined_names(&self) -> HashSet<&str> {
        let mut names = HashSet::new();
        for set in self.labelings.values() {
            for label in set.0.iter() {
                names.insert(label.name.deref());
            }
        }

        for var in self.variables.keys() {
            names.insert(var);
        }
        names
    }

    pub fn name_resolutions(&self) -> HashMap<&str, TypeRef> {
        let mut result = HashMap::new();
        for (radt_hash,label_set) in self.labelings.iter() {
            for (i,label) in label_set.0.iter().enumerate() {
                result.insert(label.name.deref(), TypeRef {
                    definition: *radt_hash,
                    item: i,
                });
            }
        }
        result
    }

    pub fn name_of_typeref(&self, t: TypeRef) -> Option<&str> {
        if let Some(LabelSet(labels)) = self.labelings.get(&t.definition) {
            Some(&labels[t.item].name)
        } else {
            None
        }
    }
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

impl<'a> AlmostLabeledTypeDefinitions<'a> {
    pub fn defined_names(&self) -> Vec<&str> {
        self.defs.iter().map(|(n,_)| *n).collect()
    }
    pub fn resolve(&self, names: &HashMap<&str, TypeRef>, prefixes: &HashMap<&[u8], Hash>) -> Definitions {
        let mut types = Vec::with_capacity(self.defs.len());
        let mut labels = Vec::with_capacity(self.defs.len());
        
        for d in self.defs.iter() {
            let (t, l) = resolve_item(&d.1, names, prefixes);
            types.push(t);
            labels.push(Label { name: d.0.to_string(), item: l });
        }

        Definitions {
            types,
            labels: LabelSet(labels),
        }
    }
}

fn resolve_item(item: &PendingItem, names: &HashMap<&str, TypeRef>, prefixes: &HashMap<&[u8], Hash>) -> (RADTItem, LabeledItem) {
    match item {
        PendingItem::ExternalType(r) => (RADTItem::ExternalType(*r), LabeledItem::Type),
        PendingItem::CycleRef(idx) => (RADTItem::CycleRef(*idx), LabeledItem::Type),
        PendingItem::Name(n) => (RADTItem::ExternalType(*names.get(n).expect("All names should be resolved")), LabeledItem::Type),
        PendingItem::ShortHash(pre, idx) => (RADTItem::ExternalType(TypeRef{
                    definition: *prefixes.get(pre).expect("All short hashes should be resolved"),
                    item: *idx,
                }), LabeledItem::Type),
        PendingItem::Sum(variants) => {
            let (types, labels) = variants.iter().map(|(name, item)| {
                let (t, l) = resolve_item(item, names, prefixes);
                (t, Label { name: name.to_string(), item: l })
            }).unzip();
            (RADTItem::Sum(types), LabeledItem::Sum(labels))
        },
        PendingItem::Product(fields) => {
            let (types, labels) = fields.iter().map(|(name, item)| {
                let (t, l) = resolve_item(item, names, prefixes);
                (t, Label { name: name.to_string(), item: l })
            }).unzip();
            (RADTItem::Product(types), LabeledItem::Product(labels))
        },
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Definitions {
    pub types: Vec<RADTItem>,
    pub labels: LabelSet,
}

// #[derive(Debug, PartialEq, Eq)]
// pub struct 

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolution() {
        let prefix: Vec<u8> = vec![8, 255];

        let unresolved = AlmostLabeledTypeDefinitions {
            names: vec!["value"],
            hash_prefixes: vec![&prefix],
            defs: vec![
                ("one", PendingItem::CycleRef(1)),
                ("two", PendingItem::Product(Vec::new())),
                ("three", PendingItem::ExternalType( TypeRef {
                    definition: Hash::of(b"dog"),
                    item: 12,
                })),
                ("four", PendingItem::Name("value")),
                ("five", PendingItem::ShortHash(&prefix, 23)),
                ("six", PendingItem::Sum(vec![
                        ("yes", PendingItem::Name("value")),
                        ("no", PendingItem::ShortHash(&prefix, 44)),
                ])),
            ]
        };

        let names: HashMap<&str, TypeRef> = [
            ("value", TypeRef { definition: Hash::of(b"cat"), item: 9 }),
        ].iter().cloned().collect();
        let mut bytes = [0; 32];
        bytes[0] = 8;
        bytes[1] = 255;
        bytes[31] = 19;
        let prefixes: HashMap<&[u8], Hash> = [
            (prefix.as_ref(), Hash(bytes))
        ].iter().cloned().collect();

        let resolved = unresolved.resolve(&names, &prefixes);
        assert_eq!(resolved, Definitions {
            types: vec![
                RADTItem::CycleRef(1),
                RADTItem::Product(Vec::new()),
                RADTItem::ExternalType(TypeRef { definition: Hash::of(b"dog"), item: 12 }),
                RADTItem::ExternalType(TypeRef { definition: Hash::of(b"cat"), item: 9 }),
                RADTItem::ExternalType(TypeRef { definition: Hash(bytes.clone()), item: 23 }),
                RADTItem::Sum(vec![
                    RADTItem::ExternalType(TypeRef { definition: Hash::of(b"cat"), item: 9 }),
                    RADTItem::ExternalType(TypeRef { definition: Hash(bytes.clone()), item: 44 }),
                ]),
            ],
            labels: LabelSet(vec![
                Label { name: "one".to_owned(), item: LabeledItem::Type },
                Label { name: "two".to_owned(), item: LabeledItem::Product(Vec::new()) },
                Label { name: "three".to_owned(), item: LabeledItem::Type },
                Label { name: "four".to_owned(), item: LabeledItem::Type },
                Label { name: "five".to_owned(), item: LabeledItem::Type },
                Label { name: "six".to_owned(), item: LabeledItem::Sum(vec![
                        Label { name: "yes".to_owned(), item: LabeledItem::Type },
                        Label { name: "no".to_owned(), item: LabeledItem::Type },
                    ])
                },
            ]),
        });
    }

    #[test]
    fn test_definition_names() {
        let prefix: Vec<u8> = vec![8, 255];
        let evaled = AlmostLabeledTypeDefinitions {
            names: vec!["value"],
            hash_prefixes: vec![&prefix],
            defs: vec![
                ("one", PendingItem::CycleRef(1)),
                ("two", PendingItem::Product(Vec::new())),
                ("three", PendingItem::ExternalType( TypeRef {
                    definition: Hash::of(b"dog"),
                    item: 12,
                })),
                ("four", PendingItem::Name("value")),
                ("five", PendingItem::ShortHash(&prefix, 23)),
                ("six", PendingItem::Sum(vec![
                        ("yes", PendingItem::Name("value")),
                        ("no", PendingItem::ShortHash(&prefix, 44)),
                ])),
            ]
        };

        assert_eq!(evaled.defined_names(), vec!["one", "two", "three", "four", "five", "six"]);
    }

    #[test]
    fn test_definition_creation() {
        let prefix: Vec<u8> = vec![8,255];
        let defs = vec![
            TypeDef("one", TypeSpec::Name("two")),
            TypeDef("two", TypeSpec::Unit),
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
                ("one", PendingItem::CycleRef(1)),
                ("two", PendingItem::Product(Vec::new())),
                ("three", PendingItem::ExternalType( TypeRef {
                    definition: Hash::of(b"dog"),
                    item: 12,
                })),
                ("four", PendingItem::Name("value")),
                ("five", PendingItem::ShortHash(&prefix, 23)),
                ("six", PendingItem::Sum(vec![
                        ("yes", PendingItem::Name("value")),
                        ("no", PendingItem::ShortHash(&prefix, 44)),
                ])),
            ]
        };

        assert_eq!(definitions(&defs).unwrap(), expected);
    }

    #[test]
    fn test_err_duplicate_namings() {
        let defs = vec![
            TypeDef("thing", TypeSpec::Unit),
            TypeDef("thing", TypeSpec::Unit),
        ];
        match definitions(&defs) {
            Ok(_) => panic!("shouldn't succeed"),
            Err(e) => {
                assert_eq!(e.to_string(), "The following definitions were made multiple times: [\"thing\"]");
            }
        }
    }
}
