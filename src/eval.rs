use std::collections::HashSet;
use std::collections::HashMap;

use crate::lang::AST;
use crate::error::*;
use crate::core::Hash;
use crate::types::TypeRef;
use crate::lang::{
    TypeSpec, TypeDef,
    ValueExpr, ValueItem,
    VariantDef, FieldsDef, ItemSpecifier, Literal,
};
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

use crate::types::*;
use crate::labels::*;
#[derive(Debug, Clone, PartialEq)]
pub struct LabeledRADT {
    pub uniqueness: [u8; 16],
    pub items: Vec<(String, LabeledRADTItem)>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum LabeledRADTItem {
    ExternalType(TypeRef),
    Sum(Vec<(String, LabeledRADTItem)>),
    Product(Vec<(String, LabeledRADTItem)>),
    CycleRef(usize),
}
impl LabeledRADT {
    fn new(radt: &RADT, labels: &LabelSet) -> LabeledRADT {
        Self::_new(radt, labels).expect("This label doesn't apply to this RADT")
    }
    fn _new(radt: &RADT, labels: &LabelSet) -> Result<LabeledRADT, ()> {
        fn make_item(base: &[RADTItem], base_labels: &[Label], item: &RADTItem, label: &LabeledItem) -> Result<LabeledRADTItem, ()> {
            match (item, label) {
                (RADTItem::CycleRef(idx),         LabeledItem::Type) => Ok(LabeledRADTItem::CycleRef(*idx)),
                (RADTItem::ExternalType(typeref), LabeledItem::Type) => Ok(LabeledRADTItem::ExternalType(typeref.clone())),
                (RADTItem::Sum(variants),         LabeledItem::Sum(variant_labels)) => {
                    if variants.len() != variant_labels.len() {
                        return Err(())
                    }
                    variants.iter().zip(variant_labels.iter()).try_fold(
                        Vec::with_capacity(variants.len()),
                        |mut v, (rad_item, label)| { v.push((label.name.clone(), make_item(base, base_labels, rad_item, &label.item)?)); Ok(v) }
                    ).map(LabeledRADTItem::Sum)
                },
                (RADTItem::Product(fields),     LabeledItem::Product(field_labels)) => {
                    if fields.len() != field_labels.len() {
                        return Err(())
                    }
                    fields.iter().zip(field_labels.iter()).try_fold(
                        Vec::with_capacity(fields.len()),
                        |mut v, (rad_item, label)| { v.push((label.name.clone(), make_item(base, base_labels, rad_item, &label.item)?)); Ok(v) }
                    ).map(LabeledRADTItem::Product)
                },
                _ => Err(()),
            }
        }

        if radt.items.len() != labels.0.len() {
            return Err(())
        }
        let items = radt.items.iter().zip(labels.0.iter()).try_fold(
            Vec::with_capacity(labels.0.len()),
            |mut v, (item, label)| { v.push((label.name.clone(), make_item(&radt.items, &labels.0, item, &label.item)?)); Ok(v) }
        );

        items.map(|items| LabeledRADT {
            uniqueness: radt.uniqueness.clone(),
            items,
        })
    }

    fn radt(&self) -> RADT {
        fn make_item(item: &LabeledRADTItem) -> RADTItem {
            match item {
                LabeledRADTItem::ExternalType(tr) => RADTItem::ExternalType(*tr),
                LabeledRADTItem::CycleRef(i) => RADTItem::CycleRef(*i),
                LabeledRADTItem::Sum(vars) => {
                    RADTItem::Sum(vars.iter().map(|(_,item)| make_item(item)).collect())
                },
                LabeledRADTItem::Product(fields) => {
                    RADTItem::Product(fields.iter().map(|(_,item)| make_item(item)).collect())
                },
            }
        }
        RADT {
            uniqueness: self.uniqueness.clone(),
            items: self.items.iter().map(|(_,item)| make_item(item)).collect(),
        }
    }
}

pub fn validate_instantiate(
    expr: &ValueItem,
    kind: &LabeledRADT,
    item: usize,
    names: &HashMap<&str, Hash>,
    prefix_resolutions: &HashMap<&[u8], Hash>
) -> Result<(TypedValue, Vec<crate::storage::Item>, HashSet<ExpectedTyping>), MonsterError> {
    use crate::bridge::Bridged;
    let mut deps = Vec::new();
    let mut expects = HashSet::new();
    let (_, stringref) = String::radt();

    fn make_item (
        radt_item: &LabeledRADTItem,
        ast_item: &ValueItem,
        full_type: &LabeledRADT,
        names: &HashMap<&str, Hash>,
        prefix_resolutions: &HashMap<&[u8], Hash>,
        deps: &mut Vec<crate::storage::Item>,
        expects: &mut HashSet<ExpectedTyping>,
    ) -> Result<RADTValue, MonsterError> {
        match (radt_item, ast_item) {
            (LabeledRADTItem::ExternalType(stringref), ValueItem::Literal(Literal::String(s))) => {
                // make the string, add it to deps, and return a Hash to it
                todo!("literal");
            },
            (LabeledRADTItem::ExternalType(t), ValueItem::HashRef(name)) => {
                // add to expectations, return a Hash to it
                todo!("full hash ref");
            },
            (LabeledRADTItem::ExternalType(t), ValueItem::NameRef(name)) => {
                // resolve the name, add to expectations, return a Hash to it
                todo!("name ref");
            },
            (LabeledRADTItem::ExternalType(t), ValueItem::ShortHashRef(prefix)) => {
                // resolve the prefix, add to expectations, return a Hash to it
                todo!("short hash ref");
            },
            (LabeledRADTItem::Product(fields), ValueItem::Unit) => {
                // ensure fields is empty, then return an empty product
                todo!("product (unit)");
            },
            (LabeledRADTItem::Sum(type_variants), ValueItem::Variant(value_variant)) => {
                // ensure a valid variant, recurse for the inner value
                let discriminant = match &value_variant.label {
                    ItemSpecifier::Index(i) => *i,
                    ItemSpecifier::Name(n) => {
                        find_specifier(*n, type_variants)?
                    },
                };
                if discriminant < type_variants.len() {
                    Ok(RADTValue::Sum {
                        kind: discriminant as u8,
                        value: Box::new(
                            make_item(&type_variants[discriminant].1, &value_variant.val, full_type, names, prefix_resolutions, deps, expects)?
                        ),
                    })
                } else {
                    Err(MonsterError::Todo("out of range variant discriminant"))
                }
            },
            (LabeledRADTItem::Product(type_fields), ValueItem::Fields(value_fields)) => {
                // ensure they're the same length,
                // resolve each name specifier,
                // recurse for each inner value
                if type_fields.len() != value_fields.len() {
                    return Err(MonsterError::Todo("wrong number of fields in product"));
                }
                let mut provided_items: Vec<(usize, &ValueItem)> = Vec::with_capacity(value_fields.len());
                for (spec, val) in value_fields.iter() {
                    match spec {
                        ItemSpecifier::Index(i) => provided_items.push((*i, val)),
                        ItemSpecifier::Name(n) => provided_items.push((find_specifier(*n, &type_fields)?, val)),
                    }
                }

                provided_items.sort_by_key(|i|i.0);
                for i in 0..provided_items.len() {
                    if i != provided_items[i].0 {
                        dbg!(provided_items);
                        return Err(MonsterError::Todo("field specified twice in product"))
                    }
                }


                let mut produced_items: Vec<RADTValue> = Vec::with_capacity(provided_items.len());
                for (i, inner) in provided_items {
                    produced_items.push(make_item(&type_fields[i].1, inner, full_type, names, prefix_resolutions, deps, expects)?);
                }
                Ok(RADTValue::Product(produced_items))
            },
            (LabeledRADTItem::CycleRef(idx), _) => {
                // recurse with the proper referenced radt_item
                make_item(&full_type.items[*idx].1, ast_item, full_type, names, prefix_resolutions, deps, expects)
            },
            _ => Err(MonsterError::Todo("thing doesn't match the type")),
        }
    }

    let value = make_item(&kind.items[item].1, expr, kind, names, prefix_resolutions, &mut deps, &mut expects)?;
    let typeref = kind.radt().item_ref(item);

    Ok((TypedValue { kind: typeref, value }, deps, expects))
}

fn find_specifier<O, Z: PartialEq + ?Sized, K: std::borrow::Borrow<Z>>(spec: &Z, values: &[(K, O)]) -> Result<usize, MonsterError> {
    values.iter().position(|(k,_)| spec == k.borrow()).ok_or(MonsterError::Todo("specifier not found"))
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

    #[test]
    fn foo() {
        
        let t = RADT {
            uniqueness: [0; 16],
            items: vec![
                // nil
                RADTItem::Product(Vec::new()),
                // cons
                RADTItem::Product(vec![
                    RADTItem::ExternalType(TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 0,
                    }),
                    RADTItem::ExternalType(TypeRef {
                        definition: RADT_TYPE_HASH,
                        item: 0,
                    }),
                    RADTItem::ExternalType(TypeRef {
                        definition: Hash([1; 32]),
                        item: 12,
                    }),
                    RADTItem::CycleRef(2),
                ]),
                // list
                RADTItem::Sum(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(0)]),
            ],
        };

        let l = LabelSet(vec![
            Label {
                name: String::from("Nil"),
                item: LabeledItem::Product(Vec::new()),
            },
            Label {
                name: String::from("Cons"),
                item: LabeledItem::Product(vec![
                    Label {
                        name: String::from("head1"),
                        item: LabeledItem::Type,
                    },
                    Label {
                        name: String::from("head2"),
                        item: LabeledItem::Type,
                    },
                    Label {
                        name: String::from("head3"),
                        item: LabeledItem::Type,
                    },
                    Label {
                        name: String::from("tail"),
                        item: LabeledItem::Type,
                    },
                ]),
            },
            Label {
                name: String::from("BlobList"),
                item: LabeledItem::Sum(vec![
                    Label {
                        name: String::from("cons"),
                        item: LabeledItem::Type,
                    },
                    Label {
                        name: String::from("nil"),
                        item: LabeledItem::Type,
                    },
                ]),
            },
        ]);

        let result = LabeledRADT::new(&t, &l);
        dbg!(&result);

        assert_eq!(result.radt(), t);
        // todo!();
    }

    #[test]
    fn test_eval_value_expr() {
        use LabeledRADTItem::*;
        use crate::bridge::Bridged;
        use hex_literal::hex;
        use crate::storage::*;
        let (_, string) = String::radt();
        let thingtype = TypeRef { definition: Hash::of(b"owl"), item: 3 };
        let input = "TypeRef (variantName {field1: thing, field2: #abcd, field0: {}, 4: (2), field3: (var2 \"stringval\")})";
        let kind = LabeledRADT {
            uniqueness: [0;16],
            items: vec![
                ("Nil".to_string(), Product(Vec::new())),
                ("Sum".to_string(), Sum(vec![
                    ("variantName".to_string(), CycleRef(2)),
                    ("var2".to_string(), ExternalType(string)),
                    ("by_number".to_string(), CycleRef(0)),
                ])),
                ("Product".to_string(), Product(vec![
                    ("field0".to_string(), CycleRef(0)),
                    ("field1".to_string(), ExternalType(thingtype)),
                    ("field2".to_string(), ExternalType(thingtype)),
                    ("field3".to_string(), CycleRef(1)),
                    ("also_by_number".to_string(), CycleRef(1)),
                ])),
            ],
        };
        let item = crate::lang::parse_value_expression(input).assert(input).val;

        let resolved_names: HashMap<&str, Hash> = [
            ("thing", Hash::of(b"dog")),
        ].iter().cloned().collect();
        let resolved_hashes: HashMap<&[u8], Hash> = [
            (&hex!("abcd")[..], Hash::of(b"cat")),
        ].iter().cloned().collect();

        let (string, mut deps) = String::from("stringval").to_value();
        let string_hash = string.typing().hash();
        deps.push(Item::Value(string));

        let expected_deps: HashSet<ExpectedTyping> = vec![
            ExpectedTyping {
                reference: Hash::of(b"dog"),
                kind: thingtype,
            },
            ExpectedTyping {
                reference: Hash::of(b"cat"),
                kind: thingtype,
            },
        ].into_iter().collect();

        // the value, the string dependency, and expected typings for thing and #abcd
        let expected = (
            TypedValue {
                kind: kind.radt().item_ref(1),
                value: RADTValue::Sum {
                    kind: 0,
                    value: Box::new(RADTValue::Product(vec![
                        RADTValue::Product(Vec::new()),
                        RADTValue::Hash(Hash::of(b"dog")),
                        RADTValue::Hash(Hash::of(b"cat")),
                        RADTValue::Sum {
                            kind: 1,
                            value: Box::new(RADTValue::Hash(string_hash)),
                        },
                        RADTValue::Sum {
                            kind: 2,
                            value: Box::new(RADTValue::Product(Vec::new())),
                        },
                    ])),
                },
            },
            deps,
            expected_deps,
        );

        // pub fn validate_instantiate(
        //     expr: &ValueItem,
        //     kind: &LabeledRADT,
        //     name: &HashMap<&str, Hash>,
        //     prefix_resolutions: &HashMap<&[u8], Hash>
        // ) -> Result<(TypedValue, Vec<TypedValue>, HashSet<ExpectedTyping>),
        //     MonsterError> {
        //
        assert_eq!(
            validate_instantiate(
                &item,
                &kind,
                1,
                &resolved_names,
                &resolved_hashes,
            ).expect("this should work"),
            expected,
        );
    }
}
