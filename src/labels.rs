use std::{
    fmt,
    fmt::{Display, Write},
};

use hex_literal::hex;
use indoc::indoc as dedent;

use crate::core::*;
use crate::error::*;
use crate::types::*;

#[derive(Debug,PartialEq,Eq)]
pub struct LabelSet(pub Vec<Label>);
#[derive(Debug,PartialEq,Eq)]
pub struct Label {
    pub name: String,
    pub item: LabeledItem,
}
#[derive(Debug,PartialEq,Eq)]
pub enum LabeledItem {
    Product(Vec<Label>),
    Sum(Vec<Label>),
    Type,
}

impl Display for LabelSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for label in self.0.iter() {
            if let LabeledItem::Product(ref v) = label.item {
                if v.len() == 0 {
                    writeln!(f, "{};", label.name)?;
                    continue;
                }
            }
            writeln!(f, "{} = {};", label.name, label.item)?;
        }
        Ok(())
    }
}

impl Display for LabeledItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LabeledItem::Type => write!(f, "#"),
            LabeledItem::Sum(ref vars) => {
                let mut first = true;
                write!(f, "(")?;
                for label in vars.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    if let LabeledItem::Product(ref v) = label.item {
                        if v.len() == 0 {
                            write!(f, "{}", label.name)?;
                        } else {
                            write!(f, "{} {}", label.name, label.item)?;
                        }
                    } else {
                        write!(f, "{} {}", label.name, label.item)?;
                    }
                }
                write!(f, ")")
            }
            LabeledItem::Product(ref fields) => {
                let mut first = true;
                write!(f, "{{")?;
                for label in fields.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", label.name, label.item)?;
                }
                write!(f, "}}")
            }
        }
    }
}

// write! returns a result, because writing to a stream may fail.
// But writing to a string won't fail, so don't bother with a bunch of error conversion boilerplate
#[allow(unused_must_use)]
fn print_with_labeling(t: &RADT, l: &LabelSet) -> Result<String, MonsterError> {
    let labels = &l.0;
    if t.items.len() != labels.len() {
        return Err(MonsterError::LabelingNumItemMismatch);
    }
    let mut result = String::new();
    for i in 0..t.items.len() {
        let item = &t.items[i];
        let label = &labels[i];
        match item {
            RADTItem::ExternalType(t) => {
                match label.item {
                    LabeledItem::Product(_) | LabeledItem::Sum(_) => {
                        return Err(MonsterError::LabelingKindMismatch)
                    }
                    LabeledItem::Type => writeln!(result, "{} = {};", label.name, t),
                };
            }
            RADTItem::CycleRef(idx) => {
                match label.item {
                    LabeledItem::Product(_) | LabeledItem::Sum(_) => {
                        return Err(MonsterError::LabelingKindMismatch)
                    }
                    LabeledItem::Type => {
                        writeln!(result, "{} = {};", label.name, labels[*idx].name)
                    }
                };
            }
            RADTItem::Product(ref v) if v.len() == 0 => {
                writeln!(result, "{};", label.name);
            }
            RADTItem::Sum(_) | RADTItem::Product(_) => {
                if let LabeledItem::Type = label.item {
                    return Err(MonsterError::LabelingKindMismatch);
                } else {
                    write!(result, "{} = ", label.name);
                    print_item_with_labeling(&mut result, &t.items, &labels, item, &label.item)?;
                    writeln!(result, ";");
                }
            }
        }
    }
    Ok(result)
}

// write! returns a result, because writing to a stream may fail.
// But writing to a string won't fail, so don't bother with a bunch of error conversion boilerplate
#[allow(unused_must_use)]
fn print_item_with_labeling(
    s: &mut String,
    base_items: &[RADTItem],
    base_labels: &[Label],
    item: &RADTItem,
    label_item: &LabeledItem,
) -> Result<(), MonsterError> {
    match item {
        RADTItem::ExternalType(t) => {
            if let LabeledItem::Type = label_item {
                write!(s, "{}", t);
            } else {
                return Err(MonsterError::LabelingKindMismatch);
            }
        }
        RADTItem::CycleRef(idx) => {
            if let LabeledItem::Type = label_item {
                write!(s, "{}", base_labels[*idx].name);
            } else {
                return Err(MonsterError::LabelingKindMismatch);
            }
        }
        RADTItem::Sum(variants) => {
            if let LabeledItem::Sum(var_labels) = label_item {
                if variants.len() != var_labels.len() {
                    return Err(MonsterError::LabelingSumVariantCountMismatch);
                }

                write!(s, "(");
                for i in 0..variants.len() {
                    if i > 0 {
                        write!(s, " | ");
                    }
                    match var_labels[i].item {
                        LabeledItem::Product(ref v) if v.len() == 0 => {
                            write!(s, "{}", var_labels[i].name);
                        }
                        _ => {
                            write!(s, "{} ", var_labels[i].name);
                            print_item_with_labeling(
                                s,
                                base_items,
                                base_labels,
                                &variants[i],
                                &var_labels[i].item,
                            )?;
                        }
                    }
                }
                write!(s, ")");
            } else {
                return Err(MonsterError::LabelingKindMismatch);
            };
        }
        RADTItem::Product(fields) => {
            if let LabeledItem::Product(field_labels) = label_item {
                if fields.len() != field_labels.len() {
                    return Err(MonsterError::LabelingProductFieldCountMismatch);
                }
                write!(s, "{{");
                for i in 0..fields.len() {
                    if i > 0 {
                        write!(s, ", ");
                    }
                    write!(s, "{}: ", field_labels[i].name);
                    print_item_with_labeling(
                        s,
                        base_items,
                        base_labels,
                        &fields[i],
                        &field_labels[i].item,
                    )?;
                }
                write!(s, "}}");
            } else {
                return Err(MonsterError::LabelingKindMismatch);
            }
        }
    }
    Ok(())
}

#[allow(unused_must_use)]
fn print_val_with_labeling(
    spec: &TypeSpec,
    LabelSet(labels): &LabelSet,
    value: &RADTValue,
) -> Result<String, MonsterError> {
    if spec.definition.items.len() != labels.len() {
        return Err(MonsterError::LabelingNumItemMismatch);
    }
    let mut s = format!("[{} ", labels[spec.item].name);
    inner_print_val_with_labeling(
        &mut s,
        &spec.definition.items,
        labels,
        spec.item(),
        &labels[spec.item].item,
        value,
    )?;
    write!(s, "]");

    Ok(s)
}

#[allow(unused_must_use)]
fn inner_print_val_with_labeling(
    w: &mut String,
    base_items: &[RADTItem],
    base_labels: &[Label],
    t: &RADTItem,
    l: &LabeledItem,
    v: &RADTValue,
) -> Result<(), MonsterError> {
    match (t, l, v) {
        (RADTItem::ExternalType(_), LabeledItem::Type, RADTValue::Hash(h)) => {
            write!(w, "{}", h);
            Ok(())
        }
        (RADTItem::CycleRef(i), LabeledItem::Type, _) => inner_print_val_with_labeling(
            w,
            base_items,
            base_labels,
            &base_items[*i],
            &base_labels[*i].item,
            v,
        ),
        (RADTItem::Sum(items), LabeledItem::Sum(labels), RADTValue::Sum { kind, value }) => {
            let kind = *kind as usize;
            if items.len() != labels.len() {
                return Err(MonsterError::LabelingSumVariantCountMismatch);
            }
            if kind >= items.len() {
                return Err(MonsterError::InvalidSumVariant(items.len(), kind));
            }
            write!(w, "{}", labels[kind].name);

            let mut newt = &items[kind];
            let mut lab = &labels[kind].item;

            while let RADTItem::CycleRef(i) = newt {
                let i = *i as usize;
                newt = &base_items[i];
                lab = &base_labels[i].item;
            }

            match (newt, lab, &**value) {
                (RADTItem::Product(ff), LabeledItem::Product(fff), RADTValue::Product(ffff))
                    if ff.len() == 0 && fff.len() == 0 && ffff.len() == 0 =>
                {
                    println!("empty");
                    Ok(())
                }
                (a, b, c) => {
                    write!(w, " ");
                    dbg!(a, b, c);
                    inner_print_val_with_labeling(
                        w,
                        base_items,
                        base_labels,
                        &items[kind],
                        &labels[kind].item,
                        value,
                    )
                }
            }
        }
        (
            RADTItem::Product(fields),
            LabeledItem::Product(field_labels),
            RADTValue::Product(field_values),
        ) => {
            if fields.len() != field_labels.len() || fields.len() != field_values.len() {
                dbg!((&fields, &field_labels, &field_values));
                return Err(MonsterError::NumFieldMismatch);
            }
            write!(w, "{{");
            for i in 0..fields.len() {
                if i > 0 {
                    write!(w, ", ");
                }
                write!(w, "{}: ", field_labels[i].name);
                inner_print_val_with_labeling(
                    w,
                    base_items,
                    base_labels,
                    &fields[i],
                    &field_labels[i].item,
                    &field_values[i],
                )?;
            }
            write!(w, "}}");
            Ok(())
        }
        _ => Err(MonsterError::LabelingKindMismatch),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_labeling_formatting() {
        let r = LabelSet(vec![
            Label {
                name: String::from("Nil"),
                item: LabeledItem::Product(Vec::new()),
            },
            Label {
                name: String::from("Cons"),
                item: LabeledItem::Product(vec![
                    Label {
                        name: String::from("head"),
                        item: LabeledItem::Sum(vec![
                            Label {
                                name: String::from("a"),
                                item: LabeledItem::Product(Vec::new()),
                            },
                            Label {
                                name: String::from("b"),
                                item: LabeledItem::Type,
                            },
                        ]),
                    },
                    Label {
                        name: String::from("tail"),
                        item: LabeledItem::Type,
                    },
                ]),
            },
            Label {
                name: String::from("List"),
                item: LabeledItem::Sum(vec![
                    Label {
                        name: String::from("Cons"),
                        item: LabeledItem::Product(Vec::new()),
                    },
                    Label {
                        name: String::from("Nil"),
                        item: LabeledItem::Product(Vec::new()),
                    },
                ]),
            },
        ]);

        assert_eq!(
            format!("{}", r),
            dedent!(
                "
                Nil;
                Cons = {head: (a | b #), tail: #};
                List = (Cons | Nil);
            "
            )
        );
    }

    #[test]
    fn test_print_instance_labeling() {
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
                        name: String::from("head"),
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

        let cafe = Hash(hex!(
            "cafebabe 12345678 12345678 12345678 12345678 12345678 12345678 12345678"
        ));
        let hash2 = Hash(hex!(
            "10011001 12345678 12345678 12345678 12345678 12345678 12345678 12345678"
        ));

        let v = RADTValue::Sum {
            kind: 0,
            value: Box::new(RADTValue::Product(vec![
                RADTValue::Hash(cafe),
                RADTValue::Sum {
                    kind: 0,
                    value: Box::new(RADTValue::Product(vec![
                        RADTValue::Hash(hash2),
                        RADTValue::Sum {
                            kind: 1,
                            value: Box::new(RADTValue::Product(Vec::new())),
                        },
                    ])),
                },
            ])),
        };

        assert_eq!(
            print_val_with_labeling(
                &TypeSpec {
                    definition: &t,
                    item: 2
                },
                &l,
                &v
            )
            .unwrap(),
            "[BlobList cons {head: #cafebabe, tail: cons {head: #10011001, tail: nil}}]"
        );
    }

    #[test]
    fn test_print_type_labeling() {
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

        assert_eq!(
            print_with_labeling(&t, &l).unwrap(),
            dedent!(
                "
        Nil;
        Cons = {head1: <blobref>, head2: <type>, head3: #01010101:12, tail: BlobList};
        BlobList = (cons Cons | nil Nil);
    "
            )
        );
    }
}
