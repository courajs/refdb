use std::{
    convert::TryInto,
    fmt,
    fmt::{Display, Write},
    path::Path,
};

use failure::{bail, Error, Fail};
use hex_literal::hex;
use indoc::indoc as dedent;
use lazy_static::lazy_static;
use nom::{
    bytes::complete::take,
    call,
    combinator::{all_consuming, map},
    length_count, map,
    number::complete::{be_u64, be_u8},
    sequence::tuple,
    switch, IResult,
};
use rkv::{Manager, Rkv, SingleStore, StoreOptions, Value};
use sha3::{Digest, Sha3_256};

    use crate::core::Hash;
    use crate::types::TypeRef;

    #[derive(Debug, Fail)]
    pub enum MonsterError {
        #[fail(display = "Expected a {}, found a {}", _0, _1)]
        Mismatch(&'static str, &'static str),
        #[fail(
            display = "Invalid sum variant. There are {} options, but found variant tag {}",
            _0, _1
        )]
        InvalidSumVariant(usize, usize),
        #[fail(
            display = "Invalid number of product fields. Expected {}, found {}",
            _0, _1
        )]
        InvalidProductFieldCount(usize, usize),
        #[fail(
            display = "Invalid cycle variant. There are {} options, but found reference to item {}",
            _0, _1
        )]
        InvalidCycleRef(usize, usize),
        #[fail(display = "Reached end of blob while parsing {}", _0)]
        Incomplete(&'static str),
        #[fail(
            display = "Excess data at end of blob. Finished parsing with {} bytes remaining out of {} total",
            _0, _1
        )]
        Excess(usize, usize),
        #[fail(display = "Error parsing {:?} from store: {:?}", _0, _1)]
        ParseError(Hash, String),
        #[fail(display = "RKV store error: {:?}", _0)]
        RkvError(#[cause] rkv::error::StoreError),
        #[fail(display = "Non-blob found in rkv store under hash {:?}", _0)]
        NonBlob(Hash),
        #[fail(display = "{:?} wasn't found in the store", _0)]
        NotFound(Hash),
        #[fail(display = "A type definition didn't point directly to bytes")]
        BrokenTypedef,
        #[fail(display = "A typing's type hash doesn't point to a type")]
        UntypedTyping,
        #[fail(
            display = "The typing {:?} couldn't be interpreted as a {:?}:\n{}",
            hash, target_type, err
        )]
        BrokenTyping {
            hash: Hash,
            target_type: TypeRef,
            err: String,
        },
        #[fail(display = "found blob instead of typing for sub-field ({:?})", _0)]
        UntypedReference(Hash),
        #[fail(
            display = "prereq {:?} is of wrong type. Expected {:?}, found {:?}",
            reference, expected_type, actual_type
        )]
        MistypedReference {
            reference: Hash,
            expected_type: TypeRef,
            actual_type: TypeRef,
        },
        #[fail(display = "labeling prob 1")]
        LabelingNumItemMismatch,
        #[fail(display = "labeling prob 2")]
        LabelingKindMismatch,
        #[fail(display = "labeling prob 3")]
        LabelingSumVariantCountMismatch,
        #[fail(display = "labeling prob 4")]
        LabelingProductFieldCountMismatch,
        #[fail(display = "labeling prob 5")]
        NumFieldMismatch,
    }
