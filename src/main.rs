#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

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

pub mod error {
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
}

pub mod core {
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


    use crate::error::MonsterError;
    pub trait Serializable {
        fn bytes_into(&self, v: &mut Vec<u8>);

        fn bytes(&self) -> Vec<u8> {
            let mut v = Vec::new();
            self.bytes_into(&mut v);
            v
        }
    }

    pub trait Decodable: Sized {
        fn decode(bytes: &[u8]) -> IResult<&[u8], Self>;
    }

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Hash(pub [u8; 32]);
    impl AsRef<[u8]> for Hash {
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    impl Hash {
        pub fn sure_from(value: &[u8]) -> Hash {
            let mut val = [0; 32];
            val.copy_from_slice(value);
            Hash(val)
        }

        pub fn of(bytes: &[u8]) -> Hash {
            let mut hasher = Sha3_256::new();
            hasher.input(bytes);

            let mut val = [0; 32];
            val.copy_from_slice(hasher.result().as_ref());
            Hash(val)
        }
    }

    impl fmt::Debug for Hash {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "sha-256:{}", hex::encode(self.0))
        }
    }
    impl Display for Hash {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "#{}", hex::encode(&self.0[..4]))
        }
    }

    impl Serializable for Hash {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            v.extend_from_slice(&self.0);
        }
    }
    impl Decodable for Hash {
        fn decode(bytes: &[u8]) -> IResult<&[u8], Hash> {
            map(take(32u8), Hash::sure_from)(bytes)
        }
    }

    pub struct Blob {
        pub bytes: Vec<u8>,
    }

    impl fmt::Debug for Blob {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
            if self.bytes.len() <= 32 {
                write!(f, "Blob(len={})<0x", self.bytes.len())?;
                for byte in &self.bytes {
                    write!(f, "{:02x}", byte)?;
                }
                write!(f, ">")?;
            } else {
                write!(f, "Blob(len={})<0x", self.bytes.len())?;
                for byte in &self.bytes[..16] {
                    write!(f, "{:02x}", byte)?;
                }
                write!(f, " ... ")?;
                for byte in &self.bytes[self.bytes.len() - 16..] {
                    write!(f, "{:02x}", byte)?;
                }
                write!(f, ">")?;
            }
            Ok(())
        }
    }

    impl Serializable for Blob {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            v.extend_from_slice(&self.bytes);
        }
    }

    impl Decodable for Blob {
        fn decode(bytes: &[u8]) -> IResult<&[u8], Blob> {
            Ok((
                &[],
                Blob {
                    bytes: bytes.into(),
                },
            ))
        }
    }
}

pub mod storage {
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

    use crate::core::*;
    use crate::error::MonsterError;
    use crate::types::*;

    impl Storable for RADTValue {
        const PREFIX: u8 = 0;
    }

    impl Storable for RADT {
        const PREFIX: u8 = 0;
    }

    pub trait Storable: Serializable {
        const PREFIX: u8;

        fn all_bytes(&self) -> Vec<u8> {
            let mut v = Vec::new();
            v.push(Self::PREFIX);
            self.bytes_into(&mut v);
            v
        }

        fn hash(&self) -> Hash {
            Hash::of(&self.all_bytes())
        }
    }

    impl Storable for Blob {
        const PREFIX: u8 = 0;
    }

    impl Storable for Typing {
        const PREFIX: u8 = 1;
    }

    pub struct Db<'a> {
        pub env: &'a rkv::Rkv,
        pub store: &'a rkv::SingleStore,
    }

    #[derive(Debug)]
    pub enum Item {
        Blob(Blob),
        BlobRef(Hash),
        TypeDef(RADT),
        Value(TypeRef, RADTValue),
    }

    #[derive(Debug)]
    pub enum LiteralItem {
        Blob(Blob),
        Typing(Typing),
    }

    pub fn decode_item(bytes: &[u8]) -> IResult<&[u8], LiteralItem> {
        switch!(
            bytes,
            be_u8,
            0 => map!(call!(Blob::decode), |b| LiteralItem::Blob(b))
            | 1 => map!(call!(all_consuming(Typing::decode)), |t| LiteralItem::Typing(t))
        )
    }

    impl<'a> Db<'a> {
        pub fn put(&self, item: &impl Storable) -> Result<Hash, MonsterError> {
            // FIXME - handle errors properly here
            let hash = item.hash();
            let mut writer = self.env.write().unwrap();
            self.store
                .put(&mut writer, &hash, &Value::Blob(&item.all_bytes()))
                .map_err(|e| MonsterError::RkvError(e))?;

            writer.commit().map_err(|e| MonsterError::RkvError(e))?;

            Ok(hash)
        }

        pub fn get_bytes(&self, hash: Hash) -> Result<Vec<u8>, MonsterError> {
            let reader = self.env.read().expect("reader");
            let r = self
                .store
                .get(&reader, &hash)
                .map_err(|e| MonsterError::RkvError(e))?;
            match r {
                Some(Value::Blob(bytes)) => Ok(bytes.into()),
                Some(_) => Err(MonsterError::NonBlob(hash)),
                None => Err(MonsterError::NotFound(hash)),
            }
        }

        pub fn get(&self, hash: Hash) -> Result<Item, MonsterError> {
            let bytes = self.get_bytes(hash)?;
            match decode_item(&bytes) {
                Err(e) => Err(MonsterError::ParseError(hash, format!("{:?}", e))),
                Ok((_, LiteralItem::Blob(b))) => Ok(Item::Blob(b)),
                Ok((_, LiteralItem::Typing(typing))) => {
                    if typing.kind.definition == BLOB_TYPE_HASH {
                        return Ok(Item::BlobRef(typing.data));
                    } else if typing.kind.definition == RADT_TYPE_HASH {
                        let definition_bytes = self.get_bytes(typing.data)?;
                        let definition_blob = match decode_item(&definition_bytes)
                            .map_err(|e| MonsterError::ParseError(hash, format!("{:?}", e)))?
                        {
                            (_, LiteralItem::Typing(_)) => Err(MonsterError::BrokenTypedef),
                            (_, LiteralItem::Blob(b)) => Ok(b),
                        }?;
                        let (_, def) = all_consuming(RADT::decode)(&definition_blob.bytes)
                            .map_err(|e| MonsterError::ParseError(hash, format!("{:?}", e)))?;
                        return Ok(Item::TypeDef(def));
                    } else {
                        match self.get(typing.kind.definition)? {
                            Item::Blob(_) | Item::BlobRef(_) | Item::Value(_, _) => {
                                Err(MonsterError::UntypedTyping)
                            }
                            Item::TypeDef(radt) => {
                                let instance_bytes = self.get_bytes(typing.data)?;
                                let instance = validate_radt_instance_bytes(
                                    &radt,
                                    typing.kind.item,
                                    &instance_bytes[1..],
                                )
                                .map_err(|e| {
                                    MonsterError::BrokenTyping {
                                        hash,
                                        target_type: typing.kind,
                                        err: format!("{}", e),
                                    }
                                })?;
                                Ok(Item::Value(typing.kind, instance))
                            }
                        }
                    }
                }
            }
        }

        pub fn confirm_typings(&self, typings: &[ExpectedTyping]) -> Result<(), Error> {
            for expected in typings {
                let bytes = self.get_bytes(expected.reference)?;
                match decode_item(&bytes[..]) {
                    Err(e) => {
                        Err(MonsterError::ParseError(
                            expected.reference,
                            format!("{:?}", e),
                        ))?;
                    }
                    Ok((_, LiteralItem::Blob(_))) => {
                        Err(MonsterError::UntypedReference(expected.reference))?;
                    }
                    Ok((_, LiteralItem::Typing(typing))) => {
                        if typing.kind != expected.kind {
                            Err(MonsterError::MistypedReference {
                                reference: expected.reference,
                                expected_type: expected.kind,
                                actual_type: typing.kind,
                            })?;
                        }
                    }
                }
            }
            Ok(())
        }
    }
}

pub mod types {
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


    use crate::core::*;
    use crate::error::MonsterError;

    pub const BLOB_TYPE_HASH: Hash = Hash(hex!(
        "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    ));
    pub const RADT_TYPE_HASH: Hash = Hash(hex!(
        "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000001"
    ));

    pub struct TypeSpec<'a> {
        pub definition: &'a RADT,
        pub item: usize,
    }

    impl TypeSpec<'_> {
        pub fn item(&self) -> &RADTItem {
            &self.definition.items[self.item]
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct TypeRef {
        pub definition: Hash,
        pub item: usize,
    }
    impl Serializable for TypeRef {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            v.extend_from_slice(&self.definition.0);
            v.extend_from_slice(&self.item.to_be_bytes());
        }
    }
    impl Decodable for TypeRef {
        fn decode(bytes: &[u8]) -> IResult<&[u8], TypeRef> {
            map(
                tuple((Hash::decode, usize::decode)),
                |(definition, item)| TypeRef { definition, item },
            )(bytes)
        }
    }

    impl Display for TypeRef {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.definition {
                BLOB_TYPE_HASH => write!(f, "<blobref>"),
                RADT_TYPE_HASH => write!(f, "<type>"),
                _ => {
                    write!(f, "#")?;
                    for c in &self.definition.0[..4] {
                        write!(f, "{:02x}", c)?;
                    }
                    write!(f, ":{}", self.item)
                }
            }
        }
    }

    impl Serializable for usize {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            v.extend_from_slice(&self.to_be_bytes());
        }
    }
    impl Decodable for usize {
        fn decode(bytes: &[u8]) -> IResult<&[u8], usize> {
            map(take(8u8), from_sure_be_bytes)(bytes)
        }
    }

    fn from_sure_be_bytes(bytes: &[u8]) -> usize {
        let mut a = [0; 8];
        a.copy_from_slice(bytes);
        usize::from_be_bytes(a)
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Typing {
        // Either this typing represents a type or a value.
        // If it represents a type, then type_hash will be a special value
        // that means type (1 for adts, 2 for custom types, etc)
        // Otherwise, if it's a value, it will be the hash of a typing
        // which represents a type.
        // If this contained the hash of a blob, it would be invalid.
        pub kind: TypeRef,
        // the actual data blob
        pub data: Hash,
    }

    impl Serializable for Typing {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            self.kind.bytes_into(v);
            self.data.bytes_into(v);
        }
    }

    impl Decodable for Typing {
        fn decode(bytes: &[u8]) -> IResult<&[u8], Typing> {
            map(tuple((TypeRef::decode, Hash::decode)), |(kind, data)| {
                Typing { kind, data }
            })(bytes)
        }
    }

    // same structure as Typing, but the Hash fields mean different things.
    // For typing, that's the hash of the blob containing the value. For this
    // one, it's a hash which should point to a typing of the same type.
    #[derive(Debug, PartialEq, Eq)]
    pub struct ExpectedTyping {
        pub reference: Hash,
        pub kind: TypeRef,
    }

    fn parse_hash(bytes: &[u8]) -> Result<Hash, MonsterError> {
        if bytes.len() >= 32 {
            Ok(Hash::sure_from(&bytes[..32]))
        } else {
            Err(MonsterError::Incomplete("a hash"))
        }
    }

    // recursive algebraic data type
    // allows cyclical references
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct RADT {
        pub uniqueness: [u8; 16],
        pub items: Vec<RADTItem>,
    }

    impl Serializable for RADT {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            v.extend_from_slice(&self.uniqueness);
            v.extend_from_slice(&self.items.len().to_be_bytes());
            for item in self.items.iter() {
                item.bytes_into(v);
            }
        }
    }

    impl Decodable for RADT {
        fn decode(bytes: &[u8]) -> IResult<&[u8], RADT> {
            let (more, uniq) = take(16u8)(bytes)?;
            let (rest, items) = length_count!(more, be_u64, RADTItem::decode)?;

            let mut uniqueness = [0; 16];
            uniqueness.copy_from_slice(uniq);
            Ok((rest, RADT { uniqueness, items }))
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum RADTItem {
        // Reference to a separate RADT - the hash and cycle index.
        ExternalType(TypeRef),
        Sum(Vec<RADTItem>),
        Product(Vec<RADTItem>),
        CycleRef(usize),
    }

    #[derive(Debug)]
    pub enum RADTValue {
        Hash(Hash),
        Sum { kind: u8, value: Box<RADTValue> },
        Product(Vec<RADTValue>),
    }

    impl Serializable for RADTValue {
        fn bytes_into(&self, v: &mut Vec<u8>) {
            match self {
                RADTValue::Hash(h) => v.extend_from_slice(&h.0[..]),
                RADTValue::Sum { kind, value } => {
                    v.push(*kind);
                    value.bytes_into(v);
                }
                RADTValue::Product(subs) => {
                    for sub in subs {
                        sub.bytes_into(v);
                    }
                }
            }
        }
    }

    // We don't need to return the expected typings here because we only interpret bytes as an instance
    // if there's already a typing, and we only make the typing if all the expected typings check out.
    pub fn validate_radt_instance_bytes(
        t: &RADT,
        idx: usize,
        bytes: &[u8],
    ) -> Result<RADTValue, MonsterError> {
        let (value, rest) = inner_validate_radt_instance_bytes(&t.items, &t.items[idx], bytes)?;
        if rest.len() > 0 {
            Err(MonsterError::Excess(rest.len(), bytes.len()))
        } else {
            Ok(value)
        }
    }

    pub fn inner_validate_radt_instance_bytes<'a, 'b>(
        base_items: &'a [RADTItem],
        t: &'a RADTItem,
        bytes: &'b [u8],
    ) -> Result<(RADTValue, &'b [u8]), MonsterError> {
        match t {
            RADTItem::ExternalType(_) => Ok((RADTValue::Hash(parse_hash(bytes)?), &bytes[32..])),
            RADTItem::Sum(variants) => {
                if bytes.len() == 0 {
                    return Err(MonsterError::Incomplete("a sum variant tag"));
                }
                let variant = bytes[0];
                if variant as usize >= variants.len() {
                    return Err(MonsterError::InvalidSumVariant(
                        variants.len(),
                        variant as usize,
                    ));
                }
                let (inner, rest) = inner_validate_radt_instance_bytes(
                    base_items,
                    &variants[variant as usize],
                    &bytes[1..],
                )?;
                Ok((
                    RADTValue::Sum {
                        kind: variant,
                        value: Box::new(inner),
                    },
                    rest,
                ))
            }
            RADTItem::Product(fields) => {
                let mut values = Vec::new();
                let mut rest = bytes;
                for field in fields {
                    let (val, more) = inner_validate_radt_instance_bytes(base_items, field, rest)?;
                    values.push(val);
                    rest = more;
                }
                Ok((RADTValue::Product(values), rest))
            }
            RADTItem::CycleRef(idx) => {
                inner_validate_radt_instance_bytes(base_items, &base_items[*idx], bytes)
            }
        }
    }

    pub fn validate_radt_instance(
        t: &RADT,
        index: usize,
        value: &RADTValue,
    ) -> Result<Vec<ExpectedTyping>, MonsterError> {
        if index >= t.items.len() {
            Err(MonsterError::InvalidCycleRef(t.items.len(), index))
        } else {
            inner_validate_radt_instance(&t.items, &t.items[index], value)
        }
    }

    fn inner_validate_radt_instance(
        base_items: &[RADTItem],
        current_item: &RADTItem,
        value: &RADTValue,
    ) -> Result<Vec<ExpectedTyping>, MonsterError> {
        match current_item {
            RADTItem::ExternalType(TypeRef {
                definition: def,
                item: idx,
            }) => match value {
                RADTValue::Sum { .. } => Err(MonsterError::Mismatch("hash", "sum")),
                RADTValue::Product(_) => Err(MonsterError::Mismatch("hash", "product")),
                RADTValue::Hash(val) => Ok(vec![ExpectedTyping {
                    reference: *val,
                    kind: TypeRef {
                        definition: *def,
                        item: *idx,
                    },
                }]),
            },
            RADTItem::Sum(subs) => match value {
                RADTValue::Hash(_) => Err(MonsterError::Mismatch("sum", "hash")),
                RADTValue::Product(_) => Err(MonsterError::Mismatch("sum", "product")),
                RADTValue::Sum { kind, value } => {
                    if (*kind as usize) >= subs.len() {
                        Err(MonsterError::InvalidSumVariant(subs.len(), *kind as usize))
                    } else {
                        inner_validate_radt_instance(base_items, &subs[*kind as usize], value)
                    }
                }
            },
            RADTItem::Product(subs) => match value {
                RADTValue::Hash(_) => Err(MonsterError::Mismatch("product", "hash")),
                RADTValue::Sum { .. } => Err(MonsterError::Mismatch("product", "sum")),
                RADTValue::Product(values) => {
                    if subs.len() != values.len() {
                        Err(MonsterError::InvalidProductFieldCount(
                            subs.len(),
                            values.len(),
                        ))
                    } else {
                        let mut results = Vec::new();
                        for i in 0..subs.len() {
                            let prereqs =
                                inner_validate_radt_instance(base_items, &subs[i], &values[i])?;
                            results.extend(prereqs);
                        }
                        Ok(results)
                    }
                }
            },
            RADTItem::CycleRef(idx) => {
                if *idx >= base_items.len() {
                    Err(MonsterError::InvalidCycleRef(base_items.len(), *idx))
                } else {
                    inner_validate_radt_instance(base_items, &base_items[*idx], value)
                }
            }
        }
    }

    fn radt_decode_external(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
        map(
            tuple((
                map(take(32u8), Hash::sure_from),
                map(take(8u8), |b: &[u8]| {
                    usize::from_be_bytes(b.try_into().unwrap())
                }),
            )),
            |(h, idx)| {
                RADTItem::ExternalType(TypeRef {
                    definition: h,
                    item: idx,
                })
            },
        )(bytes)
    }
    fn radt_decode_sum(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
        map(radt_decode_items, |items| RADTItem::Sum(items))(bytes)
    }
    fn radt_decode_product(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
        map(radt_decode_items, |items| RADTItem::Product(items))(bytes)
    }
    fn radt_decode_items(bytes: &[u8]) -> IResult<&[u8], Vec<RADTItem>> {
        length_count!(bytes, be_u64, RADTItem::decode)
    }
    fn radt_decode_cycle_ref(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
        let (rest, i) = be_u64(bytes)?;
        Ok((rest, RADTItem::CycleRef(i as usize)))
    }

    impl Decodable for RADTItem {
        fn decode(bytes: &[u8]) -> IResult<&[u8], RADTItem> {
            switch!(
                bytes,
                be_u8,
                0 => call!(radt_decode_external)
                | 1 => call!(radt_decode_sum)
                | 2 => call!(radt_decode_product)
                | 3 => call!(radt_decode_cycle_ref)
            )
        }
    }
    impl RADT {
        fn normalize(&mut self) -> Vec<usize> {
            // theoretically there should be a much better impl with no clones and simple swaps
            // but that is for another day
            let len = self.items.len();

            let mut sort_mapping: Vec<usize> = (0..len).collect();
            sort_mapping.sort_by_cached_key(|i| Hash::of(&self.items[*i].zero_bytes()));
            transpose(&mut sort_mapping);

            // dbg!(&sort_mapping);

            let orig = self.items.clone();
            for (i, item) in orig.into_iter().enumerate() {
                self.items[sort_mapping[i]] = item;
            }

            for item in self.items.iter_mut() {
                item.update_refs(&sort_mapping);
            }

            sort_mapping
        }
    }

    fn transpose(v: &mut Vec<usize>) {
        let copy = v.clone();
        for (i, val) in copy.into_iter().enumerate() {
            v[val] = i
        }
    }

    impl Serializable for RADTItem {
        fn bytes_into(&self, result: &mut Vec<u8>) {
            match self {
                RADTItem::ExternalType(TypeRef {
                    definition: h,
                    item: idx,
                }) => {
                    result.push(0);
                    result.extend_from_slice(&h.0[..]);
                    result.extend_from_slice(&idx.to_be_bytes());
                }
                RADTItem::Sum(items) => {
                    // TODO: maybe sort these somehow for easier structural comparison?
                    result.push(1);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        item.bytes_into(result);
                    }
                }
                RADTItem::Product(items) => {
                    result.push(2);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        item.bytes_into(result);
                    }
                }
                RADTItem::CycleRef(index) => {
                    result.push(3);
                    result.extend_from_slice(&index.to_be_bytes());
                }
            }
        }
    }

    impl RADTItem {
        fn zero_bytes(&self) -> Vec<u8> {
            let mut v = Vec::new();
            self.zero_bytes_into(&mut v);
            v
        }

        fn zero_bytes_into(&self, result: &mut Vec<u8>) {
            match self {
                RADTItem::ExternalType(TypeRef {
                    definition: h,
                    item: idx,
                }) => {
                    result.push(0);
                    result.extend_from_slice(&h.0[..]);
                    result.extend_from_slice(&idx.to_be_bytes());
                }
                RADTItem::Sum(items) => {
                    // TODO: maybe sort these somehow for easier structural comparison?
                    result.push(1);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        item.zero_bytes_into(result);
                    }
                }
                RADTItem::Product(items) => {
                    result.push(2);
                    result.extend_from_slice(&items.len().to_be_bytes());
                    for item in items {
                        item.zero_bytes_into(result);
                    }
                }
                // here's where the zeroing comes in
                RADTItem::CycleRef(_) => {
                    result.push(3);
                    result.extend_from_slice(&(0 as usize).to_be_bytes());
                }
            }
        }

        fn update_refs(&mut self, map: &[usize]) {
            match self {
                RADTItem::ExternalType(_) => {}
                RADTItem::CycleRef(n) => {
                    *n = map[*n];
                }
                RADTItem::Sum(items) => {
                    for sub in items {
                        sub.update_refs(map);
                    }
                }
                RADTItem::Product(items) => {
                    for sub in items {
                        sub.update_refs(map);
                    }
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_typeref_display() {
            let mut t = TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 0,
            };
            assert_eq!(t.to_string(), "<blobref>");

            t = TypeRef {
                definition: RADT_TYPE_HASH,
                item: 0,
            };
            assert_eq!(t.to_string(), "<type>");

            t = TypeRef {
                definition: Hash(hex!(
                    "ba5eba11 cafebabe 00000000 00000000 00000000 00000000 00000000 00000000"
                )),
                item: 22,
            };
            assert_eq!(t.to_string(), "#ba5eba11:22");

            t = TypeRef {
                definition: Hash(hex!(
                    "01010101 cafebabe 00000000 00000000 00000000 00000000 00000000 00000000"
                )),
                item: 12,
            };
            assert_eq!(t.to_string(), "#01010101:12");
        }

        #[test]
        fn test_validate() {
            let list = RADTItem::Sum(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(2)]);
            let nil = RADTItem::Product(Vec::new());
            let cons = RADTItem::Product(vec![
                RADTItem::ExternalType(TypeRef {
                    definition: BLOB_TYPE_HASH,
                    item: 12,
                }),
                RADTItem::CycleRef(0),
            ]);
            let blob_list = RADT {
                uniqueness: [0; 16],
                items: vec![list, nil, cons],
            };

            let value = RADTValue::Sum {
                kind: 1,
                value: Box::new(RADTValue::Product(vec![
                    RADTValue::Hash(RADT_TYPE_HASH),
                    RADTValue::Sum {
                        kind: 0,
                        value: Box::new(RADTValue::Product(Vec::new())),
                    },
                ])),
            };

            let prereqs = validate_radt_instance(&blob_list, 0, &value).expect("should validate");

            assert_eq!(
                prereqs,
                vec![ExpectedTyping {
                    kind: TypeRef {
                        definition: BLOB_TYPE_HASH,
                        item: 12,
                    },
                    reference: RADT_TYPE_HASH,
                }]
            );
        }

        #[test]
        fn test_radt_item_recode() {
            let item = RADTItem::Product(vec![RADTItem::CycleRef(12), RADTItem::CycleRef(12)]);
            let bytes = item.bytes();
            let (empty, item2) = RADTItem::decode(&bytes).expect("hey");
            assert_eq!(item, item2);
        }

        #[test]
        fn test_radt_recode() {
            let blob_list = RADT {
                uniqueness: [0; 16],
                items: vec![
                    RADTItem::Product(vec![]),
                    RADTItem::Product(vec![
                        RADTItem::ExternalType(TypeRef {
                            definition: BLOB_TYPE_HASH,
                            item: 0,
                        }),
                        RADTItem::CycleRef(0),
                    ]),
                ],
            };

            let bytes = blob_list.bytes();
            let (_, rehydrated) = RADT::decode(&bytes).expect("should parse the encoded radt");
            assert_eq!(rehydrated, blob_list);
        }

        #[test]
        fn test_normalize() {
            use RADTItem::*;
            // dbg!(hash_slice(&CycleRef(0).zero_bytes()));
            // > sha-256:53d4918ee44c2cb4ce8ba669bee35ff4f39b53e91bd79af80a841f63f8578faa
            // dbg!(hash_slice(&Product(vec![ExternalType(BLOB_TYPE_HASH, 0), CycleRef(2)]).zero_bytes()));
            // > sha-256:d785756371cd213bc86a7924489907934c5ff5f5f03568ac5deb457f80d2c196
            // dbg!(hash_slice(&Product(vec![CycleRef(0), ExternalType(BLOB_TYPE_HASH, 0)]).zero_bytes()));
            // > sha-256:089f61699620a1897360213f2f96626563bbb49c6c6235b32b9ac0c1f74bec16
            // dbg!(hash_slice(&Product(vec![CycleRef(1), CycleRef(2)]).zero_bytes()));
            // > sha-256:b5a18419a727b19bdcd967f99b0de7997da3646dd3afa878e43dd856249ad5db
            //
            // 2, 0, 3, 1

            let mut r = RADT {
                uniqueness: [0; 16],
                items: vec![
                    CycleRef(0),
                    Product(vec![
                        ExternalType(TypeRef {
                            definition: BLOB_TYPE_HASH,
                            item: 0,
                        }),
                        CycleRef(2),
                    ]),
                    Product(vec![
                        CycleRef(0),
                        ExternalType(TypeRef {
                            definition: BLOB_TYPE_HASH,
                            item: 0,
                        }),
                    ]),
                    Product(vec![CycleRef(1), CycleRef(2)]),
                ],
            };
            let expected = RADT {
                uniqueness: [0; 16],
                items: vec![
                    Product(vec![
                        CycleRef(1),
                        ExternalType(TypeRef {
                            definition: BLOB_TYPE_HASH,
                            item: 0,
                        }),
                    ]),
                    CycleRef(1),
                    Product(vec![CycleRef(3), CycleRef(0)]),
                    Product(vec![
                        ExternalType(TypeRef {
                            definition: BLOB_TYPE_HASH,
                            item: 0,
                        }),
                        CycleRef(0),
                    ]),
                ],
            };

            r.normalize();
            assert_eq!(r, expected);
        }

        #[test]
        fn test_radt_mapping() {
            let mut r = RADTItem::Product(vec![
                RADTItem::CycleRef(0),
                RADTItem::CycleRef(1),
                RADTItem::CycleRef(2),
                RADTItem::CycleRef(3),
            ]);

            let expected = RADTItem::Product(vec![
                RADTItem::CycleRef(2),
                RADTItem::CycleRef(1),
                RADTItem::CycleRef(3),
                RADTItem::CycleRef(0),
            ]);
            r.update_refs(&vec![2, 1, 3, 0]);
            assert_eq!(r, expected);
        }

        #[test]
        fn test_transpose() {
            let mut v = vec![3, 1, 0, 2];
            transpose(&mut v);
            assert_eq!(v, vec![2, 1, 3, 0])
        }
    }
}

pub mod labels {
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


    use crate::core::*;
    use crate::error::*;
    use crate::types::*;

    #[derive(Debug)]
    pub struct Labeling(pub Vec<Label>);
    #[derive(Debug)]
    pub struct Label {
        pub name: String,
        pub item: LabeledItem,
    }
    #[derive(Debug)]
    pub enum LabeledItem {
        Product(Vec<Label>),
        Sum(Vec<Label>),
        Type,
    }

    impl Display for Labeling {
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
    fn print_with_labeling(t: &RADT, l: &Labeling) -> Result<String, MonsterError> {
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
                        print_item_with_labeling(
                            &mut result,
                            &t.items,
                            &labels,
                            item,
                            &label.item,
                        )?;
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
        Labeling(labels): &Labeling,
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
                let val = &**value;

                while let RADTItem::CycleRef(i) = newt {
                    let i = *i as usize;
                    newt = &base_items[i];
                    lab = &base_labels[i].item;
                }

                match (newt, lab, &**value) {
                    (
                        RADTItem::Product(ff),
                        LabeledItem::Product(fff),
                        RADTValue::Product(ffff),
                    ) if ff.len() == 0 && fff.len() == 0 && ffff.len() == 0 => {
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
            let r = Labeling(vec![
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

            let l = Labeling(vec![
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

            let l = Labeling(vec![
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
}

use crate::core::*;
use crate::error::MonsterError;
use crate::labels::*;
use crate::storage::*;
use crate::types::*;

fn main() -> Result<(), Error> {
    // let args: Vec<String> = env::args().collect();
    // println!("{:?}", args);

    let arc = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(Path::new("/Users/aaron/dev/rf0/data"), Rkv::new)
        .unwrap();
    let env = arc.read().unwrap();
    let store: SingleStore = env.open_single("mydb", StoreOptions::create()).unwrap();

    let db = Db {
        env: &env,
        store: &store,
    };

    let blob1 = Blob {
        bytes: b"abc"[..].into(),
    };
    let blob2 = Blob {
        bytes: b"xyz"[..].into(),
    };

    let ref1 = Typing {
        kind: TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        },
        data: blob1.hash(),
    };
    let ref2 = Typing {
        kind: TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        },
        data: blob2.hash(),
    };

    let mut uniq = [0; 16];
    uniq[0] = 254;
    uniq[15] = 239;
    let double_ref_type = RADT {
        uniqueness: uniq,
        items: vec![RADTItem::Product(vec![
            RADTItem::ExternalType(TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 0,
            }),
            RADTItem::ExternalType(TypeRef {
                definition: BLOB_TYPE_HASH,
                item: 0,
            }),
        ])],
    };

    let double_ref_typedef = Typing {
        kind: TypeRef {
            definition: RADT_TYPE_HASH,
            item: 0,
        },
        data: double_ref_type.hash(),
    };

    db.put(&blob1)?;
    db.put(&blob2)?;
    db.put(&ref1)?;
    db.put(&ref2)?;
    db.put(&double_ref_type)?;
    db.put(&double_ref_typedef)?;

    let instance = RADTValue::Product(vec![
        RADTValue::Hash(ref1.hash()),
        RADTValue::Hash(ref2.hash()),
    ]);

    let confirmations = validate_radt_instance(&double_ref_type, 0, &instance)?;
    db.confirm_typings(&confirmations)?;

    let typing = Typing {
        kind: TypeRef {
            definition: double_ref_typedef.hash(),
            item: 0,
        },
        data: instance.hash(),
    };

    db.put(&instance)?;
    db.put(&typing)?;

    let utf8string = RADT {
        uniqueness: hex!("cafebabe ba5eba11 b01dface ca11ab1e"),
        items: vec![RADTItem::ExternalType(TypeRef {
            definition: BLOB_TYPE_HASH,
            item: 0,
        })],
    };
    let utf8typedef = Typing {
        kind: TypeRef {
            definition: RADT_TYPE_HASH,
            item: 0,
        },
        data: utf8string.hash(),
    };

    db.put(&utf8string)?;
    db.put(&utf8typedef)?;

    let utf8hash = utf8typedef.hash();
    let utf8 = TypeRef {
        definition: utf8hash,
        item: 0,
    };

    let mut defs = RADT {
        uniqueness: [0; 16],
        items: vec![
            // 0: nil
            RADTItem::Product(Vec::new()),
            // 1: single label
            RADTItem::Product(vec![
                // text label
                RADTItem::ExternalType(TypeRef {
                    definition: utf8hash,
                    item: 0,
                }),
                // item it's labeling. Either a deeper labeling, or nil when
                // the type bottoms out on an ExternalType
                RADTItem::CycleRef(6),
            ]),
            // 2: label list cons
            RADTItem::Product(vec![RADTItem::CycleRef(1), RADTItem::CycleRef(3)]),
            // 3: label list
            RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(2)]),
            // 4: product field labels
            RADTItem::CycleRef(3),
            // 5: variant names
            RADTItem::CycleRef(3),
            // 6: Single type labeling - product, sum, or nil if it refers to an instance of another
            // type
            RADTItem::Sum(vec![
                RADTItem::CycleRef(4),
                RADTItem::CycleRef(5),
                RADTItem::CycleRef(0),
            ]),
            // 7: item labelings cons
            RADTItem::Product(vec![RADTItem::CycleRef(6), RADTItem::CycleRef(8)]),
            // 8: item labels
            RADTItem::Sum(vec![RADTItem::CycleRef(0), RADTItem::CycleRef(7)]),
        ],
    };

    // Nil;
    // Cons = {head: (nah | bool (yes | no) | val _ | struct {val: _}), tail: List};
    // List = (Cons | Nil);

    /*

    let (typeref, val) = match db.get(typing.hash())? {
        Item::Value(t, v) => (t, v),
        _ => bail!("nah"),
    };

    dbg!(typeref);
    dbg!(val);
    */

    Ok(())
}
