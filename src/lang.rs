use std::collections::HashMap;

use nom::{
    IResult,
    error::VerboseError,
    sequence::{
        tuple,
        delimited,
        preceded,
        pair,
        separated_pair,
    },
    branch::alt,
    bytes::complete::{
        take,
        take_while1,
        escaped_transform,
    },
    character::complete::{
        alpha1,
        alphanumeric0, alphanumeric1,
        anychar, char,
        one_of, none_of,
        digit1, hex_digit1,
        multispace0, multispace1,
    },
    combinator::{
        not, map, opt, all_consuming, recognize,
        map_parser,
    },
    multi::{
        many0, many1,
        separated_list,
        separated_nonempty_list,
    },
};

use hex_literal::hex;
use indoc::indoc as dedent;

use crate::error::AssertParsed;
use crate::core::Hash;
use crate::error::*;

pub fn process_statements(input: &str) -> Result<(Vec<TypeDef>, Vec<ValueAssignment>), MonsterError> {
    match parse_statements(input).to_result(input) {
        Err(e) => Err(MonsterError::Formatted(e)),
        Ok(statements) => {
            let mut defs: Vec<TypeDef> = Vec::new();
            let mut assignments: Vec<ValueAssignment> = Vec::new();
            for s in statements {
                match s {
                    Statement::TypeDef(d) => defs.push(d),
                    Statement::Assignment(a) => assignments.push(a),
                }
            }
            Ok((defs, assignments))
        }
    }
}

type Parsed<'a> = IResult<&'a str, AST<'a>, VerboseError<&'a str>>;

#[derive(Debug, PartialEq, Eq)]
pub enum AST<'a> {
    String(String),
    Typedef(&'a str, TypeSpec<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSpec<'a> {
    // named type
    Name(&'a str),
    // full hash and cycle index
    Hash(Hash, usize),
    // hash prefix and cycle index
    ShortHash(Vec<u8>, usize),
    // Sum type - values will be one of a set of named variants
    Sum(Vec<(&'a str, TypeSpec<'a>)>),
    // Product type - a collection of named fields
    Product(Vec<(&'a str, TypeSpec<'a>)>),
    // product with no fields -
    // either a unit type or a sum variant with no child value
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDef<'a>(pub &'a str, pub TypeSpec<'a>);

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectReference<'a> {
    Ident(&'a str),
    Hash(Hash),
    ShortHash(Vec<u8>),
}
pub fn parse_object_ref(input: &str) -> IResult<&str, ObjectReference, VerboseError<&str>> {
    alt((
        map(parse_hash, |hash_ref| match hash_ref {
            HashRef::Full(h) => ObjectReference::Hash(h),
            HashRef::Prefix(pre) => ObjectReference::ShortHash(pre),
        }),
        map(parse_identifier, ObjectReference::Ident),
    ))(input)
}
pub fn as_object_ref(input: &str) -> Result<ObjectReference, MonsterError> {
    match all_consuming(parse_object_ref)(input) {
        Ok((_,val)) => Ok(val),
        Err(_) => Err(MonsterError::Todo("couldn't parse reference")),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueAssignment<'a> {
    pub ident: &'a str,
    pub val: ValueExpr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ValueExpr<'a> {
    pub kind: TypeReference<'a>,
    pub val: ValueItem<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeReference<'a> {
    // named type
    Name(&'a str),
    // full hash and cycle index
    Hash(Hash, usize),
    // hash prefix and cycle index
    ShortHash(Vec<u8>, usize),
}

impl<'a> TypeReference<'a> {
    fn to_spec(self) -> TypeSpec<'a> {
        match self {
            TypeReference::Name(n) => TypeSpec::Name(n),
            TypeReference::Hash(h, item) => TypeSpec::Hash(h, item),
            TypeReference::ShortHash(prefix, item) => TypeSpec::ShortHash(prefix, item),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueItem<'a> {
    Variant(VariantDef<'a>),
    Fields(FieldsDef<'a>),
    Literal(Literal),
    NameRef(&'a str),
    HashRef(Hash),
    ShortHashRef(Vec<u8>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemSpecifier<'a> {
    Name(&'a str),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantDef<'a> {
    pub label: ItemSpecifier<'a>,
    pub val: Box<ValueItem<'a>>,
}
pub type FieldsDef<'a> = Vec<(ItemSpecifier<'a>, ValueItem<'a>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
}

fn parse_value_assignment(input: &str) -> IResult<&str, ValueAssignment, VerboseError<&str>> {
    map(
        separated_pair(parse_identifier, squishy(char('=')), parse_value_expression),
        |(ident, val)| ValueAssignment { ident, val }
    )(input)
}

// TypeRef (variantName {fieldName: {}, fieldName2: (var2 "stringval")})
pub fn parse_value_expression(input: &str) -> IResult<&str, ValueExpr, VerboseError<&str>> {
    map(tuple((parse_typeref, multispace1, parse_value_item)), 
        |(kind, _, val)| ValueExpr { kind, val })(input)
}

fn parse_value_item(input: &str) -> IResult<&str, ValueItem, VerboseError<&str>> {
    alt((
        map(parse_variant, ValueItem::Variant),
        map(parse_fields, ValueItem::Fields),
        map(parse_literal, ValueItem::Literal),
        map(parse_identifier, ValueItem::NameRef),
        map(parse_hash, |hr| match hr {
            HashRef::Full(h) => ValueItem::HashRef(h),
            HashRef::Prefix(p) => ValueItem::ShortHashRef(p),
        }),
    ))(input)
}

fn parse_item_specifier(input: &str) -> IResult<&str, ItemSpecifier, VerboseError<&str>> {
    alt((
        map(decimal_integer, ItemSpecifier::Index),
        map(parse_identifier, ItemSpecifier::Name),
    ))(input)
}

fn parse_variant(input: &str) -> IResult<&str, VariantDef, VerboseError<&str>> {
    map(tuple((
        char('('),
        multispace0,
        parse_item_specifier,
        opt(preceded(multispace1, parse_value_item)),
        multispace0,
        char(')'),
    )), |(_,_,label,val,_,_)| match val {
        Some(val) => VariantDef { label, val: Box::new(val) },
        None => VariantDef { label, val: Box::new(ValueItem::Unit) },
    })
    (input)
}
fn parse_fields(input: &str) -> IResult<&str, FieldsDef, VerboseError<&str>> {
    delimited(
        char('{'),
        separated_list(
            char(','),
            squishy(separated_pair(
                parse_item_specifier,
                squishy(char(':')),
                parse_value_item,
            )),
        ),
        char('}'),
    )(input)
}
fn parse_literal(input: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(parse_string, Literal::String)(input)
}

pub fn parse_typeref(input: &str) -> IResult<&str, TypeReference, VerboseError<&str>> {
    alt((
        parse_hash_typeref,
        map(parse_identifier, TypeReference::Name),
    ))(input)
}

fn parse_string(input: &str) -> IResult<&str, String, VerboseError<&str>> {
    delimited(
        char('"'),
        escaped_transform(none_of("\\\""), '\\', anychar),
        char('"'),
    )(input)
}

#[derive(Clone, Debug, PartialEq)]
pub enum HashRef {
    Full(Hash),
    Prefix(Vec<u8>),
}

fn parse_type(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    alt((
        map(parse_hash_typeref, TypeReference::to_spec),     // #abcd_1234:8
        parse_product,  // {key: List}
        parse_sum,      // (yes {} | no List)
        map(parse_identifier, TypeSpec::Name),     // List
    ))(input)
}

// TODO: Cleanup
fn parse_identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(
        preceded(
            // initial character is alpha or underscore
            one_of("_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
            // alt((one_of("_"), alpha1)),
            // within identifiers we allow more things
            many0(one_of("_-?!/0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")),
        )
    )
    (input)
}

fn parse_hash_typeref(input: &str) -> IResult<&str, TypeReference, VerboseError<&str>> {
    map(
        separated_pair(parse_hash, char(':'), decimal_integer),
        |(h, cycle)| match h {
            HashRef::Full(h) => TypeReference::Hash(h, cycle),
            HashRef::Prefix(bytes) => TypeReference::ShortHash(bytes, cycle),
        }
    )(input)
}

fn parse_hash(input: &str) -> IResult<&str, HashRef, VerboseError<&str>> {
    map(preceded(char('#'), hex_bytes),
    |bytes| {
        if bytes.len() == 32 {
            HashRef::Full(Hash::sure_from(&bytes))
        } else {
            HashRef::Prefix(bytes)
        }
    })(input)
}


fn hex_bytes(input: &str) -> IResult<&str, Vec<u8>, VerboseError<&str>> {
    separated_nonempty_list(one_of("-_"), many1(hex_byte))(input).map(|(i, r)| (i, r.into_iter().flatten().collect()))
}


fn hex_byte(input: &str) -> IResult<&str, u8, VerboseError<&str>> {
    let (rest, digits) = take(2u8)(input)?;
    let (_, digits) = hex_digit1(digits)?;
    Ok((rest, u8::from_str_radix(digits, 16).unwrap()))
}
fn decimal_integer(input: &str) -> IResult<&str, usize, VerboseError<&str>> {
    let (rest, digits) = digit1(input)?;
    Ok((rest, usize::from_str_radix(digits, 10).unwrap()))
}


// TODO: parse error on duplicate keys?
fn parse_product(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    map(delimited(
            char('{'),
            squishy(separated_list(
                squishy(char(',')),
                map(tuple((parse_identifier, squishy(char(':')), parse_type)), |(n,_,t)| (n, t)),
            )),
            char('}')),
        |fields| TypeSpec::Product(fields))(input)
}

fn parse_sum(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    map(delimited(
            char('('),
            squishy(separated_list(
                squishy(char('|')),
                map(tuple((parse_identifier,multispace1,alt((parse_type,parse_empty)))), |(n,_,t)| (n, t)),
            )),
            char(')')),
        |fields| TypeSpec::Sum(fields))(input)
}
fn parse_empty(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    Ok((input, TypeSpec::Unit))
}

fn squishy<I,O,E>(f: impl Fn(I) -> IResult<I,O,E>) -> impl Fn(I) -> IResult<I,O,E>
    where
    E: nom::error::ParseError<I>,
    I: nom::InputTakeAtPosition,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone, 
{
    delimited(multispace0, f, multispace0)
}

pub fn parse_type_definition(input: &str) -> IResult<&str, TypeDef, VerboseError<&str>> {
    preceded(
        tuple((char('T'), multispace1)),
        alt((
            map(separated_pair(parse_identifier, squishy(char('=')), parse_type), |(name,t)| TypeDef(name, t)),
            map(parse_identifier, |name| TypeDef(name, TypeSpec::Unit)),
        ))
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    TypeDef(TypeDef<'a>),
    Assignment(ValueAssignment<'a>),
}

fn parse_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    alt((
        map(parse_type_definition, Statement::TypeDef),
        map(parse_value_assignment, Statement::Assignment),
    ))(input)
}

pub fn parse_statements(input: &str) -> IResult<&str, Vec<Statement>, VerboseError<&str>> {
    all_consuming(delimited(
        multispace0,
        separated_list(
            squishy(char(';')),
            parse_statement,
        ),
        squishy(opt(char(';'))),
    ))
    (input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_bytes() {
        assert_eq!(hex_bytes("12-ff00_04 "), Ok((" ", vec![18, 255, 0, 4])));
    }

    #[test]
    fn test_usize() {
        assert_eq!(decimal_integer("123 "), Ok((" ", 123usize)));
    }

    #[test]
    fn test_hex_byte() {
        assert_eq!(hex_byte("12ff"), Ok(("ff", 18)));
    }

    #[test]
    fn test_identifiers() {
        let input = "n4_me";
        assert_eq!(parse_identifier(input).assert(input), input);
        let input = "_private";
        assert_eq!(parse_identifier(input).assert(input), input);
        let input = "89";
        assert!(parse_identifier(input).is_err());
    }

    #[test]
    fn simple_type_parsing() {
        let nil = "Nil";
        let short = "#facebabe_00552288:12";
        let long = "#cafebabe12341234cafebabe12341234cafebabe12341234cafebabe12341234:3";
        // hyphens and underscores
        let long_seperated = "#cafebabe-12341234_cafebabe_12341234-cafebabe_12341234_cafebabe_12341234:3";

        let prefix = hex!("facebabe 00552288");
        let hash = hex!("cafebabe 12341234 cafebabe 12341234 cafebabe 12341234 cafebabe 12341234");

        let single_product = "{field: Value}";
        let multiple_product = "{key1: Value, key2: Value}";
        let sum = "(yes | no {reason: Text} | other Value)";

        assert_eq!(parse_type(nil).assert(nil), TypeSpec::Name("Nil"));
        assert_eq!(parse_type(short).assert(short), TypeSpec::ShortHash(prefix.to_vec(), 12));
        assert_eq!(parse_type(long).assert(long), TypeSpec::Hash(Hash(hash), 3));
        assert_eq!(parse_type(long_seperated).assert(long_seperated), TypeSpec::Hash(Hash(hash), 3));
        assert_eq!(parse_type("{}").assert("{}"), TypeSpec::Product(Vec::new()));
        assert_eq!(parse_type(single_product).assert(single_product), TypeSpec::Product(vec![("field", TypeSpec::Name("Value"))]));
        assert_eq!(parse_type(multiple_product).assert(multiple_product), TypeSpec::Product(vec![("key1", TypeSpec::Name("Value")), ("key2", TypeSpec::Name("Value"))]));
        assert_eq!(parse_type(sum).assert(sum), TypeSpec::Sum(vec![("yes", TypeSpec::Unit), ("no", TypeSpec::Product(vec![("reason", TypeSpec::Name("Text"))])), ("other", TypeSpec::Name("Value"))]));
    }

    #[test]
    fn test_string_parsing() {
        let input = r#"      "hey \"Alice\" and\\or \"Bob\""      "#.trim();
        let output = parse_string(input).assert(input);
        let expected = String::from("hey \"Alice\" and\\or \"Bob\"");

        assert_eq!(output, expected);
    }

    #[test]
    fn test_typedefs() {
        let input = "T Nil";
        let expected = TypeDef("Nil", TypeSpec::Unit);
        assert_eq!(parse_type_definition(input).assert(input), expected);

        let input = "T Cons = {head: Value, tail: List}";
        let expected = TypeDef("Cons", TypeSpec::Product(vec![
            ("head", TypeSpec::Name("Value")),
            ("tail", TypeSpec::Name("List")),
        ]));
        assert_eq!(parse_type_definition(input).assert(input), expected);

        let input = "T List = (cons Cons | nil Nil)";
        let expected = TypeDef("List", TypeSpec::Sum(vec![
            ("cons", TypeSpec::Name("Cons")),
            ("nil", TypeSpec::Name("Nil")),
        ]));
        assert_eq!(parse_type_definition(input).assert(input), expected);
    }

    #[test]
    fn test_value_expression() {
        let input = "TypeRef (variantName {field: {}, field2: thing, field3: #abcd, field4: (var2 \"stringval\"), 5: (2)})";
        let expected = ValueExpr {
            kind: TypeReference::Name("TypeRef"),
            val:  ValueItem::Variant( VariantDef {
                label: ItemSpecifier::Name("variantName"),
                val: Box::new(ValueItem::Fields([
                    (ItemSpecifier::Name("field"), ValueItem::Fields(Vec::new())),
                    (ItemSpecifier::Name("field2"), ValueItem::NameRef("thing")),
                    (ItemSpecifier::Name("field3"), ValueItem::ShortHashRef(hex!("abcd").to_vec())),
                    (ItemSpecifier::Name("field4"), ValueItem::Variant(VariantDef {
                        label: ItemSpecifier::Name("var2"),
                        val: Box::new(ValueItem::Literal(Literal::String("stringval".to_owned()))),
                    })),
                    (ItemSpecifier::Index(5), ValueItem::Variant(VariantDef {
                        label: ItemSpecifier::Index(2),
                        val: Box::new(ValueItem::Unit),
                    })),
                ].iter().cloned().collect())),
            }),
        };

        assert_eq!(parse_value_expression(input).assert(input), expected);
    }
}
