use std::collections::HashMap;

use nom::{
    IResult,
    error::VerboseError,
    sequence::{
        tuple,
        delimited,
        preceded,
        separated_pair,
    },
    branch::alt,
    bytes::complete::{
        take,
        take_while1,
        escaped_transform,
    },
    character::complete::{
        alphanumeric0,
        anychar, char,
        one_of, none_of,
        digit1, hex_digit1,
        multispace0, multispace1,
    },
    combinator::{
        not, map, opt, all_consuming,
    },
    multi::{
        many1,
        separated_list,
        separated_nonempty_list,
    },
};

use hex_literal::hex;
use indoc::indoc as dedent;

use crate::error::AssertParsed;
use crate::core::Hash;

type Parsed<'a> = IResult<&'a str, AST<'a>, VerboseError<&'a str>>;

#[derive(Debug, PartialEq, Eq)]
pub enum AST<'a> {
    String(String),
    Typedef(&'a str, TypeSpec<'a>),
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct TypeDef<'a>(pub &'a str, pub TypeSpec<'a>);

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

#[derive(Debug, Clone, PartialEq)]
pub enum ValueItem<'a> {
    Variant(VariantDef<'a>),
    Fields(FieldsDef<'a>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantDef<'a> {
    pub label: &'a str,
    pub val: Box<ValueItem<'a>>,
}
pub type FieldsDef<'a> = Vec<(&'a str, ValueItem<'a>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueAssignment<'a> {
    pub ident: &'a str,
    pub val: ValueExpr<'a>,
}

fn parse_value_assignment(input: &str) -> IResult<&str, ValueAssignment, VerboseError<&str>> {
    map(
        separated_pair(parse_identifier, squishy(char('=')), parse_value_expression),
        |(ident, val)| ValueAssignment { ident, val }
    )(input)
}

// TypeRef (variantName {fieldName: {}, fieldName2: (var2 "stringval")})
fn parse_value_expression(input: &str) -> IResult<&str, ValueExpr, VerboseError<&str>> {
    map(tuple((parse_typeref, multispace1, parse_value_item)), 
        |(kind, _, val)| ValueExpr { kind, val })(input)
}

fn parse_value_item(input: &str) -> IResult<&str, ValueItem, VerboseError<&str>> {
    alt((
        map(parse_variant, ValueItem::Variant),
        map(parse_fields, ValueItem::Fields),
        map(parse_literal, ValueItem::Literal),
    ))(input)
}

fn parse_variant(input: &str) -> IResult<&str, VariantDef, VerboseError<&str>> {
    map(tuple((
        char('('),
        multispace0,
        parse_identifier,
        multispace1,
        parse_value_item,
        multispace0,
        char(')'),
    )), |(_,_,label,_,val,_,_)| VariantDef { label, val: Box::new(val) })
    (input)
}
fn parse_fields(input: &str) -> IResult<&str, FieldsDef, VerboseError<&str>> {
    delimited(
        char('{'),
        separated_list(
            char(','),
            squishy(separated_pair(
                parse_identifier,
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

fn parse_typeref(input: &str) -> IResult<&str, TypeReference, VerboseError<&str>> {
    map(alt((
        parse_hash,
        parse_name,
    )),
    |hash_or_name| {
        match hash_or_name {
            TypeSpec::Name(n) => TypeReference::Name(n),
            TypeSpec::Hash(h, item) => TypeReference::Hash(h, item),
            TypeSpec::ShortHash(prefix, item) => TypeReference::ShortHash(prefix.clone(), item),
            _ => panic!("those parsers don't make anything else..."),
        }
    })(input)
}

fn parse_string(input: &str) -> IResult<&str, String, VerboseError<&str>> {
    delimited(
        char('"'),
        escaped_transform(none_of("\\\""), '\\', anychar),
        char('"'),
    )(input)
}

pub fn parse_ref(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    alt((
        parse_hash,
        parse_name,
    ))(input)
}

fn parse_type(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    alt((
        parse_hash,     // #abcd_1234:8
        parse_product,  // {key: List}
        parse_sum,      // (yes {} | no List)
        parse_name,     // List
    ))(input)
}
fn parse_name(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    map(parse_identifier, TypeSpec::Name)(input)
}
fn parse_identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '?' || c == '!' || c == '/')(input)
}


fn parse_hash(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    let (rest, (_,bytes,_,cycle)) = tuple((char('#'), hex_bytes, char(':'), decimal_integer))(input)?;
    // FIXME: error for hashes that are too long
    if bytes.len() == 32 {
        Ok((rest, TypeSpec::Hash(Hash::sure_from(&bytes), cycle)))
    } else {
        Ok((rest, TypeSpec::ShortHash(bytes, cycle)))
    }
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

pub fn parse_statements(input: &str) -> IResult<&str, Vec<TypeDef>, VerboseError<&str>> {
    all_consuming(delimited(
        multispace0,
        separated_list(
            squishy(char(';')),
            parse_type_definition,
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
        let input = "TypeRef (variantName {fieldName: {}, fieldName2: (var2 \"stringval\")})";
        let expected = ValueExpr {
            kind: TypeReference::Name("TypeRef"),
            val:  ValueItem::Variant( VariantDef {
                label: "variantName",
                val: Box::new(ValueItem::Fields([
                    ("fieldName", ValueItem::Fields(Vec::new())),
                    ("fieldName2", ValueItem::Variant(VariantDef {
                        label: "var2",
                        val: Box::new(ValueItem::Literal(Literal::String("stringval".to_owned()))),
                    })),
                ].iter().cloned().collect())),
            }),
        };

        assert_eq!(parse_value_expression(input), Ok(("", expected)));
    }
}
