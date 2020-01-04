use nom::{
    IResult,
    error::VerboseError,
    sequence::delimited,
    branch::alt,
    bytes::complete::escaped_transform,
    bytes::complete::take_while,
    character::complete::{
        alphanumeric0, anychar, char, one_of, none_of,
    },
    combinator::{
        not, map,
    },
    multi::many1,
};
use hex_literal::hex;

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
    // sum variant with no child value
    Empty,
}

fn parse_string(input: &str) -> Parsed {
    map(
        delimited(char('"'),
                  escaped_transform(none_of("\\\""), '\\', anychar),
                  char('"')),
    |s| AST::<'_>::String(s))(input)
}

pub fn parse(input: &str) -> Parsed {
    parse_string(input)
}

fn parse_type(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    alt((
        parse_hash,  // #abcd_1234:8
        parse_name,  // List
    ))(input)
}
fn parse_name(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    map(take_while(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '?' || c == '!' || c == '/'), |n| TypeSpec::Name(n))(input)
}

use nom::sequence::tuple;

fn parse_hash(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    let (rest, (_,bytes,_,cycle)) = tuple((char('#'), hex_bytes, char(':'), decimal_integer))(input)?;
    // FIXME: error for hashes that are too long
    if bytes.len() == 32 {
        Ok((rest, TypeSpec::Hash(Hash::sure_from(&bytes), cycle)))
    } else {
        Ok((rest, TypeSpec::ShortHash(bytes, cycle)))
    }
}

use nom::multi::separated_nonempty_list;

fn hex_bytes(input: &str) -> IResult<&str, Vec<u8>, VerboseError<&str>> {
    separated_nonempty_list(one_of("-_"), many1(hex_byte))(input).map(|(i, r)| (i, r.into_iter().flatten().collect()))
}

use nom::character::complete::hex_digit1;
use nom::character::complete::digit1;
use nom::bytes::complete::take;

fn hex_byte(input: &str) -> IResult<&str, u8, VerboseError<&str>> {
    let (rest, digits) = take(2u8)(input)?;
    let (_, digits) = hex_digit1(digits)?;
    Ok((rest, u8::from_str_radix(digits, 16).unwrap()))
}
fn decimal_integer(input: &str) -> IResult<&str, usize, VerboseError<&str>> {
    let (rest, digits) = digit1(input)?;
    Ok((rest, usize::from_str_radix(digits, 10).unwrap()))
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
        // assert_eq!(parse_type("{}").assert("{}"), TypeSpec::Product(Vec::new()));
        // assert_eq!(parse_type(single_product).assert(single_product), TypeSpec::Product(vec![("field", TypeSpec::Name("Value"))]));
        // assert_eq!(parse_type(multiple_product).assert(multiple_product), TypeSpec::Product(vec![("key1", TypeSpec::Name("Value")), ("key2", TypeSpec::Name("Value"))]));
        // assert_eq!(parse_type(sum).assert(sum), TypeSpec::Sum(vec![("yes", TypeSpec::Empty), ("no", TypeSpec::Product(vec![("reason", TypeSpec::Name("Text"))])), ("other", TypeSpec::Name("Value"))]));
    }

    #[test]
    fn test_string_parsing() {
        let input = r#"      "hey \"Alice\" and\\or \"Bob\""      "#.trim();
        let output = parse_string(input).assert(input);
        let expected = AST::String(String::from("hey \"Alice\" and\\or \"Bob\""));

        assert_eq!(output, expected);
    }
}
