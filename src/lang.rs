use nom::{
    IResult,
    error::VerboseError,
    sequence::delimited,
    branch::alt,
    bytes::complete::escaped_transform,
    character::complete::{
        anychar, char, one_of, none_of,
    },
    combinator::{
        not, map,
    },
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

fn parse_type(input: &str) -> IResult<&str, TypeSpec, VerboseError<&str>> {
    Ok(("", TypeSpec::Name("hey")))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_type_parsing() {
        let nil = "Nil";
        let short = "#facebabe_00552288:12";
        let long = "#cafebabe_12341234_cafebabe_12341234_cafebabe_12341234_cafebabe_12341234:3";

        let prefix = hex!("facebabe 00552288");
        let hash = hex!("cafebabe 12341234 cafebabe 12341234 cafebabe 12341234 cafebabe 12341234");

        let single_product = "{field: Value}";
        let multiple_product = "{key1: Value, key2: Value}";
        let sum = "(yes | no {reason: Text} | other Value)";

        assert_eq!(parse_type(nil).assert(nil), TypeSpec::Name("Nil"));
        // assert_eq!(parse_type(short).assert(short), TypeSpec::ShortHash(prefix.to_vec(), 12));
        // assert_eq!(parse_type(long).assert(long), TypeSpec::Hash(Hash(hash), 3));
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
