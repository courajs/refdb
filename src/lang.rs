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

use crate::error::AssertParsed;

type Parsed<'a> = IResult<&'a str, AST, VerboseError<&'a str>>;

#[derive(Debug, PartialEq, Eq)]
pub enum AST<'_> {
    String(String),
    TypeDecl(&str
}

fn parse_string(input: &str) -> Parsed {
    map(
        delimited(char('"'),
                  escaped_transform(none_of("\\\""), '\\', anychar),
                  char('"')),
    |s| AST::String(s))(input)
}

pub fn parse(input: &str) -> Parsed {
    parse_string(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_parsing() {
        let input = r#"      "hey \"Alice\" and\\or \"Bob\""      "#.trim();
        let output = parse_string(input).assert(input);
        let expected = AST::String(String::from("hey \"Alice\" and\\or \"Bob\""));

        assert_eq!(output, expected);
    }
}
