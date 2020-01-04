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

#[derive(Debug, PartialEq, Eq)]
pub enum AST {
    String(String),
}

fn parse(input: &str) -> IResult<&str, AST, VerboseError<&str>> {
    map(
        delimited(char('"'),
                  escaped_transform(none_of("\\\""), '\\', anychar),
                  char('"')),
    |s| AST::String(s))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_string() {
        let input = r#"      "hey \"Alice\" and\\or \"Bob\""      "#.trim();
        match parse(input) {
            Err(e) => {
                match e {
                    nom::Err::Incomplete(j) => println!("inc {:?}", j),
                    nom::Err::Error(e) => {
                        println!("{:?}", e);
                        println!("{}", nom::error::convert_error(&input, e))
                    },
                    nom::Err::Failure(e) => println!("{}", nom::error::convert_error(&input, e)),
                }
                panic!();
            },
            Ok((_, result)) => {
                assert_eq!(result, AST::String(String::from("hey \"Alice\" and\\or \"Bob\"")));
            }
        }
    }
}
