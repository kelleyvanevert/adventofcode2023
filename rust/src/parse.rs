use crate::{
    ast::{Identifier, Numeric, StrLiteral, StrLiteralFragment, StrLiteralPiece},
    parser_combinators::{delimited, map, regex, seq, tag, ParseResult, Parser},
};

pub fn identifier<'i>(input: &'i str) -> ParseResult<&'i str, Identifier<'i>> {
    map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), Identifier).parse(input)
}

pub fn ws0<'i>(input: &'i str) -> ParseResult<&'i str, &'i str> {
    regex(r"^\s*").parse(input)
}

pub fn ws1<'i>(input: &'i str) -> ParseResult<&'i str, &'i str> {
    regex(r"^\s+").parse(input)
}

// TODO
pub fn str_literal<'i>(input: &'i str) -> ParseResult<&'i str, StrLiteral<'i>> {
    map(delimited(tag("\""), regex(r""), tag("\"")), |s| {
        StrLiteral {
            pieces: vec![StrLiteralPiece::Fragment(StrLiteralFragment(s))],
        }
    })
    .parse(input)
}

// TODO
pub fn numeric<'i>(input: &'i str) -> ParseResult<&'i str, Numeric> {
    map(regex(r"^[0-9]+"), |s| {
        Numeric::Int(s.parse::<i64>().unwrap())
    })
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Identifier,
        parse::identifier,
        parser_combinators::{alt, recognize, seq, tag, Parser},
    };

    #[test]
    fn bla() {
        assert_eq!(identifier.parse("kelley"), Some(("", Identifier("kelley"))));
        assert_eq!(identifier.parse("_kel6*"), Some(("*", Identifier("_kel6"))));
        assert_eq!(identifier.parse(" kelley"), None);
        assert_eq!(identifier.parse(""), None);

        assert_eq!(
            seq((ws0, identifier)).parse("kelley"),
            Some(("", ("", Identifier("kelley"))))
        );
        assert_eq!(seq((ws1, identifier)).parse("kelley"), None);
        assert_eq!(
            seq((ws1, identifier)).parse(" kelley"),
            Some(("", (" ", Identifier("kelley"))))
        );
        assert_eq!(
            recognize(seq((ws1, identifier, ws0))).parse(" kelley ?"),
            Some(("?", " kelley "))
        );
        assert_eq!(
            seq((ws1, identifier, ws1, identifier)).parse(" kelley  bla"),
            Some(("", (" ", Identifier("kelley"), "  ", Identifier("bla"))))
        );
        assert_eq!(alt((tag("blue"), tag("red"))).parse("  kelley"), None);
        assert_eq!(
            alt((tag("blue"), tag("red"), ws1)).parse("  kelley"),
            Some(("kelley", "  "))
        );
        assert_eq!(
            alt((tag("blue"), tag("red"), ws1)).parse("blue  kelley"),
            Some(("  kelley", "blue"))
        );
    }
}
