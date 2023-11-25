use crate::{
    ast::Identifier,
    parser_combinators::{map, regex, ParseResult, Parser},
};

pub fn identifier<'i>(input: &'i str) -> ParseResult<&'i str, Identifier<'i>> {
    map(regex("^[_a-zA-Z][_a-zA-Z0-9]*"), Identifier).parse(input)
}

#[cfg(test)]
mod tests {
    use crate::{ast::Identifier, parse::identifier};

    #[test]
    fn bla() {
        assert_eq!(identifier("kelley"), Some(("", Identifier("kelley"))));
        assert_eq!(identifier("_kel6*"), Some(("*", Identifier("_kel6"))));
        assert_eq!(identifier(" kelley"), None);
        assert_eq!(identifier(""), None);
    }
}
