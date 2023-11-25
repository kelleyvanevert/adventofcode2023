use regex::Regex;

pub type ParseResult<I, O> = Option<(I, O)>;

pub trait Parser<I> {
    type Output;

    fn parse(&mut self, input: I) -> Option<(I, Self::Output)>;
}

impl<I, O, F> Parser<I> for F
where
    F: FnMut(I) -> Option<(I, O)>,
{
    type Output = O;

    fn parse(&mut self, input: I) -> Option<(I, Self::Output)> {
        self(input)
    }
}

pub struct TagParser<'t> {
    tag: &'t str,
}

pub fn tag<'t>(tag: &'t str) -> TagParser<'t> {
    TagParser { tag }
}

impl<'i, 't: 'i> Parser<&'i str> for TagParser<'t> {
    type Output = &'i str;

    fn parse(&mut self, input: &'i str) -> Option<(&'i str, Self::Output)> {
        if input.starts_with(self.tag) {
            Some((&input[self.tag.len()..], self.tag))
        } else {
            None
        }
    }
}

pub struct RegexParser {
    regex: Regex,
}

pub fn regex(str: &str) -> RegexParser {
    RegexParser {
        regex: Regex::new(str).unwrap(),
    }
}

impl<'i> Parser<&'i str> for RegexParser {
    type Output = &'i str;

    fn parse(&mut self, input: &'i str) -> Option<(&'i str, Self::Output)> {
        if let Some(m) = self.regex.find(input) {
            let found = &input[m.range()];
            Some((&input[found.len()..], found))
        } else {
            None
        }
    }
}

pub fn map<I, O1, O2>(
    mut p1: impl Parser<I, Output = O1>,
    mut f: impl FnMut(O1) -> O2,
) -> impl Parser<I, Output = O2> {
    move |input: I| {
        p1.parse(input).map(|(remaining, res)| {
            //
            (remaining, f(res))
        })
    }
}
