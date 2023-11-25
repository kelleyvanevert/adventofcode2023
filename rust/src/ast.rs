use std::marker::PhantomData;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier<'a>(pub &'a str);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrLiteralFragment<'a>(pub &'a str);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrLiteralInterpolation<'a>(pub &'a PhantomData<str>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StrLiteralPiece<'a> {
    Fragment(StrLiteralFragment<'a>),
    Interpolation(StrLiteralInterpolation<'a>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrLiteral<'a> {
    pub pieces: Vec<StrLiteralPiece<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str(String);

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Numeric {
    Int(i64),
    UInt(u64),
    Double(f64),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Argument<'a> {
    pub name: Option<Identifier<'a>>,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Expr<'a> {
    StrLiteral(StrLiteral<'a>),
    Str(Str),
    Numeric(Numeric),
    Variable(Identifier<'a>),
    UnaryExpr {
        expr: Box<Expr<'a>>,
        op: &'a str,
    },
    BinaryExpr {
        left: Box<Expr<'a>>,
        op: &'a str,
        right: Box<Expr<'a>>,
    },
    Invocation {
        expr: Box<Expr<'a>>,
        args: Vec<Argument<'a>>,
    },
    AnonymousFn {
        params: Vec<Identifier<'a>>,
        body: Vec<Stmt<'a>>,
    },
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Stmt<'a> {
    Assign {
        id: Identifier<'a>,
        expr: Box<Expr<'a>>,
    },
    Expr {
        expr: Box<Expr<'a>>,
    }, // ...
}
