#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier<'a>(pub &'a str);

#[derive(Debug, PartialEq, PartialOrd)]
pub enum StrLiteralPiece<'a> {
    Fragment(&'a str),
    Interpolation(Expr<'a>),
}

#[derive(Debug, PartialEq, PartialOrd)]
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
    If {
        cond: Box<Expr<'a>>,
        then: Vec<Stmt<'a>>,
        els: Option<Vec<Stmt<'a>>>,
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

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Document<'a> {
    pub body: Vec<Stmt<'a>>,
}
