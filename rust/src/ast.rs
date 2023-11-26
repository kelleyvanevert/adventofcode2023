use std::fmt::Display;

use crate::runtime::Numeric;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier<'a>(pub &'a str);

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum StrLiteralPiece<'a> {
    Fragment(&'a str),
    Interpolation(Expr<'a>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Argument<'a> {
    pub name: Option<Identifier<'a>>,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr<'a> {
    StrLiteral {
        pieces: Vec<StrLiteralPiece<'a>>,
    },
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
