use std::fmt::Display;

use compact_str::CompactString;

use crate::runtime::Numeric;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub CompactString);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Expr),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Argument {
    pub name: Option<Identifier>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    StrLiteral {
        pieces: Vec<StrLiteralPiece>,
    },
    UnitLiteral,
    Numeric(Numeric),
    Variable(Identifier),
    UnaryExpr {
        expr: Box<Expr>,
        op: CompactString,
    },
    BinaryExpr {
        left: Box<Expr>,
        op: CompactString,
        right: Box<Expr>,
    },
    ListLiteral {
        elements: Vec<Expr>,
    },
    TupleLiteral {
        elements: Vec<Expr>,
    },
    // Index {
    //     expr: Box<Expr>,
    //     index: Box<Expr>,
    // },
    Invocation {
        expr: Box<Expr>,
        args: Vec<Argument>,
    },
    AnonymousFn {
        params: Vec<Identifier>,
        body: Block,
    },
    If {
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    },
    While {
        cond: Box<Expr>,
        body: Block,
    },
    DoWhile {
        body: Block,
        cond: Option<Box<Expr>>,
    },
    Loop {
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Item {
    NamedFn {
        name: Identifier,
        params: Vec<Identifier>,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Return { expr: Box<Expr> },
    Declare { id: Identifier, expr: Box<Expr> },
    Assign { id: Identifier, expr: Box<Expr> },
    Expr { expr: Box<Expr> }, // ...
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Block {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Document {
    pub body: Block,
}
