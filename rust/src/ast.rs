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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable<'a>(pub Identifier<'a>);

#[derive(Debug, PartialEq, PartialOrd)]
pub struct UnaryExpr<'a> {
    pub expr: Box<Expr<'a>>,
    pub op: &'a str,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct BinaryExpr<'a> {
    pub left: Box<Expr<'a>>,
    pub op: &'a str,
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Argument<'a> {
    pub name: Option<Identifier<'a>>,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ExprCall<'a> {
    pub expr: Box<Expr<'a>>,
    pub args: Vec<Argument<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct AnonymousFn<'a> {
    pub params: Vec<Identifier<'a>>,
    pub block: Stmt<'a>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Expr<'a> {
    StrLiteral(StrLiteral<'a>),
    Str(Str),
    Numeric(Numeric),
    Variable(Variable<'a>),
    UnaryExpr(UnaryExpr<'a>),
    BinaryExpr(BinaryExpr<'a>),
    ExprCall(ExprCall<'a>),
    AnonymousFn(AnonymousFn<'a>),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct AssignStmt<'a> {
    pub id: Identifier<'a>,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ExprStmt<'a> {
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Stmt<'a> {
    Assign(AssignStmt<'a>),
    Expr(ExprStmt<'a>),
    // ...
}
