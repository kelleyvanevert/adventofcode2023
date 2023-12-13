use std::{cmp::Ordering, collections::HashSet, fmt::Display};

use compact_str::CompactString;
use either::Either;

use crate::value::{AlRegex, Numeric};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Nil,
    Bool,
    Str,
    Numeric,
    Regex,
    FnDef,
    List(Box<Type>),
    Tuple,
    Dict,
    Union(Vec<Type>),
}

impl Type {
    fn flatten_unions(&self) -> Vec<Type> {
        match self {
            Type::Union(types) => types.into_iter().flat_map(Type::flatten_unions).collect(),
            _ => vec![self.clone()],
        }
    }

    fn canonicalize(&self) -> Type {
        match self {
            // does:
            // - flatten nested unions
            // - remove duplicates
            // - if single type -> return that single type
            Type::Union(_) => {
                let types = self
                    .flatten_unions()
                    .iter()
                    .map(Type::canonicalize)
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

                if types.len() == 1 {
                    types[0].clone()
                } else if types.contains(&Type::Any) {
                    Type::Any
                } else {
                    Type::Union(types)
                }
            }
            t => t.clone(),
        }
    }

    pub fn narrow(&self, other: &Type) -> Option<Type> {
        match self.partial_cmp(other) {
            None => None,
            Some(comparison) => match comparison {
                Ordering::Greater => Some(other.clone()),
                _ => Some(self.clone()),
            },
        }
    }
}

// lower = more specific, higher = less specific
// - Any is the highest i.e. least specific, it permits all types
// - Nil contains 1 element "nil" (it's basically unit, but, I call it differently)
// - Union([]) contains zero elements
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = self.canonicalize();
        let b = other.canonicalize();

        let simple = [
            Type::Nil,
            Type::Bool,
            Type::Numeric,
            Type::Str,
            Type::Tuple,
            Type::Regex,
            Type::Dict,
        ];

        match (&a, &b) {
            (Type::Any, Type::Any) => Some(Ordering::Equal),
            (Type::Any, _) => Some(Ordering::Greater),
            (_, Type::Any) => Some(Ordering::Less),

            (a, b) if simple.contains(a) & simple.contains(b) => {
                if a == b {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }

            (Type::List(_), b) if simple.contains(b) => None,
            (a, Type::List(_)) if simple.contains(a) => None,
            (Type::List(a), Type::List(b)) => a.partial_cmp(b),

            // TODO make fn types comparable as well
            (Type::FnDef, _) => None,
            (_, Type::FnDef) => None,

            (_, _) => {
                let a_types = a.flatten_unions();
                let b_types = b.flatten_unions();

                let mut a_gte_b = true;
                let mut b_gte_a = true;

                for b in &b_types {
                    // disprove that a >= b
                    if a_gte_b && !a_types.iter().any(|a| a >= b) {
                        a_gte_b = false;
                        println!("found b {} not in A {:?}", b, a_types);
                        if !b_gte_a {
                            return None;
                        }
                    }
                }

                for a in &a_types {
                    // try to find any other type that is not covered in my types
                    if b_gte_a && !b_types.iter().any(|b| b >= a) {
                        println!("found a {} not in B {:?}", a, b_types);
                        b_gte_a = false;
                        if !a_gte_b {
                            return None;
                        }
                    }
                }

                println!("res a >= b {a_gte_b}, b >= a {b_gte_a}");
                if a_gte_b && b_gte_a {
                    Some(Ordering::Equal)
                } else if a_gte_b {
                    Some(Ordering::Greater)
                } else if b_gte_a {
                    Some(Ordering::Less)
                } else {
                    None
                }
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "Any"),
            Type::Nil => write!(f, "Nil"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::Numeric => write!(f, "Numeric"),
            Type::Regex => write!(f, "Regex"),
            Type::FnDef => write!(f, "FnDef"),
            Type::List(t) => write!(f, "List[{t}]"),
            Type::Tuple => write!(f, "Tuple"),
            Type::Dict => write!(f, "Dict"),
            Type::Union(types) => {
                if types.len() == 0 {
                    write!(f, "!")
                } else {
                    write!(
                        f,
                        "{}",
                        types
                            .iter()
                            .map(|t| format!("{t}"))
                            .collect::<Vec<_>>()
                            .join(" | ")
                    )
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub CompactString);

impl<'a> From<&'a str> for Identifier {
    fn from(id: &'a str) -> Self {
        Identifier(id.into())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum DeclarePattern {
    Id(Identifier, Option<Type>),
    List {
        elements: Vec<DeclarePattern>,
        rest: Option<(Identifier, Option<Type>)>,
    },
    Tuple {
        elements: Vec<DeclarePattern>,
        rest: Option<(Identifier, Option<Type>)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignPattern {
    Id(Identifier),
    Index(Box<AssignPattern>, Option<Box<Expr>>),
    List {
        elements: Vec<AssignPattern>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to list literal exprs
    },
    Tuple {
        elements: Vec<AssignPattern>,
        // TODO maybe also add rest spread
        //   AFTER I also add rest spread to tuple literal exprs
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StrLiteralPiece {
    Fragment(String),
    Interpolation(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub name: Option<Identifier>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StrLiteral {
        pieces: Vec<StrLiteralPiece>,
    },
    NilLiteral,
    RegexLiteral {
        regex: AlRegex,
    },
    Bool(bool),
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
        splat: Option<Box<Expr>>,
    },
    TupleLiteral {
        elements: Vec<Expr>,
    },
    DictLiteral {
        elements: Vec<(Either<Identifier, Expr>, Expr)>,
    },
    Invocation {
        expr: Box<Expr>,
        args: Vec<Argument>,
    },
    AnonymousFn {
        params: Vec<DeclarePattern>,
        body: Block,
    },
    If {
        pattern: Option<DeclarePattern>,
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    },
    While {
        label: Option<Identifier>,
        cond: Box<Expr>,
        body: Block,
    },
    DoWhile {
        label: Option<Identifier>,
        body: Block,
        cond: Option<Box<Expr>>,
    },
    Loop {
        label: Option<Identifier>,
        body: Block,
    },
    For {
        label: Option<Identifier>,
        pattern: DeclarePattern,
        range: Box<Expr>,
        body: Block,
    },
}

impl From<AssignPattern> for Expr {
    fn from(pattern: AssignPattern) -> Self {
        match pattern {
            AssignPattern::Id(id) => Expr::Variable(id),
            AssignPattern::Index(box location, index_expr) => {
                let mut args = vec![Argument {
                    name: None,
                    expr: Expr::from(location),
                }];

                if let Some(box index_expr) = index_expr {
                    args.push(Argument {
                        name: None,
                        expr: index_expr,
                    });
                }

                Expr::Invocation {
                    expr: Expr::Variable(Identifier("index".into())).into(),
                    args,
                }
            }
            AssignPattern::List { elements } => Expr::ListLiteral {
                elements: elements.into_iter().map(Expr::from).collect(),
                splat: None,
            },
            AssignPattern::Tuple { elements } => Expr::TupleLiteral {
                elements: elements.into_iter().map(Expr::from).collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    NamedFn {
        name: Identifier,
        params: Vec<DeclarePattern>,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Break {
        expr: Option<Expr>,
    },
    Continue {
        label: Option<Identifier>,
    },
    Return {
        expr: Option<Expr>,
    },
    Declare {
        pattern: DeclarePattern,
        expr: Box<Expr>,
    },
    Assign {
        pattern: AssignPattern,
        expr: Box<Expr>,
    },
    Expr {
        expr: Box<Expr>,
    }, // ...
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Document {
    pub body: Block,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn types_po() {
        assert_eq!(Type::Bool.partial_cmp(&Type::Bool), Some(Ordering::Equal));

        assert_eq!(Type::Bool.partial_cmp(&Type::Nil), None);

        assert_eq!(Type::Any.partial_cmp(&Type::Bool), Some(Ordering::Greater));

        assert_eq!(
            Type::Union(vec![Type::Bool]).partial_cmp(&Type::Bool),
            Some(Ordering::Equal)
        );

        assert_eq!(
            Type::Union(vec![Type::Bool, Type::Nil]).partial_cmp(&Type::Bool),
            Some(Ordering::Greater)
        );

        assert_eq!(
            Type::Union(vec![Type::Bool, Type::FnDef]).partial_cmp(&Type::Bool),
            Some(Ordering::Greater)
        );

        assert_eq!(
            Type::Union(vec![
                Type::Bool,
                Type::List(Type::Union(vec![Type::Bool, Type::FnDef]).into())
            ])
            .partial_cmp(&Type::List(Type::Bool.into())),
            Some(Ordering::Greater)
        );
    }
}
