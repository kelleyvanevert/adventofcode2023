use crate::{
    ast::{Argument, Block, Document, Expr, Identifier, Item, Stmt, StrLiteralPiece},
    parser_combinators::{
        alt, delimited, many0, map, optional, preceded, regex, seq, tag, terminated, ParseResult,
        Parser,
    },
    runtime::Numeric,
};

pub fn identifier<'i>(input: &'i str) -> ParseResult<&'i str, Identifier<'i>> {
    let (input, id) = map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), Identifier).parse(input)?;

    if [
        "fn", "if", "else", "then", "while", "do", "for", "let", "loop",
    ]
    .contains(&id.0)
    {
        return None;
    }

    Some((input, id))
}

pub fn slws0<'i>(input: &'i str) -> ParseResult<&'i str, &'i str> {
    regex(r"^[ \t]*").parse(input)
}

pub fn ws0<'i>(input: &'i str) -> ParseResult<&'i str, &'i str> {
    regex(r"^\s*").parse(input)
}

pub fn ws1<'i>(input: &'i str) -> ParseResult<&'i str, &'i str> {
    regex(r"^\s+").parse(input)
}

pub fn eof<'i>(input: &'i str) -> ParseResult<&'i str, ()> {
    if input.len() == 0 {
        Some((input, ()))
    } else {
        None
    }
}

fn unescape<'i>(input: &'i str) -> String {
    input.replace("\\n", "\n")
}

pub fn str_literal<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        delimited(
            tag("\""),
            many0(alt((
                map(map(regex("^[^\"{]+"), unescape), StrLiteralPiece::Fragment),
                map(
                    seq((tag("{"), ws0, expr, ws0, tag("}"))),
                    |(_, _, expr, _, _)| StrLiteralPiece::Interpolation(expr),
                ),
            ))),
            tag("\""),
        ),
        |pieces| Expr::StrLiteral { pieces },
    )
    .parse(input)
}

// TODO
pub fn numeric<'i>(input: &'i str) -> ParseResult<&'i str, Numeric> {
    map(regex(r"^[0-9]+"), |s| {
        Numeric::Int(s.parse::<i64>().unwrap())
    })
    .parse(input)
}

pub fn anonymous_fn<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            optional(delimited(
                seq((tag("|"), ws0)),
                parameter_list,
                seq((ws0, tag("|"), ws0)),
            )),
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(params, body)| Expr::AnonymousFn {
            params: params.unwrap_or_else(|| vec![]),
            body,
        },
    )
    .parse(input)
}

pub fn if_expr<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            tag("if"),
            ws1,
            tag("("),
            ws0,
            expr,
            ws0,
            tag(")"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
            optional(delimited(
                seq((ws0, tag("else"), ws0, tag("{"), ws0)),
                block_contents,
                seq((ws0, tag("}"))),
            )),
        )),
        |(_, _, _, _, cond, _, _, _, then, els)| Expr::If {
            cond: cond.into(),
            then,
            els,
        },
    )
    .parse(input)
}

pub fn do_while_expr<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            tag("do"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
            optional(delimited(
                seq((ws0, tag("while"), ws1, tag("("), ws0)),
                expr,
                seq((ws0, tag(")"))),
            )),
        )),
        |(_, _, body, cond)| Expr::DoWhile {
            cond: cond.map(Box::new),
            body,
        },
    )
    .parse(input)
}

pub fn loop_expr<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            tag("loop"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(_, _, body)| Expr::Loop { body },
    )
    .parse(input)
}

pub fn while_expr<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            tag("while"),
            ws1,
            tag("("),
            ws0,
            expr,
            ws0,
            tag(")"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(_, _, _, _, cond, _, _, _, body)| Expr::While {
            cond: cond.into(),
            body,
        },
    )
    .parse(input)
}

pub fn expr_leaf<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    alt((
        do_while_expr,
        while_expr,
        loop_expr,
        map(identifier, Expr::Variable),
        map(numeric, Expr::Numeric),
        str_literal,
        anonymous_fn,
        delimited(seq((tag("("), ws0)), expr, seq((ws0, tag(")")))),
    ))
    .parse(input)
}

pub fn argument<'i>(input: &'i str) -> ParseResult<&'i str, Argument<'i>> {
    map(
        seq((
            optional(terminated(identifier, seq((ws0, tag("="), ws0)))),
            expr,
        )),
        |(name, expr)| Argument {
            name,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn parenthesized_args<'i>(input: &'i str) -> ParseResult<&'i str, Vec<Argument<'i>>> {
    map(
        seq((
            tag("("),
            ws0,
            optional(seq((
                argument,
                many0(preceded(seq((ws0, tag(","), ws0)), argument)),
                ws0,
                optional(tag(",")),
            ))),
            ws0,
            tag(")"),
        )),
        |(_, _, args, _, _)| match args {
            None => vec![],
            Some((first_arg, mut args, _, _)) => {
                args.insert(0, first_arg);
                args
            }
        },
    )
    .parse(input)
}

pub fn invocation_args<'i>(input: &'i str) -> ParseResult<&'i str, Vec<Argument<'i>>> {
    let trailing_anon_fn = map(anonymous_fn, |anon| Argument {
        name: None,
        expr: anon.into(),
    });

    if let Some((input, args)) = parenthesized_args.parse(input) {
        let mut seen_named_arg = false;
        for arg in &args {
            if seen_named_arg && arg.name.is_none() {
                // unnamed args cannot follow named args
                return None;
            } else if arg.name.is_some() {
                seen_named_arg = true;
            }
        }

        if let Some((input, arg)) = preceded(slws0, trailing_anon_fn).parse(input) {
            let mut args = args;
            args.push(arg);
            Some((input, args))
        } else {
            Some((input, args))
        }
    } else {
        map(trailing_anon_fn, |arg| vec![arg]).parse(input)
    }
}

pub fn expr_call_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((expr_leaf, many0(preceded(slws0, invocation_args)))),
        |(mut expr, invocations)| {
            for args in invocations {
                expr = Expr::Invocation {
                    expr: expr.into(),
                    args,
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn unary_expr_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((many0(terminated(tag("!"), ws0)), expr_call_stack)),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                expr = Expr::UnaryExpr {
                    expr: expr.into(),
                    op,
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn mul_expr_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            unary_expr_stack,
            many0(seq((ws0, alt((tag("*"), tag("/"))), ws0, unary_expr_stack))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op,
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn add_expr_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            mul_expr_stack,
            many0(seq((ws0, alt((tag("+"), tag("-"))), ws0, mul_expr_stack))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op,
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn infix_or_postfix_fn_call_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    enum Op<'i> {
        Unary(Identifier<'i>),
        Binary((Identifier<'i>, Expr<'i>)),
    }

    map(
        seq((
            add_expr_stack,
            many0(seq((
                alt((
                    map(preceded(seq((ws0, tag("."))), identifier), Op::Unary),
                    map(
                        seq((
                            preceded(seq((ws0, tag(":"))), identifier),
                            preceded(ws0, add_expr_stack),
                        )),
                        Op::Binary,
                    ),
                )),
                optional(preceded(ws0, invocation_args)),
            ))),
        )),
        |(mut expr, ops)| {
            for (op, additional_args) in ops {
                expr = match op {
                    Op::Unary(fn_name) => Expr::Invocation {
                        expr: Expr::Variable(fn_name).into(),
                        args: vec![
                            vec![Argument { name: None, expr }],
                            additional_args.unwrap_or(vec![]),
                        ]
                        .concat(),
                    },
                    Op::Binary((fn_name, right)) => Expr::Invocation {
                        expr: Expr::Variable(fn_name).into(),
                        args: vec![
                            vec![
                                Argument { name: None, expr },
                                Argument {
                                    name: None,
                                    expr: right.into(),
                                },
                            ],
                            additional_args.unwrap_or(vec![]),
                        ]
                        .concat(),
                    },
                };
            }
            expr
        },
    )
    .parse(input)
}

pub fn equ_expr_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            infix_or_postfix_fn_call_stack,
            many0(seq((
                ws0,
                alt((
                    tag("!="),
                    tag(">="),
                    tag("<="),
                    tag("=="),
                    tag("<"),
                    tag(">"),
                )),
                ws0,
                infix_or_postfix_fn_call_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op,
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn expr<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    alt((if_expr, equ_expr_stack)).parse(input)
}

pub fn parameter_list<'i>(mut input: &'i str) -> ParseResult<&'i str, Vec<Identifier<'i>>> {
    if let Some((rem, id)) = identifier.parse(input) {
        let mut ids = vec![];
        let mut seen_comma = false;

        ids.push(id);
        input = rem;

        loop {
            if seen_comma && let Some((rem, id)) = preceded(ws0, identifier).parse(input) {
                ids.push(id);
                input = rem;
                seen_comma = false;
            } else if !seen_comma && let Some((rem, _)) = preceded(ws0, tag(",")).parse(input) {
                input = rem;
                seen_comma = true;
            } else {
                return Some((input, ids));
            }
        }
    }

    Some((input, vec![]))
}

pub fn return_stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    map(seq((tag("return"), ws1, expr)), |(_, _, expr)| {
        Stmt::Return { expr: expr.into() }
    })
    .parse(input)
}

pub fn declare_stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    map(
        seq((tag("let"), ws1, identifier, ws0, tag("="), ws0, expr)),
        |(_, _, id, _, _, _, expr)| Stmt::Declare {
            id,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn assign_stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    map(
        seq((identifier, ws0, tag("="), ws0, expr)),
        |(id, _, _, _, expr)| Stmt::Assign {
            id,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    alt((
        return_stmt,
        declare_stmt,
        assign_stmt,
        map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

pub fn named_fn_item<'i>(input: &'i str) -> ParseResult<&'i str, Item<'i>> {
    map(
        seq((
            tag("fn"),
            ws0,
            identifier,
            ws0,
            tag("("),
            ws0,
            parameter_list,
            ws0,
            tag(")"),
            ws0,
            tag("{"),
            ws0,
            block_contents,
            ws0,
            tag("}"),
        )),
        |(_, _, name, _, _, _, params, _, _, _, _, _, body, _, _)| Item::NamedFn {
            name,
            params,
            body,
        },
    )
    .parse(input)
}

pub fn item<'i>(input: &'i str) -> ParseResult<&'i str, Item<'i>> {
    alt((
        named_fn_item,
        // declare_stmt,
        // assign_stmt,
        // map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

enum StmtOrItem<'i> {
    Stmt(Stmt<'i>),
    Item(Item<'i>),
}

fn stmt_or_item<'i>(input: &'i str) -> ParseResult<&'i str, StmtOrItem<'i>> {
    alt((map(stmt, StmtOrItem::Stmt), map(item, StmtOrItem::Item))).parse(input)
}

pub fn block_contents<'i>(input: &'i str) -> ParseResult<&'i str, Block<'i>> {
    let sep = regex(r"^[ \t]*([;\n][ \t]*)+");

    map(
        optional(seq((
            stmt_or_item,
            many0(preceded(many0(sep), stmt_or_item)),
        ))),
        |m| {
            let mut block = Block {
                items: vec![],
                stmts: vec![],
            };

            if let Some((first, rest)) = m {
                match first {
                    StmtOrItem::Stmt(stmt) => block.stmts.push(stmt),
                    StmtOrItem::Item(item) => block.items.push(item),
                }

                for el in rest {
                    match el {
                        StmtOrItem::Stmt(stmt) => block.stmts.push(stmt),
                        StmtOrItem::Item(item) => block.items.push(item),
                    }
                }
            }

            block
        },
    )
    .parse(input)
}

pub fn document<'i>(input: &'i str) -> ParseResult<&'i str, Document<'i>> {
    map(seq((ws0, block_contents, ws0, eof)), |(_, body, _, _)| {
        Document { body }
    })
    .parse(input)
}

pub fn parse_document<'i>(input: &'i str) -> Option<Document<'i>> {
    document.parse(input).map(|(_, doc)| doc)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Identifier,
        parse::identifier,
        parser_combinators::{alt, recognize, seq, tag, Parser},
    };

    fn var<'a>(name: &'a str) -> Expr<'a> {
        Expr::Variable(Identifier(name))
    }

    fn str<'a>(s: &'a str) -> Expr<'a> {
        Expr::StrLiteral {
            pieces: vec![StrLiteralPiece::Fragment(s.into())],
        }
    }

    fn binary<'a>(op: &'a str, left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
        Expr::BinaryExpr {
            left: left.into(),
            op,
            right: right.into(),
        }
    }

    fn unary<'a>(op: &'a str, expr: Expr<'a>) -> Expr<'a> {
        Expr::UnaryExpr {
            op,
            expr: expr.into(),
        }
    }

    fn empty_anon<'a>() -> Expr<'a> {
        Expr::AnonymousFn {
            params: vec![],
            body: Block {
                items: vec![],
                stmts: vec![],
            },
        }
    }

    fn anon_expr<'a>(params: Vec<&'a str>, expr: Expr<'a>) -> Expr<'a> {
        Expr::AnonymousFn {
            params: params.iter().map(|name| Identifier(name)).collect(),
            body: Block {
                items: vec![],
                stmts: vec![Stmt::Expr { expr: expr.into() }],
            },
        }
    }

    fn simple_invocation<'a>(name: &'a str, exprs: Vec<Expr<'a>>) -> Expr<'a> {
        Expr::Invocation {
            expr: Expr::Variable(Identifier(name)).into(),
            args: exprs
                .into_iter()
                .map(|expr| Argument { name: None, expr })
                .collect(),
        }
    }

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
        assert_eq!(
            parameter_list.parse("blue , kelley"),
            Some(("", vec![Identifier("blue"), Identifier("kelley")]))
        );
        assert_eq!(
            parameter_list.parse("kelley ,,"),
            Some((",", vec![Identifier("kelley")]))
        );
        assert_eq!(
            parameter_list.parse("kelley , blue , )"),
            Some((" )", vec![Identifier("kelley"), Identifier("blue")]))
        );
        assert_eq!(
            expr.parse("kelley ?"),
            Some((" ?", Expr::Variable(Identifier("kelley"))))
        );
        assert_eq!(
            expr.parse("kelley + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Variable(Identifier("kelley")).into(),
                    op: "+",
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            if_expr.parse("if ( kelley ) { 21 } ?"),
            Some((
                " ?",
                Expr::If {
                    cond: Expr::Variable(Identifier("kelley")).into(),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Expr {
                            expr: Expr::Numeric(Numeric::Int(21)).into()
                        }]
                    },
                    els: None,
                }
            ))
        );
        assert_eq!(
            expr.parse("kelley(12) + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Invocation {
                        expr: Expr::Variable(Identifier("kelley")).into(),
                        args: vec![Argument {
                            name: None,
                            expr: Expr::Numeric(Numeric::Int(12)).into()
                        }]
                    }
                    .into(),
                    op: "+",
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            expr.parse("kelley ( bla = 12, ) + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Invocation {
                        expr: Expr::Variable(Identifier("kelley")).into(),
                        args: vec![Argument {
                            name: Some(Identifier("bla")),
                            expr: Expr::Numeric(Numeric::Int(12)).into()
                        }]
                    }
                    .into(),
                    op: "+",
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            expr.parse("kelley ( bla = 12, ) || { } + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Invocation {
                        expr: Expr::Variable(Identifier("kelley")).into(),
                        args: vec![
                            Argument {
                                name: Some(Identifier("bla")),
                                expr: Expr::Numeric(Numeric::Int(12)).into()
                            },
                            Argument {
                                name: None,
                                expr: Expr::AnonymousFn {
                                    params: vec![],
                                    body: Block {
                                        items: vec![],
                                        stmts: vec![]
                                    }
                                }
                                .into(),
                            }
                        ]
                    }
                    .into(),
                    op: "+",
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= 7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    id: Identifier("h"),
                    expr: Expr::Numeric(Numeric::Int(7)).into()
                }
            ))
        );
        assert_eq!(expr.parse(r#""world""#), Some(("", str("world"))));
        assert_eq!(expr.parse(r#"stdin"#), Some(("", var("stdin"))));
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n""#),
            Some((
                "",
                simple_invocation("split", vec![var("stdin"), str(r"\n\n")])
            ))
        );
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n" :map {}"#),
            Some((
                "",
                simple_invocation(
                    "map",
                    vec![
                        simple_invocation("split", vec![var("stdin"), str(r"\n\n")]),
                        empty_anon()
                    ]
                )
            ))
        );
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n" :map |group| { group }"#),
            Some((
                "",
                simple_invocation(
                    "map",
                    vec![
                        simple_invocation("split", vec![var("stdin"), str(r"\n\n")]),
                        anon_expr(vec!["group"], var("group"))
                    ]
                )
            ))
        );
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n" :map |group| { group } .max"#),
            Some((
                "",
                simple_invocation(
                    "max",
                    vec![simple_invocation(
                        "map",
                        vec![
                            simple_invocation("split", vec![var("stdin"), str(r"\n\n")]),
                            anon_expr(vec!["group"], var("group"))
                        ]
                    )]
                )
            ))
        );
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n" :map |group| { group } .max :bla bla"#),
            Some((
                "",
                simple_invocation(
                    "bla",
                    vec![
                        simple_invocation(
                            "max",
                            vec![simple_invocation(
                                "map",
                                vec![
                                    simple_invocation("split", vec![var("stdin"), str(r"\n\n")]),
                                    anon_expr(vec!["group"], var("group"))
                                ]
                            )]
                        ),
                        var("bla")
                    ]
                )
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "world""#),
            Some((
                "",
                Stmt::Declare {
                    id: Identifier("v"),
                    expr: str("world").into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "wor{ x + 1 }ld""#),
            Some((
                "",
                Stmt::Declare {
                    id: Identifier("v"),
                    expr: Expr::StrLiteral {
                        pieces: vec![
                            StrLiteralPiece::Fragment("wor".into()),
                            StrLiteralPiece::Interpolation(Expr::BinaryExpr {
                                left: Expr::Variable(Identifier("x")).into(),
                                op: "+",
                                right: Expr::Numeric(Numeric::Int(1)).into()
                            }),
                            StrLiteralPiece::Fragment("ld".into()),
                        ]
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            item.parse(r#"fn main() {}"#),
            Some((
                "",
                Item::NamedFn {
                    name: Identifier("main"),
                    params: vec![],
                    body: Block {
                        items: vec![],
                        stmts: vec![]
                    }
                }
            ))
        );
        assert_eq!(
            item.parse(r#"fn main() { h = 1 }"#),
            Some((
                "",
                Item::NamedFn {
                    name: Identifier("main"),
                    params: vec![],
                    body: Block {
                        items: vec![],
                        stmts: vec![Stmt::Assign {
                            id: Identifier("h"),
                            expr: Expr::Numeric(Numeric::Int(1)).into()
                        }]
                    }
                }
            ))
        );
        assert_eq!(
            item.parse(
                r#"fn make_counter(start) {
                    let n = start
                    |d| {
                        if (d == 0) {
                            n = 0
                        } else {
                            n = n + d
                        }
                    }
                }"#
            ),
            Some((
                "",
                Item::NamedFn {
                    name: Identifier("make_counter"),
                    params: vec![Identifier("start")],
                    body: Block {
                        items: vec![],
                        stmts: vec![
                            Stmt::Declare {
                                id: Identifier("n"),
                                expr: Expr::Variable(Identifier("start")).into()
                            },
                            Stmt::Expr {
                                expr: Expr::AnonymousFn {
                                    params: vec![Identifier("d")],
                                    body: Block {
                                        items: vec![],
                                        stmts: vec![Stmt::Expr {
                                            expr: Expr::If {
                                                cond: Expr::BinaryExpr {
                                                    left: Expr::Variable(Identifier("d")).into(),
                                                    op: "==",
                                                    right: Expr::Numeric(Numeric::Int(0)).into()
                                                }
                                                .into(),
                                                then: Block {
                                                    items: vec![],
                                                    stmts: vec![Stmt::Assign {
                                                        id: Identifier("n"),
                                                        expr: Expr::Numeric(Numeric::Int(0)).into()
                                                    }]
                                                },
                                                els: Some(Block {
                                                    items: vec![],
                                                    stmts: vec![Stmt::Assign {
                                                        id: Identifier("n"),
                                                        expr: Expr::BinaryExpr {
                                                            left: Expr::Variable(Identifier("n"))
                                                                .into(),
                                                            op: "+",
                                                            right: Expr::Variable(Identifier("d"))
                                                                .into()
                                                        }
                                                        .into()
                                                    }]
                                                })
                                            }
                                            .into()
                                        }]
                                    }
                                }
                                .into()
                            }
                        ]
                    }
                }
            ))
        );
        assert_eq!(
            block_contents.parse(
                "let h= 7
 ?"
            ),
            Some((
                "\n ?",
                Block {
                    items: vec![],
                    stmts: vec![Stmt::Declare {
                        id: Identifier("h"),
                        expr: Expr::Numeric(Numeric::Int(7)).into()
                    }]
                }
            ))
        );
        assert_eq!(
            block_contents.parse(
                "h= 7 

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Assign {
                            id: Identifier("h"),
                            expr: Expr::Numeric(Numeric::Int(7)).into()
                        },
                        Stmt::Expr {
                            expr: Expr::Numeric(Numeric::Int(5)).into()
                        }
                    ]
                }
            ))
        );
        assert_eq!(
            block_contents.parse(
                "let h= 7 ; kelley= 712 ;; 

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Declare {
                            id: Identifier("h"),
                            expr: Expr::Numeric(Numeric::Int(7)).into()
                        },
                        Stmt::Assign {
                            id: Identifier("kelley"),
                            expr: Expr::Numeric(Numeric::Int(712)).into()
                        },
                        Stmt::Expr {
                            expr: Expr::Numeric(Numeric::Int(5)).into()
                        }
                    ]
                }
            ))
        );
        assert_eq!(
            block_contents.parse("let h = 7; fn main() {}"),
            Some((
                "",
                Block {
                    items: vec![Item::NamedFn {
                        name: Identifier("main"),
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }],
                    stmts: vec![Stmt::Declare {
                        id: Identifier("h"),
                        expr: Expr::Numeric(Numeric::Int(7)).into()
                    }]
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= { 7 ;1 } ?"),
            Some((
                " ?",
                Stmt::Declare {
                    id: Identifier("h"),
                    expr: Expr::AnonymousFn {
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![
                                Stmt::Expr {
                                    expr: Expr::Numeric(Numeric::Int(7)).into()
                                },
                                Stmt::Expr {
                                    expr: Expr::Numeric(Numeric::Int(1)).into()
                                }
                            ]
                        }
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            document.parse(
                r#"
let v = "world"

let h = 2

{
  "hello {v} {h}"
}
"#
            ),
            Some((
                "",
                Document {
                    body: Block {
                        items: vec![],
                        stmts: vec![
                            Stmt::Declare {
                                id: Identifier("v"),
                                expr: str("world").into()
                            },
                            Stmt::Declare {
                                id: Identifier("h"),
                                expr: Expr::Numeric(Numeric::Int(2)).into()
                            },
                            Stmt::Expr {
                                expr: Expr::AnonymousFn {
                                    params: vec![],
                                    body: Block {
                                        items: vec![],
                                        stmts: vec![Stmt::Expr {
                                            expr: Expr::StrLiteral {
                                                pieces: vec![
                                                    StrLiteralPiece::Fragment("hello ".into()),
                                                    StrLiteralPiece::Interpolation(Expr::Variable(
                                                        Identifier("v")
                                                    )),
                                                    StrLiteralPiece::Fragment(" ".into()),
                                                    StrLiteralPiece::Interpolation(Expr::Variable(
                                                        Identifier("h")
                                                    )),
                                                ]
                                            }
                                            .into()
                                        }]
                                    }
                                }
                                .into()
                            }
                        ]
                    }
                }
            ))
        );

        {
            let a = document.parse(
                r#"
let v = "world"

View() {
    print("hello {v}")

    let result = run {
        let h = 7
        6 + h
    }

    if (something(2, 6)) {
        sdf
    } else {

    }

    Box |ctx| {
        Title {

        }
    }
}
"#,
            );

            assert!(a.is_some());
        }

        {
            let a = document.parse(
                r#"let i = 0

        fn make_counter(start) {}

        "#,
            );

            assert!(a.is_some());
        }
    }
}
