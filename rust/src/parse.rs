use crate::{
    ast::{
        Argument, Expr, Identifier, Numeric, Stmt, StrLiteral, StrLiteralFragment, StrLiteralPiece,
    },
    parser_combinators::{
        alt, delimited, many0, map, optional, preceded, regex, seq, tag, terminated, ParseResult,
        Parser,
    },
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

pub fn anonymous_fn<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            optional(delimited(
                seq((tag("|"), ws0)),
                parameter_list,
                seq((ws0, tag("|"), ws0)),
            )),
            delimited(seq((tag("{"), ws0)), body, seq((ws0, tag("}")))),
        )),
        |(params, body)| Expr::AnonymousFn {
            params: params.unwrap_or_else(|| vec![]),
            body,
        },
    )
    .parse(input)
}

pub fn expr_leaf<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    alt((
        map(identifier, Expr::Variable),
        map(numeric, Expr::Numeric),
        map(str_literal, Expr::StrLiteral),
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

pub fn invocation_args<'i>(input: &'i str) -> ParseResult<&'i str, Vec<Argument<'i>>> {
    let mut parenthesized_args = map(
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
    );

    let trailing_anon_fn = map(anonymous_fn, |anon| Argument {
        name: None,
        expr: anon.into(),
    });

    if let Some((input, args)) = parenthesized_args.parse(input) {
        if let Some((input, arg)) = preceded(ws0, trailing_anon_fn).parse(input) {
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
        seq((expr_leaf, many0(preceded(ws0, invocation_args)))),
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

pub fn equ_expr_stack<'i>(input: &'i str) -> ParseResult<&'i str, Expr<'i>> {
    map(
        seq((
            add_expr_stack,
            many0(seq((
                ws0,
                alt((tag("=="), tag("!="), tag(">="), tag("<="))),
                ws0,
                add_expr_stack,
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
    equ_expr_stack.parse(input)
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

pub fn assign_stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    map(
        seq((tag("let"), ws1, identifier, ws0, tag("="), ws0, expr)),
        |(_, _, id, _, _, _, expr)| Stmt::Assign {
            id,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn stmt<'i>(input: &'i str) -> ParseResult<&'i str, Stmt<'i>> {
    alt((
        assign_stmt,
        map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

pub fn body<'i>(input: &'i str) -> ParseResult<&'i str, Vec<Stmt<'i>>> {
    let sep = regex(r"^[ \t]*([;\n][ \t]*)+");

    map(
        optional(seq((stmt, many0(preceded(many0(sep), stmt))))),
        |m| match m {
            None => vec![],
            Some((first_stmt, mut rest)) => {
                rest.insert(0, first_stmt);
                rest
            }
        },
    )
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
                                    body: vec![]
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
                Stmt::Assign {
                    id: Identifier("h"),
                    expr: Expr::Numeric(Numeric::Int(7)).into()
                }
            ))
        );
        assert_eq!(
            body.parse(
                "let h= 7 
?"
            ),
            Some((
                " \n?",
                vec![Stmt::Assign {
                    id: Identifier("h"),
                    expr: Expr::Numeric(Numeric::Int(7)).into()
                }]
            ))
        );
        assert_eq!(
            body.parse(
                "let h= 7 

5 ?"
            ),
            Some((
                " ?",
                vec![
                    Stmt::Assign {
                        id: Identifier("h"),
                        expr: Expr::Numeric(Numeric::Int(7)).into()
                    },
                    Stmt::Expr {
                        expr: Expr::Numeric(Numeric::Int(5)).into()
                    }
                ]
            ))
        );
        assert_eq!(
            body.parse(
                "let h= 7 ; let kelley= 712 ;; 

5 ?"
            ),
            Some((
                " ?",
                vec![
                    Stmt::Assign {
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
            ))
        );
        assert_eq!(
            stmt.parse("let h= { 7 ;1 } ?"),
            Some((
                " ?",
                Stmt::Assign {
                    id: Identifier("h"),
                    expr: Expr::AnonymousFn {
                        params: vec![],
                        body: vec![
                            Stmt::Expr {
                                expr: Expr::Numeric(Numeric::Int(7)).into()
                            },
                            Stmt::Expr {
                                expr: Expr::Numeric(Numeric::Int(1)).into()
                            }
                        ]
                    }
                    .into()
                }
            ))
        );
    }
}
