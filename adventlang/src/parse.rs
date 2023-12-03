use std::str::FromStr;

use regex::Regex;

use crate::{
    ast::{
        Argument, Block, Document, Expr, Identifier, Item, Pattern, Stmt, StrLiteralPiece, Type,
    },
    parser_combinators::{
        alt, delimited, many0, map, optional, preceded, regex, seq, tag, terminated, ParseResult,
        Parser,
    },
    runtime::{AlRegex, Numeric},
};

pub fn identifier(input: &str) -> ParseResult<&str, Identifier> {
    let (input, id) =
        map(regex(r"^[_a-zA-Z][_a-zA-Z0-9]*"), |s| Identifier(s.into())).parse(input)?;

    if [
        "fn", "if", "else", "then", "while", "do", "for", "let", "loop", "true", "false",
    ]
    .contains(&id.0.as_str())
    {
        return None;
    }

    Some((input, id))
}

pub fn slws0(input: &str) -> ParseResult<&str, &str> {
    regex(r"^[ \t]*").parse(input)
}

pub fn ws0(input: &str) -> ParseResult<&str, &str> {
    regex(r"^\s*").parse(input)
}

pub fn ws1(input: &str) -> ParseResult<&str, &str> {
    regex(r"^\s+").parse(input)
}

pub fn eof(input: &str) -> ParseResult<&str, ()> {
    if input.len() == 0 {
        Some((input, ()))
    } else {
        None
    }
}

// TODO
fn unescape(input: &str) -> String {
    input.replace("\\n", "\n").replace("\\/", "/")
}

pub fn str_literal(input: &str) -> ParseResult<&str, Expr> {
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

// ugly, I know
fn regex_contents(input: &str) -> ParseResult<&str, String> {
    let mut contents = "".to_string();
    let mut escaped = false;

    for (i, c) in input.char_indices() {
        if escaped {
            if c == 'n' {
                contents.push('\n');
                // etc..
            } else {
                contents.push(c);
            }
            escaped = false;
        } else if c == '/' {
            return Some((&input[i..], contents));
        } else if c == '\\' {
            escaped = true;
        } else {
            contents.push(c);
        }
    }

    Some(("", contents))
}

pub fn regex_literal(input: &str) -> ParseResult<&str, Expr> {
    let (rem, str) = delimited(tag("/"), regex_contents, tag("/")).parse(input)?;

    Some((
        rem,
        Expr::RegexLiteral {
            regex: AlRegex(Regex::from_str(&str).ok()?),
        },
    ))
}

// TODO
pub fn numeric(input: &str) -> ParseResult<&str, Numeric> {
    map(regex(r"^-?[0-9]+"), |s| {
        Numeric::Int(s.parse::<i64>().unwrap())
    })
    .parse(input)
}

pub fn anonymous_fn(input: &str) -> ParseResult<&str, Expr> {
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

pub fn if_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("if"),
            ws1,
            tag("("),
            ws0,
            optional(delimited(
                seq((tag("let"), ws1)),
                pattern,
                seq((ws0, tag("="), ws0)),
            )),
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
        |(_, _, _, _, pattern, cond, _, _, _, then, els)| Expr::If {
            pattern,
            cond: cond.into(),
            then,
            els,
        },
    )
    .parse(input)
}

pub fn do_while_expr(input: &str) -> ParseResult<&str, Expr> {
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

pub fn loop_expr(input: &str) -> ParseResult<&str, Expr> {
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

pub fn while_expr(input: &str) -> ParseResult<&str, Expr> {
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

pub fn for_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("for"),
            ws1,
            tag("("),
            ws0,
            tag("let"),
            ws0,
            pattern,
            ws0,
            tag("in"),
            ws0,
            expr,
            ws0,
            tag(")"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(_, _, _, _, _, _, pattern, _, _, _, range, _, _, _, body)| Expr::For {
            pattern,
            range: range.into(),
            body,
        },
    )
    .parse(input)
}

pub fn list_literal(input: &str) -> ParseResult<&str, Expr> {
    delimited(
        seq((tag("["), ws0)),
        map(
            optional(seq((
                expr,
                many0(preceded(seq((ws0, tag(","), ws0)), expr)),
                ws0,
                optional(tag(",")),
            ))),
            |opt| Expr::ListLiteral {
                elements: match opt {
                    None => vec![],
                    Some((first_el, mut els, _, _)) => {
                        els.insert(0, first_el);
                        els
                    }
                },
            },
        ),
        seq((ws0, tag("]"))),
    )
    .parse(input)
}

pub fn tuple_literal_or_parenthesized_expr(input: &str) -> ParseResult<&str, Expr> {
    delimited(
        seq((tag("("), ws0)),
        map(
            seq((
                expr,
                many0(preceded(seq((ws0, tag(","), ws0)), expr)),
                ws0,
                optional(tag(",")),
            )),
            |(first_el, mut els, _, final_comma)| {
                if els.len() == 0 && final_comma.is_none() {
                    return first_el;
                }

                Expr::TupleLiteral {
                    elements: {
                        els.insert(0, first_el);
                        els
                    },
                }
            },
        ),
        seq((ws0, tag(")"))),
    )
    .parse(input)
}

pub fn expr_leaf(input: &str) -> ParseResult<&str, Expr> {
    alt((
        map(tag("true"), |_| Expr::Bool(true)),
        map(tag("false"), |_| Expr::Bool(false)),
        map(tag("nil"), |_| Expr::NilLiteral),
        do_while_expr,
        while_expr,
        loop_expr,
        for_expr,
        map(identifier, Expr::Variable),
        map(numeric, Expr::Numeric),
        str_literal,
        regex_literal,
        anonymous_fn,
        tuple_literal_or_parenthesized_expr,
        list_literal,
    ))
    .parse(input)
}

pub fn argument(input: &str) -> ParseResult<&str, Argument> {
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

pub fn parenthesized_args(input: &str) -> ParseResult<&str, Vec<Argument>> {
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

pub fn invocation_args(input: &str) -> ParseResult<&str, Vec<Argument>> {
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

pub fn expr_index_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            expr_leaf,
            many0(delimited(
                seq((ws0, tag("["), ws0)),
                expr,
                seq((ws0, tag("]"))),
            )),
        )),
        |(mut expr, indices)| {
            for index in indices {
                expr = Expr::Invocation {
                    expr: Expr::Variable(Identifier("index".into())).into(),
                    args: vec![
                        Argument { name: None, expr },
                        Argument {
                            name: None,
                            expr: index,
                        },
                    ],
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn expr_call_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((expr_index_stack, many0(preceded(slws0, invocation_args)))),
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

pub fn unary_expr_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((many0(terminated(tag("!"), ws0)), expr_call_stack)),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                expr = Expr::UnaryExpr {
                    expr: expr.into(),
                    op: op.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn mul_expr_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            unary_expr_stack,
            many0(seq((ws0, alt((tag("*"), tag("/"))), ws0, unary_expr_stack))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn add_expr_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            mul_expr_stack,
            many0(seq((ws0, alt((tag("+"), tag("-"))), ws0, mul_expr_stack))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn infix_or_postfix_fn_call_stack(input: &str) -> ParseResult<&str, Expr> {
    enum Op {
        Unary(Identifier),
        Binary((Identifier, Expr)),
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

pub fn equ_expr_stack(input: &str) -> ParseResult<&str, Expr> {
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
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn and_expr_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            equ_expr_stack,
            many0(seq((
                ws0,
                alt((
                    tag("&&"),
                    //
                )),
                ws0,
                equ_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn or_expr_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            and_expr_stack,
            many0(seq((
                ws0,
                alt((
                    tag("||"),
                    //
                )),
                ws0,
                and_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                expr = Expr::BinaryExpr {
                    left: expr.into(),
                    op: op.into(),
                    right: right.into(),
                }
            }
            expr
        },
    )
    .parse(input)
}

pub fn expr(input: &str) -> ParseResult<&str, Expr> {
    alt((if_expr, or_expr_stack)).parse(input)
}

pub fn parameter_list(mut input: &str) -> ParseResult<&str, Vec<Pattern>> {
    if let Some((rem, id)) = pattern.parse(input) {
        let mut ids = vec![];
        let mut seen_comma = false;

        ids.push(id);
        input = rem;

        loop {
            if seen_comma && let Some((rem, id)) = preceded(ws0, pattern).parse(input) {
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

pub fn return_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(seq((tag("return"), ws1, expr)), |(_, _, expr)| {
        Stmt::Return { expr: expr.into() }
    })
    .parse(input)
}

fn type_leaf(input: &str) -> ParseResult<&str, Type> {
    alt((
        map(tag("any"), |_| Type::Any),
        map(tag("nil"), |_| Type::Nil),
        map(tag("bool"), |_| Type::Bool),
        map(tag("str"), |_| Type::Str),
        map(tag("int"), |_| Type::Numeric),
        map(tag("double"), |_| Type::Numeric),
        // TODO fns
        map(tag("list"), |_| Type::List(Type::Any.into())),
        map(tag("tuple"), |_| Type::Tuple),
    ))
    .parse(input)
}

pub fn typespec(input: &str) -> ParseResult<&str, Type> {
    map(
        seq((
            type_leaf,
            many0(preceded(seq((ws0, tag("|"), ws0)), type_leaf)),
        )),
        |(first, mut rest)| {
            if rest.len() > 0 {
                rest.insert(0, first);
                Type::Union(rest)
            } else {
                first
            }
        },
    )
    .parse(input)
}

pub fn pattern(input: &str) -> ParseResult<&str, Pattern> {
    alt((
        map(
            seq((
                identifier,
                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
            )),
            |(id, ty)| Pattern::Id(id, ty),
        ),
        delimited(
            seq((tag("["), ws0)),
            map(
                optional(seq((
                    pattern,
                    many0(preceded(seq((ws0, tag(","), ws0)), pattern)),
                    ws0,
                    optional(tag(",")),
                ))),
                |opt| {
                    Pattern::List(match opt {
                        None => vec![],
                        Some((first, mut ps, _, _)) => {
                            ps.insert(0, first);
                            ps
                        }
                    })
                },
            ),
            seq((ws0, tag("]"))),
        ),
        delimited(
            seq((tag("("), ws0)),
            map(
                optional(seq((
                    pattern,
                    many0(preceded(seq((ws0, tag(","), ws0)), pattern)),
                    ws0,
                    optional(tag(",")),
                ))),
                |opt| {
                    Pattern::Tuple(match opt {
                        None => vec![],
                        Some((first, mut ps, _, _)) => {
                            ps.insert(0, first);
                            ps
                        }
                    })
                },
            ),
            seq((ws0, tag(")"))),
        ),
    ))
    .parse(input)
}

pub fn declare_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(
        seq((tag("let"), ws1, pattern, ws0, tag("="), ws0, expr)),
        |(_, _, pattern, _, _, _, expr)| Stmt::Declare {
            pattern,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn assign_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(
        seq((identifier, ws0, tag("="), ws0, expr)),
        |(id, _, _, _, expr)| Stmt::Assign {
            id,
            expr: expr.into(),
        },
    )
    .parse(input)
}

pub fn stmt(input: &str) -> ParseResult<&str, Stmt> {
    alt((
        return_stmt,
        declare_stmt,
        assign_stmt,
        map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

pub fn named_fn_item(input: &str) -> ParseResult<&str, Item> {
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

pub fn item(input: &str) -> ParseResult<&str, Item> {
    alt((
        named_fn_item,
        // declare_stmt,
        // assign_stmt,
        // map(expr, |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

enum StmtOrItem {
    Stmt(Stmt),
    Item(Item),
}

fn stmt_or_item(input: &str) -> ParseResult<&str, StmtOrItem> {
    alt((map(stmt, StmtOrItem::Stmt), map(item, StmtOrItem::Item))).parse(input)
}

pub fn block_contents(input: &str) -> ParseResult<&str, Block> {
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

pub fn document(input: &str) -> ParseResult<&str, Document> {
    map(seq((ws0, block_contents, ws0, eof)), |(_, body, _, _)| {
        Document { body }
    })
    .parse(input)
}

pub fn parse_document(input: &str) -> Option<Document> {
    let input = input
        .lines()
        // remove //-style comments
        .map(|line| match line.split_once("//") {
            None => line,
            Some((code, _)) => code,
        })
        .collect::<Vec<_>>()
        .join("\n");

    document.parse(&input).map(|(_, doc)| doc)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Identifier,
        parse::identifier,
        parser_combinators::{alt, recognize, seq, tag, Parser},
    };

    fn id(id: &str) -> Identifier {
        Identifier(id.into())
    }

    fn var(name: &str) -> Expr {
        Expr::Variable(Identifier(name.into()))
    }

    fn tuple(elements: Vec<Expr>) -> Expr {
        Expr::TupleLiteral { elements }
    }

    fn str(s: &str) -> Expr {
        Expr::StrLiteral {
            pieces: vec![StrLiteralPiece::Fragment(s.into())],
        }
    }

    fn int(n: i64) -> Expr {
        Expr::Numeric(Numeric::Int(n))
    }

    fn binary(op: &str, left: Expr, right: Expr) -> Expr {
        Expr::BinaryExpr {
            left: left.into(),
            op: op.into(),
            right: right.into(),
        }
    }

    fn unary(op: &str, expr: Expr) -> Expr {
        Expr::UnaryExpr {
            op: op.into(),
            expr: expr.into(),
        }
    }

    fn empty_anon() -> Expr {
        Expr::AnonymousFn {
            params: vec![],
            body: Block {
                items: vec![],
                stmts: vec![],
            },
        }
    }

    fn anon_expr(params: Vec<&str>, expr: Expr) -> Expr {
        Expr::AnonymousFn {
            params: params
                .iter()
                .map(|&name| Pattern::Id(Identifier(name.into()), None))
                .collect(),
            body: Block {
                items: vec![],
                stmts: vec![Stmt::Expr { expr: expr.into() }],
            },
        }
    }

    fn simple_invocation(name: &str, exprs: Vec<Expr>) -> Expr {
        Expr::Invocation {
            expr: Expr::Variable(Identifier(name.into())).into(),
            args: exprs
                .into_iter()
                .map(|expr| Argument { name: None, expr })
                .collect(),
        }
    }

    #[test]
    fn bla() {
        assert_eq!(identifier.parse("kelley"), Some(("", id("kelley"))));
        assert_eq!(identifier.parse("_kel6*"), Some(("*", id("_kel6"))));
        assert_eq!(identifier.parse(" kelley"), None);
        assert_eq!(identifier.parse(""), None);

        assert_eq!(
            seq((ws0, identifier)).parse("kelley"),
            Some(("", ("", id("kelley"))))
        );
        assert_eq!(seq((ws1, identifier)).parse("kelley"), None);
        assert_eq!(
            seq((ws1, identifier)).parse(" kelley"),
            Some(("", (" ", id("kelley"))))
        );
        assert_eq!(
            recognize(seq((ws1, identifier, ws0))).parse(" kelley ?"),
            Some(("?", " kelley "))
        );
        assert_eq!(
            seq((ws1, identifier, ws1, identifier)).parse(" kelley  bla"),
            Some(("", (" ", id("kelley"), "  ", id("bla"))))
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
            Some((
                "",
                vec![
                    Pattern::Id(id("blue"), None),
                    Pattern::Id(id("kelley"), None)
                ]
            ))
        );
        assert_eq!(
            parameter_list.parse("kelley ,,"),
            Some((",", vec![Pattern::Id(id("kelley"), None)]))
        );
        assert_eq!(
            parameter_list.parse("kelley , blue , )"),
            Some((
                " )",
                vec![
                    Pattern::Id(id("kelley"), None),
                    Pattern::Id(id("blue"), None)
                ]
            ))
        );
        assert_eq!(expr.parse("kelley ?"), Some((" ?", var("kelley"))));
        assert_eq!(expr.parse("(kelley) ?"), Some((" ?", var("kelley"))));
        assert_eq!(
            expr.parse("(kelley,) ?"),
            Some((" ?", tuple(vec![var("kelley")])))
        );
        assert_eq!(
            expr.parse("(kelley, 21,) ?"),
            Some((" ?", tuple(vec![var("kelley"), int(21)])))
        );
        assert_eq!(
            expr.parse("kelley + 21 ?"),
            Some((
                " ?",
                Expr::BinaryExpr {
                    left: Expr::Variable(id("kelley")).into(),
                    op: "+".into(),
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            if_expr.parse("if ( kelley ) { 21 } ?"),
            Some((
                " ?",
                Expr::If {
                    pattern: None,
                    cond: Expr::Variable(id("kelley")).into(),
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
            if_expr.parse("if (let h = kelley ) { 21 } ?"),
            Some((
                " ?",
                Expr::If {
                    pattern: Some(Pattern::Id(id("h"), None)),
                    cond: Expr::Variable(id("kelley")).into(),
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
                        expr: Expr::Variable(id("kelley")).into(),
                        args: vec![Argument {
                            name: None,
                            expr: Expr::Numeric(Numeric::Int(12)).into()
                        }]
                    }
                    .into(),
                    op: "+".into(),
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
                        expr: Expr::Variable(id("kelley")).into(),
                        args: vec![Argument {
                            name: Some(id("bla")),
                            expr: Expr::Numeric(Numeric::Int(12)).into()
                        }]
                    }
                    .into(),
                    op: "+".into(),
                    right: Expr::Numeric(Numeric::Int(21)).into()
                }
            ))
        );
        assert_eq!(
            expr.parse("kelley ( bla = 12, ) || { } + 21 ?"),
            Some((
                " ?",
                binary(
                    "+",
                    Expr::Invocation {
                        expr: Expr::Variable(id("kelley")).into(),
                        args: vec![
                            Argument {
                                name: Some(id("bla")),
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
                    },
                    int(21)
                )
            ))
        );
        assert_eq!(
            expr.parse("|| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    params: vec![],
                    body: Block {
                        items: vec![],
                        stmts: vec![]
                    }
                }
            ))
        );
        assert_eq!(
            expr.parse("|a| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    params: vec![Pattern::Id(id("a"), None)],
                    body: Block {
                        items: vec![],
                        stmts: vec![]
                    }
                }
            ))
        );
        assert_eq!(
            expr.parse("|(a, b)| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    params: vec![Pattern::Tuple(vec![
                        Pattern::Id(id("a"), None),
                        Pattern::Id(id("b"), None)
                    ])],
                    body: Block {
                        items: vec![],
                        stmts: vec![]
                    }
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= 7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: Pattern::Id(id("h"), None),
                    expr: int(7).into()
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= -7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: Pattern::Id(id("h"), None),
                    expr: int(-7).into()
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= !-7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: Pattern::Id(id("h"), None),
                    expr: unary("!", int(-7)).into()
                }
            ))
        );
        assert_eq!(expr.parse(r#""world""#), Some(("", str("world"))));
        assert_eq!(expr.parse(r#"stdin"#), Some(("", var("stdin"))));
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n""#),
            Some((
                "",
                simple_invocation("split", vec![var("stdin"), str("\n\n")])
            ))
        );
        assert_eq!(
            expr.parse(r#"stdin :split "\n\n" :map {}"#),
            Some((
                "",
                simple_invocation(
                    "map",
                    vec![
                        simple_invocation("split", vec![var("stdin"), str("\n\n")]),
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
                        simple_invocation("split", vec![var("stdin"), str("\n\n")]),
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
                            simple_invocation("split", vec![var("stdin"), str("\n\n")]),
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
                                    simple_invocation("split", vec![var("stdin"), str("\n\n")]),
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
            stmt.parse(r#"let v = /[0-9]+/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[0-9]+").unwrap())
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = /[0-9\/]+/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[0-9/]+").unwrap())
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = /[!@^&*#+%$=\/]/"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: Expr::RegexLiteral {
                        regex: AlRegex(Regex::from_str("[!@^&*#+%$=/]").unwrap())
                    }
                    .into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(
                r#"let v = y > 0 && schematic[y - 1] :slice (x, x+len) :match /[!@^&*#+%$=\/]/"#
            ),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: binary(
                        "&&",
                        binary(">", var("y"), int(0)),
                        simple_invocation(
                            "match",
                            vec![
                                simple_invocation(
                                    "slice",
                                    vec![
                                        simple_invocation(
                                            "index",
                                            vec![var("schematic"), binary("-", var("y"), int(1))]
                                        ),
                                        Expr::TupleLiteral {
                                            elements: vec![
                                                var("x"),
                                                binary("+", var("x"), var("len"))
                                            ]
                                        }
                                    ]
                                ),
                                Expr::RegexLiteral {
                                    regex: AlRegex(Regex::from_str("[!@^&*#+%$=/]").unwrap())
                                }
                            ]
                        )
                    )
                    .into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "world""#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: str("world").into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "world"[0]"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: simple_invocation("index", vec![str("world").into(), int(0).into(),])
                        .into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "wor{ x + 1 }ld""#),
            Some((
                "",
                Stmt::Declare {
                    pattern: Pattern::Id(id("v"), None),
                    expr: Expr::StrLiteral {
                        pieces: vec![
                            StrLiteralPiece::Fragment("wor".into()),
                            StrLiteralPiece::Interpolation(Expr::BinaryExpr {
                                left: Expr::Variable(id("x")).into(),
                                op: "+".into(),
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
                    name: id("main"),
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
                    name: id("main"),
                    params: vec![],
                    body: Block {
                        items: vec![],
                        stmts: vec![Stmt::Assign {
                            id: id("h"),
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
                    name: id("make_counter"),
                    params: vec![Pattern::Id(id("start"), None)],
                    body: Block {
                        items: vec![],
                        stmts: vec![
                            Stmt::Declare {
                                pattern: Pattern::Id(id("n"), None),
                                expr: Expr::Variable(id("start")).into()
                            },
                            Stmt::Expr {
                                expr: Expr::AnonymousFn {
                                    params: vec![Pattern::Id(id("d"), None)],
                                    body: Block {
                                        items: vec![],
                                        stmts: vec![Stmt::Expr {
                                            expr: Expr::If {
                                                pattern: None,
                                                cond: Expr::BinaryExpr {
                                                    left: Expr::Variable(id("d")).into(),
                                                    op: "==".into(),
                                                    right: Expr::Numeric(Numeric::Int(0)).into()
                                                }
                                                .into(),
                                                then: Block {
                                                    items: vec![],
                                                    stmts: vec![Stmt::Assign {
                                                        id: id("n"),
                                                        expr: Expr::Numeric(Numeric::Int(0)).into()
                                                    }]
                                                },
                                                els: Some(Block {
                                                    items: vec![],
                                                    stmts: vec![Stmt::Assign {
                                                        id: id("n"),
                                                        expr: Expr::BinaryExpr {
                                                            left: Expr::Variable(id("n")).into(),
                                                            op: "+".into(),
                                                            right: Expr::Variable(id("d")).into()
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
                        pattern: Pattern::Id(id("h"), None),
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
                            id: id("h"),
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
                "let h:int = 7 ; kelley= 712 ;; 

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Declare {
                            pattern: Pattern::Id(id("h"), Some(Type::Numeric)),
                            expr: Expr::Numeric(Numeric::Int(7)).into()
                        },
                        Stmt::Assign {
                            id: id("kelley"),
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
                        name: id("main"),
                        params: vec![],
                        body: Block {
                            items: vec![],
                            stmts: vec![]
                        }
                    }],
                    stmts: vec![Stmt::Declare {
                        pattern: Pattern::Id(id("h"), None),
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
                    pattern: Pattern::Id(id("h"), None),
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
            stmt.parse("for (let i in range(1, 2)) {} ?"),
            Some((
                " ?",
                Stmt::Expr {
                    expr: Expr::For {
                        pattern: Pattern::Id(id("i"), None),
                        range: simple_invocation("range", vec![int(1), int(2)]).into(),
                        body: Block {
                            items: vec![],
                            stmts: vec![]
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
                                pattern: Pattern::Id(id("v"), None),
                                expr: str("world").into()
                            },
                            Stmt::Declare {
                                pattern: Pattern::Id(id("h"), None),
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
                                                        id("v")
                                                    )),
                                                    StrLiteralPiece::Fragment(" ".into()),
                                                    StrLiteralPiece::Interpolation(Expr::Variable(
                                                        id("h")
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
