use std::str::FromStr;

use either::Either;
use regex::Regex;

use crate::{
    ast::{
        Argument, AssignPattern, Block, DeclarePattern, Document, Expr, Identifier, Item, Stmt,
        StrLiteralPiece, Type,
    },
    parser_combinators::{
        alt, delimited, many0, map, optional, preceded, regex, seq, tag, terminated, ParseResult,
        Parser,
    },
    runtime::{AlRegex, Numeric},
};

fn identifier(input: &str) -> ParseResult<&str, Identifier> {
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

fn slws0(input: &str) -> ParseResult<&str, &str> {
    regex(r"^[ \t]*").parse(input)
}

fn ws0(input: &str) -> ParseResult<&str, &str> {
    regex(r"^\s*").parse(input)
}

fn ws1(input: &str) -> ParseResult<&str, &str> {
    regex(r"^\s+").parse(input)
}

fn slws1(input: &str) -> ParseResult<&str, &str> {
    regex(r"^[ \t]+").parse(input)
}

fn eof(input: &str) -> ParseResult<&str, ()> {
    if input.len() == 0 {
        Some((input, ()))
    } else {
        None
    }
}

fn listy<'a, P, O>(
    open_tag: &'static str,
    first: P,
    rest: P,
    close_tag: &'static str,
) -> impl Parser<&'a str, Output = Vec<O>>
where
    P: Parser<&'a str, Output = O>,
{
    delimited(
        seq((tag(open_tag), ws0)),
        map(
            optional(seq((
                first,
                many0(preceded(seq((ws0, tag(","), ws0)), rest)),
                ws0,
                optional(tag(",")),
            ))),
            |opt| match opt {
                None => vec![],
                Some((first_el, mut els, _, _)) => {
                    els.insert(0, first_el);
                    els
                }
            },
        ),
        seq((ws0, tag(close_tag))),
    )
}

// TODO
fn unescape(input: &str) -> String {
    input.replace("\\n", "\n").replace("\\/", "/")
}

fn str_literal(input: &str) -> ParseResult<&str, Expr> {
    map(
        delimited(
            tag("\""),
            many0(alt((
                map(map(regex("^[^\"{]+"), unescape), StrLiteralPiece::Fragment),
                map(
                    seq((tag("{"), ws0, expr(false), ws0, tag("}"))),
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

fn regex_literal(input: &str) -> ParseResult<&str, Expr> {
    let (rem, str) = delimited(tag("/"), regex_contents, tag("/")).parse(input)?;

    Some((
        rem,
        Expr::RegexLiteral {
            regex: AlRegex(Regex::from_str(&str).ok()?),
        },
    ))
}

// TODO
fn integer(input: &str) -> ParseResult<&str, Numeric> {
    map(regex(r"^-?[0-9]+"), |s| {
        Numeric::Int(s.parse::<i64>().unwrap())
    })
    .parse(input)
}

// TODO
fn double(input: &str) -> ParseResult<&str, Numeric> {
    map(regex(r"^-?[0-9]+\.[0-9]+"), |s| {
        Numeric::Double(s.parse::<f64>().unwrap())
    })
    .parse(input)
}

fn anonymous_fn(input: &str) -> ParseResult<&str, Expr> {
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

fn maybe_parenthesized<'a, P, T>(mut parser: P) -> impl Parser<&'a str, Output = T>
where
    P: Parser<&'a str, Output = T>,
{
    move |input| {
        let (input, opt) = optional(seq((tag("("), ws0))).parse(input)?;

        let (input, res) = parser.parse(input)?;

        let input = match opt {
            None => input,
            Some(_) => seq((ws0, tag(")"))).parse(input)?.0,
        };

        Some((input, res))
    }
}

fn if_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("if"),
            ws1,
            maybe_parenthesized(seq((
                optional(delimited(
                    seq((tag("let"), ws1)),
                    declare_pattern,
                    seq((ws0, tag("="), ws0)),
                )),
                expr(true),
            ))),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
            optional(preceded(
                seq((ws0, tag("else"), ws0)),
                alt((
                    map(if_expr, Either::Left),
                    map(
                        delimited(
                            seq((ws0, tag("{"), ws0)),
                            block_contents,
                            seq((ws0, tag("}"))),
                        ),
                        Either::Right,
                    ),
                )),
            )),
        )),
        |(_, _, (pattern, cond), _, then, further)| Expr::If {
            pattern,
            cond: cond.into(),
            then,
            els: match further {
                Some(Either::Left(if_expr)) => Some(Block {
                    items: vec![],
                    stmts: vec![Stmt::Expr {
                        expr: if_expr.into(),
                    }],
                }),
                Some(Either::Right(else_block)) => Some(else_block),
                _ => None,
            },
        },
    )
    .parse(input)
}

fn do_while_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("do"),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
            optional(preceded(
                seq((ws0, tag("while"), slws1)),
                maybe_parenthesized(expr(true)),
            )),
        )),
        |(_, _, body, cond)| Expr::DoWhile {
            cond: cond.map(Box::new),
            body,
        },
    )
    .parse(input)
}

fn loop_expr(input: &str) -> ParseResult<&str, Expr> {
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

fn while_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("while"),
            ws1,
            maybe_parenthesized(expr(true)),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(_, _, cond, _, body)| Expr::While {
            cond: cond.into(),
            body,
        },
    )
    .parse(input)
}

fn for_expr(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            tag("for"),
            ws1,
            maybe_parenthesized(seq((
                tag("let"),
                ws0,
                declare_pattern,
                ws0,
                tag("in"),
                ws0,
                expr(true),
            ))),
            ws0,
            delimited(seq((tag("{"), ws0)), block_contents, seq((ws0, tag("}")))),
        )),
        |(_, _, (_, _, pattern, _, _, _, range), _, body)| Expr::For {
            pattern,
            range: range.into(),
            body,
        },
    )
    .parse(input)
}

fn list_literal(input: &str) -> ParseResult<&str, Expr> {
    map(listy("[", expr(false), expr(false), "]"), |elements| {
        Expr::ListLiteral { elements }
    })
    .parse(input)
}

fn tuple_literal_or_parenthesized_expr(input: &str) -> ParseResult<&str, Expr> {
    delimited(
        seq((tag("("), ws0)),
        map(
            seq((
                expr(false),
                many0(preceded(seq((ws0, tag(","), ws0)), expr(false))),
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

fn dict_pair(input: &str) -> ParseResult<&str, (Either<Identifier, Expr>, Expr)> {
    map(
        seq((
            alt((
                map(preceded(tag("."), identifier), Either::Left),
                map(expr(true), Either::Right),
            )),
            ws1,
            expr(false),
        )),
        |(key, _, value)| (key, value),
    )
    .parse(input)
}

fn dict_literal(input: &str) -> ParseResult<&str, Expr> {
    map(
        preceded(tag("@"), listy("{", dict_pair, dict_pair, "}")),
        |elements| Expr::DictLiteral { elements },
    )
    .parse(input)
}

fn expr_leaf(input: &str) -> ParseResult<&str, Expr> {
    alt((
        dict_literal,
        map(tag("true"), |_| Expr::Bool(true)),
        map(tag("false"), |_| Expr::Bool(false)),
        map(tag("nil"), |_| Expr::NilLiteral),
        do_while_expr,
        while_expr,
        loop_expr,
        for_expr,
        map(identifier, Expr::Variable),
        map(double, Expr::Numeric),
        map(integer, Expr::Numeric),
        str_literal,
        regex_literal,
        anonymous_fn,
        tuple_literal_or_parenthesized_expr,
        list_literal,
    ))
    .parse(input)
}

fn argument(input: &str) -> ParseResult<&str, Argument> {
    map(
        seq((
            optional(terminated(identifier, seq((ws0, tag("="), ws0)))),
            expr(false),
        )),
        |(name, expr)| Argument {
            name,
            expr: expr.into(),
        },
    )
    .parse(input)
}

fn invocation_args<'a>(constrained: bool) -> impl Parser<&'a str, Output = Vec<Argument>> {
    move |input: &'a str| {
        let trailing_anon_fn = map(anonymous_fn, |anon| Argument {
            name: None,
            expr: anon.into(),
        });

        if let Some((input, args)) = listy("(", argument, argument, ")").parse(input) {
            let mut seen_named_arg = false;
            for arg in &args {
                if seen_named_arg && arg.name.is_none() {
                    // unnamed args cannot follow named args
                    return None;
                } else if arg.name.is_some() {
                    seen_named_arg = true;
                }
            }

            if !constrained && let Some((input, arg)) = preceded(slws0, trailing_anon_fn).parse(input) {
                let mut args = args;
                args.push(arg);
                Some((input, args))
            } else {
                Some((input, args))
            }
        } else {
            if constrained {
                None
            } else {
                map(trailing_anon_fn, |arg| vec![arg]).parse(input)
            }
        }
    }
}

fn expr_index_stack(input: &str) -> ParseResult<&str, Expr> {
    map(
        seq((
            expr_leaf,
            many0(delimited(
                seq((ws0, tag("["), ws0)),
                expr(false),
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

fn expr_call_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            expr_index_stack,
            many0(preceded(slws0, invocation_args(constrained))),
        )),
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
}

fn unary_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            many0(terminated(tag("!"), ws0)),
            expr_call_stack(constrained),
        )),
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
}

#[derive(Debug, Clone)]
enum TmpOp {
    IndexSugar(Expr),
    Unary {
        id: Identifier,
        additional_args: Vec<Argument>,
    },
    Binary(Identifier, Expr, Vec<Argument>),
}

fn postfix_index_sugar<'a>(input: &'a str) -> ParseResult<&'a str, TmpOp> {
    map(
        delimited(seq((ws0, tag(".["))), expr(false), tag("]")),
        TmpOp::IndexSugar,
    )
    .parse(input)
}

fn unary_postfix_fn<'a>(input: &'a str) -> ParseResult<&'a str, TmpOp> {
    map(
        seq((
            preceded(seq((ws0, tag("."))), identifier),
            optional(invocation_args(true)),
        )),
        |(id, additional_args)| TmpOp::Unary {
            id,
            additional_args: additional_args.unwrap_or(vec![]),
        },
    )
    .parse(input)
}

fn binary_infix_fn_latter_part<'a>(constrained: bool) -> impl Parser<&'a str, Output = TmpOp> {
    map(
        seq((
            preceded(seq((ws0, tag(":"))), identifier),
            optional(invocation_args(true)),
            preceded(ws1, unary_expr_stack(true)),
        )),
        |(id, additional_args, expr)| TmpOp::Binary(id, expr, additional_args.unwrap_or(vec![])),
    )
}

fn infix_or_postfix_fn_call_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            unary_expr_stack(constrained),
            many0(alt((
                postfix_index_sugar,
                unary_postfix_fn,
                binary_infix_fn_latter_part(constrained),
            ))),
        )),
        |(mut expr, ops)| {
            for op in ops {
                expr = match op {
                    TmpOp::IndexSugar(index_expr) => Expr::Invocation {
                        expr: Expr::Variable(Identifier("index".into())).into(),
                        args: vec![
                            Argument { name: None, expr },
                            Argument {
                                name: None,
                                expr: index_expr,
                            },
                        ],
                    },
                    TmpOp::Unary {
                        id,
                        additional_args,
                    } => Expr::Invocation {
                        expr: Expr::Variable(id).into(),
                        args: vec![vec![Argument { name: None, expr }], additional_args].concat(),
                    },
                    TmpOp::Binary(fn_name, right, additional_args) => Expr::Invocation {
                        expr: Expr::Variable(fn_name).into(),
                        args: vec![
                            vec![
                                Argument { name: None, expr },
                                Argument {
                                    name: None,
                                    expr: right.into(),
                                },
                            ],
                            additional_args,
                        ]
                        .concat(),
                    },
                };
            }
            expr
        },
    )
}

fn mul_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            infix_or_postfix_fn_call_stack(constrained),
            many0(seq((
                ws0,
                alt((tag("*"), tag("/"), tag("%"))),
                ws0,
                infix_or_postfix_fn_call_stack(constrained),
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
}

fn add_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            mul_expr_stack(constrained),
            many0(seq((
                ws0,
                alt((tag("+"), tag("-"), tag("<<"))),
                ws0,
                mul_expr_stack(constrained),
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
}

fn equ_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            add_expr_stack(constrained),
            many0(seq((
                ws0,
                alt((
                    tag("!="),
                    tag(">="),
                    tag("<="),
                    tag("=="),
                    tag("<"),
                    tag(">"),
                    tag("^"),
                )),
                ws0,
                add_expr_stack(constrained),
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
}

fn and_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            equ_expr_stack(constrained),
            many0(seq((
                ws0,
                alt((
                    tag("&&"),
                    //
                )),
                ws0,
                equ_expr_stack(constrained),
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
}

fn or_expr_stack<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    map(
        seq((
            and_expr_stack(constrained),
            many0(seq((
                ws0,
                alt((
                    tag("||"),
                    //
                )),
                ws0,
                and_expr_stack(constrained),
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
}

fn expr<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
    alt((if_expr, or_expr_stack(constrained)))
}

fn parameter_list(mut input: &str) -> ParseResult<&str, Vec<DeclarePattern>> {
    if let Some((rem, id)) = declare_pattern.parse(input) {
        let mut ids = vec![];
        let mut seen_comma = false;

        ids.push(id);
        input = rem;

        loop {
            if seen_comma && let Some((rem, id)) = preceded(ws0, declare_pattern).parse(input) {
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

fn return_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(seq((tag("return"), ws1, expr(false))), |(_, _, expr)| {
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

fn typespec(input: &str) -> ParseResult<&str, Type> {
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

fn assign_pattern(input: &str) -> ParseResult<&str, AssignPattern> {
    alt((
        map(
            seq((
                identifier,
                many0(delimited(
                    seq((ws0, tag("["), ws0)),
                    expr(false),
                    seq((ws0, tag("]"))),
                )),
            )),
            |(id, indexes)| {
                let mut pattern = AssignPattern::Id(id);

                for index in indexes {
                    pattern = AssignPattern::Index(pattern.into(), index.into());
                }

                pattern
            },
        ),
        map(
            listy("[", assign_pattern, assign_pattern, "]"),
            |elements| AssignPattern::List { elements },
        ),
        map(
            listy("(", assign_pattern, assign_pattern, ")"),
            |elements| AssignPattern::Tuple { elements },
        ),
    ))
    .parse(input)
}

fn declare_pattern(input: &str) -> ParseResult<&str, DeclarePattern> {
    alt((
        map(
            seq((
                identifier,
                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
            )),
            |(id, ty)| DeclarePattern::Id(id, ty),
        ),
        delimited(
            seq((tag("["), ws0)),
            map(
                optional(seq((
                    declare_pattern,
                    many0(preceded(seq((ws0, tag(","), ws0)), declare_pattern)),
                    ws0,
                    optional(preceded(
                        tag(","),
                        optional(delimited(
                            seq((ws0, tag(".."), ws0)),
                            seq((
                                identifier,
                                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
                            )),
                            optional(seq((ws0, tag(",")))),
                        )),
                    )),
                ))),
                |opt| match opt {
                    None => DeclarePattern::List {
                        elements: vec![],
                        rest: None,
                    },
                    Some((first, mut elements, _, rest)) => {
                        elements.insert(0, first);
                        DeclarePattern::List {
                            elements,
                            rest: rest.flatten(),
                        }
                    }
                },
            ),
            seq((ws0, tag("]"))),
        ),
        delimited(
            seq((tag("("), ws0)),
            map(
                optional(seq((
                    declare_pattern,
                    many0(preceded(seq((ws0, tag(","), ws0)), declare_pattern)),
                    ws0,
                    optional(preceded(
                        tag(","),
                        optional(delimited(
                            seq((ws0, tag(".."), ws0)),
                            seq((
                                identifier,
                                optional(preceded(seq((ws0, tag(":"), ws0)), typespec)),
                            )),
                            optional(seq((ws0, tag(",")))),
                        )),
                    )),
                ))),
                |opt| match opt {
                    None => DeclarePattern::Tuple {
                        elements: vec![],
                        rest: None,
                    },
                    Some((first, mut elements, _, rest)) => {
                        elements.insert(0, first);
                        DeclarePattern::Tuple {
                            elements,
                            rest: rest.flatten(),
                        }
                    }
                },
            ),
            seq((ws0, tag(")"))),
        ),
    ))
    .parse(input)
}

fn declare_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(
        seq((
            tag("let"),
            ws1,
            declare_pattern,
            ws0,
            tag("="),
            ws0,
            expr(false),
        )),
        |(_, _, pattern, _, _, _, expr)| Stmt::Declare {
            pattern,
            expr: expr.into(),
        },
    )
    .parse(input)
}

fn assign_stmt(input: &str) -> ParseResult<&str, Stmt> {
    map(
        seq((
            assign_pattern,
            ws0,
            optional(alt((
                tag("+"),
                tag("*"),
                tag("^"),
                tag("-"),
                tag("/"),
                tag("%"),
                tag("<<"),
            ))),
            tag("="),
            ws0,
            expr(false),
        )),
        |(location, _, op, _, _, expr)| Stmt::Assign {
            pattern: location.clone(),
            expr: match op {
                None => expr.into(),
                Some(op) => Expr::BinaryExpr {
                    left: Expr::from(location).into(),
                    op: op.into(),
                    right: expr.into(),
                }
                .into(),
            },
        },
    )
    .parse(input)
}

fn stmt(input: &str) -> ParseResult<&str, Stmt> {
    alt((
        return_stmt,
        declare_stmt,
        assign_stmt,
        map(expr(false), |expr| Stmt::Expr { expr: expr.into() }),
    ))
    .parse(input)
}

fn named_fn_item(input: &str) -> ParseResult<&str, Item> {
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

fn item(input: &str) -> ParseResult<&str, Item> {
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

fn block_contents(input: &str) -> ParseResult<&str, Block> {
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

fn document(input: &str) -> ParseResult<&str, Document> {
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

    fn list(elements: Vec<Expr>) -> Expr {
        Expr::ListLiteral { elements }
    }

    fn tuple(elements: Vec<Expr>) -> Expr {
        Expr::TupleLiteral { elements }
    }

    fn param(name: &str) -> DeclarePattern {
        DeclarePattern::Id(id(name), None)
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
                .map(|&name| DeclarePattern::Id(Identifier(name.into()), None))
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
                    DeclarePattern::Id(id("blue"), None),
                    DeclarePattern::Id(id("kelley"), None)
                ]
            ))
        );
        assert_eq!(
            parameter_list.parse("kelley ,,"),
            Some((",", vec![DeclarePattern::Id(id("kelley"), None)]))
        );
        assert_eq!(
            parameter_list.parse("kelley , blue , )"),
            Some((
                " )",
                vec![
                    DeclarePattern::Id(id("kelley"), None),
                    DeclarePattern::Id(id("blue"), None)
                ]
            ))
        );
        assert_eq!(expr(false).parse("kelley ?"), Some((" ?", var("kelley"))));
        assert_eq!(expr(false).parse("(kelley) ?"), Some((" ?", var("kelley"))));
        assert_eq!(
            expr(false).parse("(kelley,) ?"),
            Some((" ?", tuple(vec![var("kelley")])))
        );
        assert_eq!(
            expr(false).parse("(kelley, 21,) ?"),
            Some((" ?", tuple(vec![var("kelley"), int(21)])))
        );
        assert_eq!(
            expr(false).parse("kelley + 21 ?"),
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
                    pattern: Some(DeclarePattern::Id(id("h"), None)),
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
            expr(false).parse("kelley(12) + 21 ?"),
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
            expr(false).parse("kelley ( bla = 12, ) + 21 ?"),
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
            expr(false).parse("kelley ( bla = 12, ) || { } + 21 ?"),
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
            expr(false).parse("|| { } ?"),
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
            expr(false).parse("||{} ?"),
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
            expr(false).parse("|a| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    params: vec![DeclarePattern::Id(id("a"), None)],
                    body: Block {
                        items: vec![],
                        stmts: vec![]
                    }
                }
            ))
        );
        assert_eq!(
            expr(false).parse("|(a, b)| { } ?"),
            Some((
                " ?",
                Expr::AnonymousFn {
                    params: vec![DeclarePattern::Tuple {
                        elements: vec![
                            DeclarePattern::Id(id("a"), None),
                            DeclarePattern::Id(id("b"), None)
                        ],
                        rest: None,
                    }],
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
                    pattern: DeclarePattern::Id(id("h"), None),
                    expr: int(7).into()
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= -7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: DeclarePattern::Id(id("h"), None),
                    expr: int(-7).into()
                }
            ))
        );
        assert_eq!(
            stmt.parse("let h= !-7 ?"),
            Some((
                " ?",
                Stmt::Declare {
                    pattern: DeclarePattern::Id(id("h"), None),
                    expr: unary("!", int(-7)).into()
                }
            ))
        );
        assert_eq!(expr(false).parse(r#""world""#), Some(("", str("world"))));
        assert_eq!(expr(false).parse(r#"stdin"#), Some(("", var("stdin"))));
        assert_eq!(
            expr(false).parse(r#"stdin :split "\n\n""#),
            Some((
                "",
                simple_invocation("split", vec![var("stdin"), str("\n\n")])
            ))
        );
        assert_eq!(
            expr(false).parse(r#"stdin :split "\n\n" :map {}"#),
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
            expr(false).parse(r#"stdin :split "\n\n" :map |group| { group }"#),
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
            expr(false).parse(r#"stdin :split "\n\n" :map |group| { group } .max"#),
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
            expr(false).parse(r#"stdin :split "\n\n" :map |group| { group } .max :bla bla"#),
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
                    pattern: DeclarePattern::Id(id("v"), None),
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
                    pattern: DeclarePattern::Id(id("v"), None),
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
                    pattern: DeclarePattern::Id(id("v"), None),
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
                    pattern: DeclarePattern::Id(id("v"), None),
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
                    pattern: DeclarePattern::Id(id("v"), None),
                    expr: str("world").into()
                }
            ))
        );
        assert_eq!(
            stmt.parse(r#"let v = "world"[0]"#),
            Some((
                "",
                Stmt::Declare {
                    pattern: DeclarePattern::Id(id("v"), None),
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
                    pattern: DeclarePattern::Id(id("v"), None),
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
                            pattern: AssignPattern::Id(id("h")),
                            expr: Expr::Numeric(Numeric::Int(1)).into()
                        }]
                    }
                }
            ))
        );

        let if_block = Expr::If {
            pattern: None,
            cond: Expr::BinaryExpr {
                left: Expr::Variable(id("d")).into(),
                op: "==".into(),
                right: Expr::Numeric(Numeric::Int(0)).into(),
            }
            .into(),
            then: Block {
                items: vec![],
                stmts: vec![Stmt::Assign {
                    pattern: AssignPattern::Id(id("n")),
                    expr: Expr::Numeric(Numeric::Int(0)).into(),
                }],
            },
            els: Some(Block {
                items: vec![],
                stmts: vec![Stmt::Assign {
                    pattern: AssignPattern::Id(id("n")),
                    expr: Expr::BinaryExpr {
                        left: Expr::Variable(id("n")).into(),
                        op: "+".into(),
                        right: Expr::Variable(id("d")).into(),
                    }
                    .into(),
                }],
            }),
        };

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
                    params: vec![DeclarePattern::Id(id("start"), None)],
                    body: Block {
                        items: vec![],
                        stmts: vec![
                            Stmt::Declare {
                                pattern: DeclarePattern::Id(id("n"), None),
                                expr: Expr::Variable(id("start")).into()
                            },
                            Stmt::Expr {
                                expr: Expr::AnonymousFn {
                                    params: vec![DeclarePattern::Id(id("d"), None)],
                                    body: Block {
                                        items: vec![],
                                        stmts: vec![Stmt::Expr {
                                            expr: if_block.clone().into()
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
            expr(false).parse(
                r#"if (d == 0) {
                    n = 0
                } else {
                    n = n + d
                }"#
            ),
            Some(("", if_block.clone().into()))
        );

        assert_eq!(
            expr(false).parse(
                r#"if (d == 0) {
                    n = 0
                } else if (d == 0) {
                    n = 0
                } else {
                    n = n + d
                }"#
            ),
            Some((
                "",
                Expr::If {
                    pattern: None,
                    cond: Expr::BinaryExpr {
                        left: Expr::Variable(id("d")).into(),
                        op: "==".into(),
                        right: Expr::Numeric(Numeric::Int(0)).into(),
                    }
                    .into(),
                    then: Block {
                        items: vec![],
                        stmts: vec![Stmt::Assign {
                            pattern: AssignPattern::Id(id("n")),
                            expr: Expr::Numeric(Numeric::Int(0)).into(),
                        }],
                    },
                    els: Some(Block {
                        items: vec![],
                        stmts: vec![Stmt::Expr {
                            expr: if_block.clone().into()
                        }],
                    }),
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
                        pattern: DeclarePattern::Id(id("h"), None),
                        expr: Expr::Numeric(Numeric::Int(7)).into()
                    }]
                }
            ))
        );
        assert_eq!(
            block_contents.parse(
                "h+= 7 

5 ?"
            ),
            Some((
                " ?",
                Block {
                    items: vec![],
                    stmts: vec![
                        Stmt::Assign {
                            pattern: AssignPattern::Id(id("h")),
                            expr: binary("+", var("h"), Expr::Numeric(Numeric::Int(7))).into()
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
                            pattern: DeclarePattern::Id(id("h"), Some(Type::Numeric)),
                            expr: Expr::Numeric(Numeric::Int(7)).into()
                        },
                        Stmt::Assign {
                            pattern: AssignPattern::Id(id("kelley")),
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
                        pattern: DeclarePattern::Id(id("h"), None),
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
                    pattern: DeclarePattern::Id(id("h"), None),
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
                        pattern: DeclarePattern::Id(id("i"), None),
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
                                pattern: DeclarePattern::Id(id("v"), None),
                                expr: str("world").into()
                            },
                            Stmt::Declare {
                                pattern: DeclarePattern::Id(id("h"), None),
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

    #[test]
    fn trailing_anon_vs_separate_stmt_ambiguity() {
        let doc_1 = parse_document(
            r#"
fn make_closure() {
  let rules = input
    :map |n| {
      "hi im closure"
    }

  |n| {
    "hi im closure"
  }
}
        "#,
        )
        .expect("should parse");

        match doc_1 {
            Document {
                body: Block { mut items, .. },
            } => match items.pop().unwrap() {
                Item::NamedFn {
                    body: Block { stmts, .. },
                    ..
                } => {
                    assert_eq!(stmts.len(), 2);
                    assert!(matches!(&stmts[0], &Stmt::Declare { .. }));
                    assert!(matches!(
                        &stmts[1],
                        &Stmt::Expr {
                            expr: box Expr::AnonymousFn { .. }
                        }
                    ));
                }
            },
        }

        //         let doc_2 = parse_document(
        //             r#"
        // fn make_closure() {
        //   let rules = input
        //     :map |n| {
        //       "hi im closure"
        //     } |n| {
        //       "hi im closure"
        //     }
        // }
        //         "#,
        //         )
        //         .expect("should parse");

        //         match doc_2 {
        //             Document {
        //                 body: Block { mut items, .. },
        //             } => match items.pop().unwrap() {
        //                 Item::NamedFn {
        //                     body: Block { stmts, .. },
        //                     ..
        //                 } => {
        //                     assert_eq!(stmts.len(), 1);
        //                     assert_eq!(
        //                         stmts,
        //                         vec![Stmt::Declare {
        //                             pattern: DeclarePattern::Id(id("rules"), None),
        //                             expr: simple_invocation(
        //                                 "map",
        //                                 vec![
        //                                     var("input"),
        //                                     Expr::Invocation {
        //                                         expr: Expr::AnonymousFn {
        //                                             params: vec![DeclarePattern::Id(id("n"), None)],
        //                                             body: Block {
        //                                                 items: vec![],
        //                                                 stmts: vec![Stmt::Expr {
        //                                                     expr: str("hi im closure").into()
        //                                                 }]
        //                                             }
        //                                         }
        //                                         .into(),
        //                                         args: vec![Argument {
        //                                             name: None,
        //                                             expr: Expr::AnonymousFn {
        //                                                 params: vec![DeclarePattern::Id(id("n"), None)],
        //                                                 body: Block {
        //                                                     items: vec![],
        //                                                     stmts: vec![Stmt::Expr {
        //                                                         expr: str("hi im closure").into()
        //                                                     }]
        //                                                 }
        //                                             }
        //                                         }]
        //                                     }
        //                                 ]
        //                             )
        //                             .into()
        //                         }]
        //                     );
        //                 }
        //             },
        //         }
    }

    #[test]
    fn infix_fns() {
        let anonymous_fn = Expr::AnonymousFn {
            params: vec![param("a"), param("b")],
            body: Block {
                items: vec![],
                stmts: vec![Stmt::Expr {
                    expr: binary("*", var("a"), var("b")).into(),
                }],
            },
        };

        // This is the intended syntax / usage
        // --
        // There's no space between `fold` and `(1)`, which allows the `1` to be an additional param to `fold`
        assert_eq!(
            expr(false).parse("[1, 2, 3] :fold(1) |a, b| { a * b }"),
            Some((
                "",
                Expr::Invocation {
                    expr: var("fold").into(),
                    args: vec![
                        Argument {
                            name: None,
                            expr: list(vec![int(1), int(2), int(3)])
                        },
                        Argument {
                            name: None,
                            expr: anonymous_fn.clone()
                        },
                        Argument {
                            name: None,
                            expr: int(1)
                        }
                    ]
                }
            ))
        );

        // This is probably the programmer's syntax mistake
        // ---
        // There's a space between `fold` and `(1)`, which makes the `1` ineligible for additional params.
        assert_eq!(
            expr(false).parse("[1, 2, 3] :fold (1) |a, b| { a * b }"),
            Some((
                " |a, b| { a * b }",
                Expr::Invocation {
                    expr: var("fold").into(),
                    args: vec![
                        Argument {
                            name: None,
                            expr: list(vec![int(1), int(2), int(3)])
                        },
                        Argument {
                            name: None,
                            expr: int(1)
                        }
                    ]
                }
            ))
        );
    }
}
