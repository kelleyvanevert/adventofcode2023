use std::{collections::HashMap, fmt::Display};

use crate::ast::{Document, Expr, Identifier, Stmt, StrLiteralPiece};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeError(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str(pub String);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Numeric {
    UInt(u64),
    Int(i64),
    Double(f64),
}

impl Numeric {
    fn negate(&self) -> Result<Numeric, RuntimeError> {
        match self {
            Numeric::UInt(n) if *n == 0 => Ok(Numeric::UInt(0)),
            Numeric::UInt(n) => Err(RuntimeError(format!("Could not negate uint {n}"))),
            Numeric::Int(n) => Ok(Numeric::Int(-n)),
            Numeric::Double(n) => Ok(Numeric::Double(-n)),
        }
    }

    fn add(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a + b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() + b),

            (Numeric::Int(a), b) => Numeric::Int(a + b.get_int()),
            (a, Numeric::Int(b)) => Numeric::Int(a.get_int() + b),

            (Numeric::UInt(a), Numeric::UInt(b)) => Numeric::UInt(a + b),
        }
    }

    fn get_double(&self) -> f64 {
        match self {
            Numeric::UInt(a) => *a as f64,
            Numeric::Int(a) => *a as f64,
            Numeric::Double(a) => *a as f64,
        }
    }

    fn get_int(&self) -> i64 {
        match self {
            Numeric::UInt(a) => *a as i64,
            Numeric::Int(a) => *a as i64,
            Numeric::Double(a) => *a as i64,
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::UInt(n) => write!(f, "{n}"),
            Numeric::Int(n) => write!(f, "{n}"),
            Numeric::Double(n) => write!(f, "{n}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef<'a> {
    parent_scope: usize,
    params: Vec<Identifier<'a>>,
    body: FnBody<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody<'a> {
    Code(Vec<Stmt<'a>>),
    Builtin(fn(&mut Runtime<'a>, usize) -> Result<Value<'a>, RuntimeError>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Unit,
    Bool(bool),
    Str(Str),
    Numeric(Numeric),
    FnDef(FnDef<'a>),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(str) => write!(f, "{}", str.0),
            Value::Numeric(num) => write!(f, "{num}"),
            Value::FnDef(_) => write!(f, "[fn]"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Type {
    Unit,
    Bool,
    Str,
    Numeric,
    FnDef,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::Numeric => write!(f, "Numeric"),
            Type::FnDef => write!(f, "FnDef"),
        }
    }
}

impl<'a> Value<'a> {
    fn negate(&self) -> Result<Value<'a>, RuntimeError> {
        match self {
            Value::Unit => Err(RuntimeError(format!("Can't negate unit"))),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => Err(RuntimeError(format!("Can't negate str"))),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            Value::FnDef(_) => Err(RuntimeError(format!("Can't negate fndef"))),
        }
    }

    fn add(&self, other: Value<'a>) -> Result<Value<'a>, RuntimeError> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(Str(a.0.to_string() + &b.0))),
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.add(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    // TODO
    fn auto_coerce_str(&self) -> String {
        match self {
            Value::Unit => "".into(),
            Value::Bool(b) => {
                if *b {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            Value::Str(str) => str.0.clone(),
            Value::Numeric(n) => format!("{n}"),
            Value::FnDef(_) => "<fn>".into(),
        }
    }

    fn ty(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Str(_) => Type::Str,
            Value::Numeric(_) => Type::Numeric,
            Value::FnDef(_) => Type::FnDef,
        }
    }

    fn auto_coerce_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError(format!("cannot coerce {}", self.ty()))),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Scope<'a> {
    parent_scope: Option<usize>,
    values: HashMap<Identifier<'a>, Value<'a>>,
}

pub struct Runtime<'a> {
    scopes: Vec<Scope<'a>>,
}

impl<'a> Runtime<'a> {
    fn new() -> Runtime<'a> {
        Runtime {
            scopes: vec![Scope {
                parent_scope: None,
                values: HashMap::new(),
            }],
        }
    }

    fn lookup(&self, scope_id: usize, id: &Identifier<'a>) -> Option<Value<'a>> {
        let scope = self.scopes.get(scope_id)?;
        if let Some(value) = scope.values.get(id) {
            return Some(value.clone());
        }

        if let Some(parent_scope_id) = scope.parent_scope {
            self.lookup(parent_scope_id, id)
        } else {
            None
        }
    }

    fn execute(&mut self, scope: usize, stmt: &Stmt<'a>) -> Result<Value<'a>, RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Assign { id, expr } => {
                let value = self.evaluate(scope, expr)?;

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .values
                    .insert(*id, value);

                Ok(Value::Unit)
            }
        }
    }

    fn invoke(
        &mut self,
        def: FnDef<'a>,
        args: Vec<(Option<Identifier<'a>>, Value<'a>)>,
    ) -> Result<Value<'a>, RuntimeError> {
        let execution_scope = self.scopes.len();
        self.scopes.push(Scope {
            parent_scope: Some(def.parent_scope),
            values: HashMap::new(),
        });

        let mut params_remaining = def.params.clone();
        for (arg_name, arg_value) in args {
            if let Some(name) = arg_name {
                // assign named param
                match params_remaining.iter().position(|&p| p == name) {
                    Some(i) => {
                        params_remaining.remove(i);
                        self.scopes[execution_scope].values.insert(name, arg_value);
                    }
                    None => {
                        return Err(RuntimeError(format!(
                            "cannot pass named arg {name} (no param with that name)"
                        )));
                    }
                }
            } else if let Some(&name) = params_remaining.get(0) {
                // assign next available param
                params_remaining.remove(0);
                self.scopes[execution_scope].values.insert(name, arg_value);
            } else {
                // no params left
                return Err(RuntimeError(format!("no param to pass arg to")));
            }
        }

        if params_remaining.len() > 0 {
            // unassigned params
            return Err(RuntimeError(format!("unassigned params left")));
        }

        let mut result = Value::Unit;
        match def.body {
            FnBody::Code(stmts) => {
                for stmt in stmts {
                    result = self.execute(execution_scope, &stmt)?;
                }
            }
            FnBody::Builtin(f) => {
                result = f(self, execution_scope)?;
            }
        }

        Ok(result)
    }

    fn evaluate(&mut self, scope: usize, expr: &Expr<'a>) -> Result<Value<'a>, RuntimeError> {
        match expr {
            Expr::StrLiteral { pieces } => {
                let mut build = "".to_string();

                for piece in pieces {
                    match piece {
                        StrLiteralPiece::Fragment(fragment) => {
                            build += fragment;
                        }
                        StrLiteralPiece::Interpolation(expr) => {
                            build += &self.evaluate(scope, expr)?.auto_coerce_str();
                        }
                    }
                }

                Ok(Value::Str(Str(build)))
            }
            Expr::Numeric(num) => Ok(Value::Numeric(num.clone())),
            Expr::Variable(id) => {
                let Some(value) = self.lookup(scope, id) else {
                    return Err(RuntimeError(format!("Could not find id: {id}")));
                };

                Ok(value)
            }
            Expr::UnaryExpr { expr, op } => {
                let value = self.evaluate(scope, expr)?;
                match op {
                    &"!" => value.negate(),
                    _ => Err(RuntimeError(format!("Unknown unary operation: {op}"))),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let left_value = self.evaluate(scope, left)?;
                match op {
                    &"+" => left_value.add(self.evaluate(scope, right)?),
                    _ => Err(RuntimeError(format!("Unknown binary operation: {op}"))),
                }
            }
            Expr::Invocation { expr, args } => {
                let expr_value = self.evaluate(scope, expr)?;
                let Value::FnDef(def) = expr_value else {
                    return Err(RuntimeError(format!("cannot call {}", expr_value.ty())));
                };

                let mut evaluated_args = vec![];
                for arg in args {
                    let arg_value = self.evaluate(scope, &arg.expr)?;
                    evaluated_args.push((arg.name, arg_value));
                }

                self.invoke(def, evaluated_args)
            }
            Expr::AnonymousFn { params, body } => Ok(Value::FnDef(FnDef {
                parent_scope: scope,
                params: params.clone(),
                body: FnBody::Code(body.clone()),
            })),
            Expr::If { cond, then, els } => {
                let mut result = Value::Unit;

                let cond_value = self.evaluate(scope, cond)?.auto_coerce_bool()?;
                if cond_value {
                    for stmt in then {
                        result = self.execute(scope, stmt)?;
                    }
                } else if let Some(stmts) = els {
                    for stmt in stmts {
                        result = self.execute(scope, stmt)?;
                    }
                }

                Ok(result)
            }
        }
    }
}

pub fn execute<'a>(doc: &'a Document<'a>, stdin: String) -> Result<Value<'a>, RuntimeError> {
    let mut runtime = Runtime::new();

    runtime.scopes[0]
        .values
        .insert(Identifier("stdin"), Value::Str(Str(stdin)));

    runtime.scopes[0].values.insert(
        Identifier("print"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![Identifier("text")],
            body: FnBody::Builtin(|runtime, scope| {
                println!(
                    "{}",
                    runtime.scopes[scope]
                        .values
                        .get(&Identifier("text"))
                        .unwrap()
                );
                Ok(Value::Unit)
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        Identifier("run"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![Identifier("f")],
            body: FnBody::Builtin(|runtime, scope| {
                let f = runtime.scopes[scope].values.get(&Identifier("f")).unwrap();
                match f {
                    Value::FnDef(def) => {
                        if def.params.len() > 0 {
                            return Err(RuntimeError(format!("cannot run fn w/ params")));
                        }

                        runtime.invoke(def.clone(), vec![])
                    }
                    _ => Err(RuntimeError(format!("cannot run: {}", f.ty()))),
                }
            }),
        }),
    );

    let mut result = Value::Unit;

    for stmt in &doc.body {
        result = runtime.execute(0, stmt)?;
    }

    Ok(result)
}
