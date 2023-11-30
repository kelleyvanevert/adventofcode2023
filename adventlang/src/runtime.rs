use std::{collections::HashMap, fmt::Display};

use crate::ast::{Block, Document, Expr, Identifier, Item, Stmt, StrLiteralPiece};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeError(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str(pub String);

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

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

    fn max(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a.max(b.get_double())),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double().max(b)),

            (Numeric::Int(a), b) => Numeric::Int(*a.max(&b.get_int())),
            (a, Numeric::Int(b)) => Numeric::Int(a.get_int().max(b)),

            (Numeric::UInt(a), Numeric::UInt(b)) => Numeric::UInt(*a.max(&b)),
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
pub struct FnDef {
    parent_scope: usize,
    params: Vec<Identifier>,
    body: FnBody,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Code(Block),
    Builtin(fn(&mut Runtime, usize) -> Result<Value, RuntimeError>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Unit,
    Bool(bool),
    Str(Str),
    Numeric(Numeric),
    FnDef(FnDef),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(str) => write!(f, "{}", str.0),
            Value::Numeric(num) => write!(f, "{num}"),
            Value::FnDef(_) => write!(f, "[fn]"),
            Value::List(list) => {
                write!(f, "[")?;
                let len = list.len();
                for (i, item) in list.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < len - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Type {
    Nil,
    Unit,
    Bool,
    Str,
    Numeric,
    FnDef,
    List,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Nil => write!(f, "Nil"),
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::Numeric => write!(f, "Numeric"),
            Type::FnDef => write!(f, "FnDef"),
            Type::List => write!(f, "List"),
        }
    }
}

impl Value {
    fn negate(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Nil => Ok(Value::Nil),
            Value::Unit => Ok(Value::Unit),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => Err(RuntimeError(format!("Can't negate str"))),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            Value::FnDef(_) => Err(RuntimeError(format!("Can't negate fndef"))),
            Value::List(_) => Err(RuntimeError(format!("Can't negate list"))),
        }
    }

    fn add(&self, other: Value) -> Result<Value, RuntimeError> {
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

    fn max(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Nil) => Ok(Value::Numeric(a.clone())),
            (Value::Nil, Value::Numeric(b)) => Ok(Value::Numeric(b)),
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.max(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} max {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    fn lt(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a.0.len() < b.0.len())),
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Bool(a.get_double() < b.get_double()))
            }
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    fn eq(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            // (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a.0.len() < b.0.len())),
            (Value::Numeric(a), Value::Numeric(b)) => {
                Ok(Value::Bool(a.get_double() == b.get_double()))
            }
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
            Value::Nil => "".into(),
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
            Value::List(_) => "<list>".into(),
        }
    }

    // TODO
    fn auto_coerce_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Nil => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
            Value::Unit => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
            Value::Bool(b) => {
                if *b {
                    Ok(1)
                } else {
                    Ok(0)
                }
            }
            Value::Str(str) => {
                return str
                    .0
                    .parse::<i64>()
                    .map_err(|_| RuntimeError(format!("cannot coerce '{}' to int", str.0)));
            }
            Value::Numeric(n) => match n {
                Numeric::Double(_) => Err(RuntimeError(format!("cannot coerce double to int"))),
                Numeric::Int(n) => Ok(*n),
                Numeric::UInt(n) => Ok(*n as i64),
            },
            Value::FnDef(_) => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
            Value::List(_) => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
        }
    }

    fn ty(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Str(_) => Type::Str,
            Value::Numeric(_) => Type::Numeric,
            Value::FnDef(_) => Type::FnDef,
            Value::List(_) => Type::List,
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
struct Scope {
    parent_scope: Option<usize>,
    values: HashMap<Identifier, Value>,
}

pub struct Runtime {
    scopes: Vec<Scope>,
}

impl Runtime {
    fn new() -> Runtime {
        Runtime {
            scopes: vec![Scope {
                parent_scope: None,
                values: HashMap::new(),
            }],
        }
    }

    fn lookup(&self, scope_id: usize, id: &Identifier) -> Option<(usize, Value)> {
        let scope = self.scopes.get(scope_id)?;
        if let Some(value) = scope.values.get(id) {
            return Some((scope_id, value.clone()));
        }

        if let Some(parent_scope_id) = scope.parent_scope {
            self.lookup(parent_scope_id, id)
        } else {
            None
        }
    }

    fn execute_block(
        &mut self,
        scope: usize,
        block: &Block,
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        let mut result = Value::Unit;
        let mut ret = None;

        for item in &block.items {
            self.define(scope, &item)?;
        }

        for stmt in &block.stmts {
            (result, ret) = self.execute(scope, &stmt)?;
            if ret.is_some() {
                return Ok((Value::Unit, ret));
            }
        }

        Ok((result, None))
    }

    fn define(&mut self, scope: usize, item: &Item) -> Result<(), RuntimeError> {
        match item {
            Item::NamedFn { name, params, body } => {
                self.scopes[scope].values.insert(
                    name.clone(),
                    Value::FnDef(FnDef {
                        parent_scope: scope,
                        params: params.clone(),
                        body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                    }),
                );
            }
        }

        Ok(())
    }

    fn execute(
        &mut self,
        scope: usize,
        stmt: &Stmt,
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        match stmt {
            Stmt::Return { expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }

                Ok((Value::Unit, Some(value)))
            }
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Declare { id, expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }

                self.scopes[scope].values.insert(id.clone(), value);

                Ok((Value::Unit, None))
            }
            Stmt::Assign { id, expr } => {
                let Some((def_scope, _)) = self.lookup(scope, id) else {
                    return Err(RuntimeError(format!(
                        "cannot assign to undefined var: {id}"
                    )));
                };

                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }

                self.scopes[def_scope]
                    .values
                    .insert(id.clone(), value.clone());

                Ok((value, None))
            }
        }
    }

    fn invoke(
        &mut self,
        def: FnDef,
        args: Vec<(Option<Identifier>, Value)>,
    ) -> Result<Value, RuntimeError> {
        let execution_scope = self.scopes.len();
        self.scopes.push(Scope {
            parent_scope: Some(def.parent_scope),
            values: HashMap::new(),
        });

        let mut params_remaining = def.params.clone();
        for (arg_name, arg_value) in args {
            if let Some(name) = arg_name {
                // assign named param
                match params_remaining.iter().position(|p| p == &name) {
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
            } else if let Some(name) = params_remaining.get(0).cloned() {
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

        Ok(match def.body {
            FnBody::Code(block) => {
                let (block_eval_result, ret) = self.execute_block(execution_scope, &block)?;
                match ret {
                    Some(return_value) => return_value,
                    None => block_eval_result,
                }
            }
            FnBody::Builtin(f) => f(self, execution_scope)?,
        })
    }

    fn evaluate(
        &mut self,
        scope: usize,
        expr: &Expr,
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        match expr {
            Expr::StrLiteral { pieces } => {
                let mut build = "".to_string();

                for piece in pieces {
                    match piece {
                        StrLiteralPiece::Fragment(fragment) => {
                            build += fragment;
                        }
                        StrLiteralPiece::Interpolation(expr) => {
                            let (value, ret) = self.evaluate(scope, expr)?;
                            if let Some(return_value) = ret {
                                return Ok((Value::Unit, Some(return_value)));
                            }
                            build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok((Value::Str(Str(build)), None))
            }
            Expr::Numeric(num) => Ok((Value::Numeric(num.clone()), None)),
            Expr::Variable(id) => {
                let Some((_, value)) = self.lookup(scope, id) else {
                    return Err(RuntimeError(format!("Could not find id: {id}")));
                };

                Ok((value, None))
            }
            Expr::UnaryExpr { expr, op } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }
                match op.as_str() {
                    "!" => Ok((value.negate()?, None)),
                    _ => Err(RuntimeError(format!("Unknown unary operation: {op}"))),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let (left_value, ret) = self.evaluate(scope, left)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }
                match op.as_str() {
                    "+" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Unit, Some(return_value)));
                        }
                        Ok((left_value.add(right_value)?, None))
                    }
                    "<" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Unit, Some(return_value)));
                        }
                        Ok((left_value.lt(right_value)?, None))
                    }
                    "==" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Unit, Some(return_value)));
                        }
                        Ok((left_value.eq(right_value)?, None))
                    }
                    _ => Err(RuntimeError(format!("Unknown binary operation: {op}"))),
                }
            }
            Expr::Invocation { expr, args } => {
                let (expr_value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Unit, Some(return_value)));
                }
                let Value::FnDef(def) = expr_value else {
                    return Err(RuntimeError(format!("cannot call {}", expr_value.ty())));
                };

                let mut evaluated_args = vec![];
                for arg in args {
                    let (arg_value, ret) = self.evaluate(scope, &arg.expr)?;
                    if let Some(return_value) = ret {
                        return Ok((Value::Unit, Some(return_value)));
                    }
                    evaluated_args.push((arg.name.clone(), arg_value));
                }

                Ok((self.invoke(def, evaluated_args)?, None))
            }
            Expr::AnonymousFn { params, body } => Ok((
                Value::FnDef(FnDef {
                    parent_scope: scope,
                    params: params.clone(),
                    body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                }),
                None,
            )),
            Expr::If { cond, then, els } => {
                let (cond_value, ret) = self.evaluate(scope, cond)?;
                if ret.is_some() {
                    return Ok((Value::Unit, ret));
                }

                let mut result = Value::Unit;
                let mut ret = None;
                if cond_value.auto_coerce_bool()? {
                    (result, ret) = self.execute_block(scope, then)?;
                    if ret.is_some() {
                        return Ok((Value::Unit, ret));
                    }
                } else if let Some(els) = els {
                    (result, ret) = self.execute_block(scope, els)?;
                    if ret.is_some() {
                        return Ok((Value::Unit, ret));
                    }
                }

                Ok((result, None))
            }
            Expr::While { cond, body } => {
                let mut result = Value::Unit;
                let mut ret = None;
                loop {
                    let (cond_value, cond_ret) = self.evaluate(scope, cond)?;
                    if let Some(return_value) = cond_ret {
                        return Ok((Value::Unit, Some(return_value)));
                    }
                    if !cond_value.auto_coerce_bool()? {
                        return Ok((result, None));
                    }

                    (result, ret) = self.execute_block(scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Unit, ret));
                    }
                }
            }
            Expr::DoWhile { cond, body } => loop {
                loop {
                    let mut result = Value::Unit;
                    let mut ret = None;

                    (result, ret) = self.execute_block(scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Unit, ret));
                    }

                    if let Some(cond) = cond {
                        let (cond_value, ret) = self.evaluate(scope, cond)?;
                        if ret.is_some() {
                            return Ok((Value::Unit, ret));
                        }
                        if !cond_value.auto_coerce_bool()? {
                            return Ok((result, None));
                        }
                    } else {
                        return Ok((result, None));
                    }
                }
            },
            Expr::Loop { body } => loop {
                loop {
                    let (_, ret) = self.execute_block(scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Unit, ret));
                    }
                }
            },
        }
    }
}

pub fn execute(doc: &Document, stdin: String) -> Result<Value, RuntimeError> {
    let mut runtime = Runtime::new();

    runtime.scopes[0]
        .values
        .insert(id("stdin"), Value::Str(Str(stdin)));

    runtime.scopes[0].values.insert(
        id("print"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                println!("{}", text);
                Ok(Value::Unit)
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("run"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("f")],
            body: FnBody::Builtin(|runtime, scope| {
                let f = runtime.scopes[scope].values.get(&id("f")).unwrap();

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

    runtime.scopes[0].values.insert(
        id("max"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                match items {
                    Value::List(list) => {
                        let mut result = Value::Nil;
                        for item in list.iter() {
                            result = result.max(item.clone())?;
                        }

                        Ok(result)
                    }
                    _ => Err(RuntimeError(format!("cannot get max of: {}", items.ty()))),
                }
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("map"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items"), id("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let list = list.clone();

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "cannot use map w/ cb of type: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                let mut result = vec![];
                for item in list.iter() {
                    result.push(runtime.invoke(def.clone(), vec![(None, item.clone())])?);
                }

                Ok(Value::List(result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("sum"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let mut result = Value::Numeric(Numeric::UInt(0));
                for item in list.iter() {
                    result = result.add(item.clone())?;
                }

                Ok(result)
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("split"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text"), id("sep")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                let Value::Str(text) = text else {
                    return Err(RuntimeError(format!(
                        "split[#1] must be a string, is a: {}",
                        text.ty()
                    )));
                };

                let sep = runtime.scopes[scope].values.get(&id("sep")).unwrap();

                let Value::Str(sep) = sep else {
                    return Err(RuntimeError(format!(
                        "split[#2] must be a string, is a: {}",
                        sep.ty()
                    )));
                };

                let result = text
                    .0
                    .split(&sep.0)
                    .map(|piece| Value::Str(Str(piece.to_string())))
                    .collect::<Vec<_>>();

                Ok(Value::List(result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("trim"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                let Value::Str(text) = text else {
                    return Err(RuntimeError(format!(
                        "trim[#1] must be a string, is a: {}",
                        text.ty()
                    )));
                };

                Ok(Value::Str(Str(text.0.trim().to_string())))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("int"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.scopes[scope].values.get(&id("data")).unwrap();

                let result = data.auto_coerce_int()?;

                Ok(Value::Numeric(Numeric::Int(result)))
            }),
        }),
    );

    let (body_eval_result, ret) = runtime.execute_block(0, &doc.body)?;
    let result = match ret {
        Some(return_value) => return_value,
        None => body_eval_result,
    };

    Ok(result)
}
