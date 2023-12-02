use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::Display,
};

use arcstr::Substr;

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
    Int(i64),
    Double(f64),
}

impl Numeric {
    fn negate(&self) -> Result<Numeric, RuntimeError> {
        match self {
            Numeric::Int(n) => Ok(Numeric::Int(-n)),
            Numeric::Double(n) => Ok(Numeric::Double(-n)),
        }
    }

    fn add(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a + b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() + b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a + b),
        }
    }

    fn max(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a.max(b.get_double())),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double().max(b)),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(*a.max(&b)),
        }
    }

    fn get_double(&self) -> f64 {
        match self {
            Numeric::Int(a) => *a as f64,
            Numeric::Double(a) => *a as f64,
        }
    }

    fn get_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Numeric::Int(a) => Ok(*a as i64),
            Numeric::Double(_) => Err(RuntimeError(
                "value is a double, cannot be converted to an int".to_string(),
            )),
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
    Bool(bool),
    Str(Substr),
    Numeric(Numeric),
    FnDef(FnDef),
    List(Type, Vec<Value>),
    Tuple(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(str) => write!(f, "{}", str),
            Value::Numeric(num) => write!(f, "{num}"),
            Value::FnDef(_) => write!(f, "[fn]"),
            Value::List(_, list) => {
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
            Value::Tuple(list) => {
                write!(f, "(")?;
                let len = list.len();
                for (i, item) in list.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < len - 1 {
                        write!(f, ", ")?;
                    }
                }
                if len == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Nil,
    Bool,
    Str,
    Numeric,
    FnDef,
    List(Box<Type>),
    Tuple,
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

    fn narrow(&self, other: &Type) -> Option<Type> {
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

        let simple = [Type::Nil, Type::Bool, Type::Numeric, Type::Str, Type::Tuple];

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
                println!("REL");
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
            Type::FnDef => write!(f, "FnDef"),
            Type::List(t) => write!(f, "List[{t}]"),
            Type::Tuple => write!(f, "Tuple"),
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

impl Value {
    fn negate(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Nil => Ok(Value::Nil),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => Err(RuntimeError(format!("Can't negate str"))),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            Value::FnDef(_) => Err(RuntimeError(format!("Can't negate fndef"))),
            Value::List(_, _) => Err(RuntimeError(format!("Can't negate list"))),
            Value::Tuple(_) => Err(RuntimeError(format!("Can't negate tuple"))),
        }
    }

    fn add(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => {
                let new = Substr::from(a.to_string() + &b);
                Ok(Value::Str(new))
            }
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
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a.len() < b.len())),
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
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a == &b)),
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
            Value::Bool(b) => {
                if *b {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            Value::Str(str) => str.to_string(),
            Value::Numeric(n) => format!("{n}"),
            Value::FnDef(_) => "<fn>".into(),
            Value::List(_, _) => "<list>".into(),
            Value::Tuple(_) => "<tuple>".into(),
        }
    }

    // TODO
    fn auto_coerce_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Nil => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
            Value::Bool(b) => {
                if *b {
                    Ok(1)
                } else {
                    Ok(0)
                }
            }
            Value::Str(str) => {
                return str
                    .parse::<i64>()
                    .map_err(|_| RuntimeError(format!("cannot coerce '{}' to int", str)));
            }
            Value::Numeric(n) => n.get_int(),
            _ => Err(RuntimeError(format!("cannot coerce {} to int", self.ty()))),
        }
    }

    fn ty(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Bool(_) => Type::Bool,
            Value::Str(_) => Type::Str,
            Value::Numeric(_) => Type::Numeric,
            Value::FnDef(_) => Type::FnDef,
            Value::List(t, _) => Type::List(t.clone().into()),
            Value::Tuple(_) => Type::Tuple,
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

    fn lookup(&self, scope_id: usize, id: &Identifier) -> Option<(usize, &Value)> {
        let scope = self.scopes.get(scope_id)?;
        if let Some(value) = scope.values.get(id) {
            return Some((scope_id, value));
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
        let mut result = Value::Nil;
        let mut ret = None;

        for item in &block.items {
            self.define(scope, &item)?;
        }

        for stmt in &block.stmts {
            (result, ret) = self.execute(scope, &stmt)?;
            if ret.is_some() {
                return Ok((Value::Nil, ret));
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
                    return Ok((Value::Nil, Some(return_value)));
                }

                Ok((Value::Nil, Some(value)))
            }
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Declare { id, expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }

                self.scopes[scope].values.insert(id.clone(), value);

                Ok((Value::Nil, None))
            }
            Stmt::Assign { id, expr } => {
                let Some((def_scope, _)) = self.lookup(scope, id) else {
                    return Err(RuntimeError(format!(
                        "cannot assign to undefined var: {id}"
                    )));
                };

                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
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
            Expr::NilLiteral => Ok((Value::Nil, None)),
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
                                return Ok((Value::Nil, Some(return_value)));
                            }
                            build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok((Value::Str(Substr::from(build)), None))
            }
            Expr::Numeric(num) => Ok((Value::Numeric(num.clone()), None)),
            Expr::Variable(id) => {
                let Some((_, value)) = self.lookup(scope, id) else {
                    return Err(RuntimeError(format!("Could not find id: {id}")));
                };

                Ok((value.clone(), None))
            }
            Expr::UnaryExpr { expr, op } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }
                match op.as_str() {
                    "!" => Ok((value.negate()?, None)),
                    _ => Err(RuntimeError(format!("Unknown unary operation: {op}"))),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let (left_value, ret) = self.evaluate(scope, left)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }
                match op.as_str() {
                    "+" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.add(right_value)?, None))
                    }
                    "<" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.lt(right_value)?, None))
                    }
                    "==" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.eq(right_value)?, None))
                    }
                    _ => Err(RuntimeError(format!("Unknown binary operation: {op}"))),
                }
            }
            Expr::ListLiteral { elements } => {
                let mut ty = Type::Any;
                let mut element_values = vec![];
                for expr in elements {
                    let (expr_value, ret) = self.evaluate(scope, expr)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                    if let Some(narrowed) = ty.narrow(&expr_value.ty()) {
                        ty = narrowed;
                    } else {
                        return Err(RuntimeError("list contains distinct types".into()));
                    }
                    element_values.push(expr_value);
                }

                let list = Value::List(ty, element_values);
                Ok((list, None))
            }
            Expr::TupleLiteral { elements } => {
                let mut element_values = vec![];
                for expr in elements {
                    let (expr_value, ret) = self.evaluate(scope, expr)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                    element_values.push(expr_value);
                }

                let list = Value::Tuple(element_values);
                Ok((list, None))
            }
            Expr::Invocation { expr, args } => {
                let (expr_value, ret) = self.evaluate(scope, expr)?;
                if ret.is_some() {
                    return Ok((Value::Nil, ret));
                }
                let Value::FnDef(def) = expr_value else {
                    return Err(RuntimeError(format!("cannot call {}", expr_value.ty())));
                };

                let mut evaluated_args = vec![];
                for arg in args {
                    let (arg_value, ret) = self.evaluate(scope, &arg.expr)?;
                    if let Some(return_value) = ret {
                        return Ok((Value::Nil, Some(return_value)));
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
                    return Ok((Value::Nil, ret));
                }

                let mut result = Value::Nil;
                let mut ret = None;
                if cond_value.auto_coerce_bool()? {
                    (result, ret) = self.execute_block(scope, then)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                } else if let Some(els) = els {
                    (result, ret) = self.execute_block(scope, els)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                }

                Ok((result, None))
            }
            Expr::While { cond, body } => {
                let mut result = Value::Nil;
                let mut ret = None;
                loop {
                    let (cond_value, cond_ret) = self.evaluate(scope, cond)?;
                    if let Some(return_value) = cond_ret {
                        return Ok((Value::Nil, Some(return_value)));
                    }
                    if !cond_value.auto_coerce_bool()? {
                        return Ok((result, None));
                    }

                    (result, ret) = self.execute_block(scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                }
            }
            Expr::DoWhile { cond, body } => loop {
                loop {
                    let mut result = Value::Nil;
                    let mut ret = None;

                    (result, ret) = self.execute_block(scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }

                    if let Some(cond) = cond {
                        let (cond_value, ret) = self.evaluate(scope, cond)?;
                        if ret.is_some() {
                            return Ok((Value::Nil, ret));
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
                        return Ok((Value::Nil, ret));
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
        .insert(id("stdin"), Value::Str(Substr::from(stdin)));

    runtime.scopes[0].values.insert(
        id("print"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                println!("{}", text);
                Ok(Value::Nil)
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
                    Value::List(_, list) => {
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

                let Value::List(_, list) = items else {
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

                // TODO
                Ok(Value::List(Type::Any, result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("in"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("needle"), id("haystack")],
            body: FnBody::Builtin(|runtime, scope| {
                let needle = runtime.scopes[scope].values.get(&id("needle")).unwrap();

                let haystack = runtime.scopes[scope].values.get(&id("haystack")).unwrap();

                let Value::List(_, haystack) = haystack else {
                    return Err(RuntimeError(format!(
                        "cannot get max of: {}",
                        haystack.ty()
                    )));
                };

                Ok(Value::Bool(haystack.contains(needle)))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("filter"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items"), id("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(_, list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let list = list.clone();

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "cannot use filter w/ cb of type: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                let mut result = vec![];
                for item in list.iter() {
                    if runtime
                        .invoke(def.clone(), vec![(None, item.clone())])?
                        .auto_coerce_bool()?
                    {
                        result.push(item.clone());
                    }
                }

                // TODO
                Ok(Value::List(Type::Any, result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("filter_map"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items"), id("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(_, list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let list = list.clone();

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "cannot use filter w/ cb of type: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                let mut result = vec![];
                for item in list.iter() {
                    let item = runtime.invoke(def.clone(), vec![(None, item.clone())])?;

                    match item {
                        // TODO fix the "nil as well as unit" problem
                        Value::Nil => {}
                        _ => {
                            result.push(item);
                        }
                    }
                }

                // TODO
                Ok(Value::List(Type::Any, result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("find_map"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("items"), id("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(_, list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let list = list.clone();

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "cannot use filter w/ cb of type: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                for item in list.iter() {
                    let item = runtime.invoke(def.clone(), vec![(None, item.clone())])?;

                    match item {
                        // TODO fix the "nil as well as unit" problem
                        Value::Nil => {}
                        _ => {
                            return Ok(item);
                        }
                    }
                }

                Ok(Value::Nil)
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("range"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("start"), id("end")],
            body: FnBody::Builtin(|runtime, scope| {
                let start = runtime.scopes[scope].values.get(&id("start")).unwrap();

                let Value::Numeric(start) = start else {
                    return Err(RuntimeError(format!(
                        "range() start must be int, is: {}",
                        start.ty()
                    )));
                };

                let start = start.get_int()?;

                let end = runtime.scopes[scope].values.get(&id("end")).unwrap();

                let Value::Numeric(end) = end else {
                    return Err(RuntimeError(format!(
                        "range() end must be int, is: {}",
                        end.ty()
                    )));
                };

                let end = end.get_int()?;

                if end >= start {
                    Ok(Value::List(
                        Type::Numeric,
                        (start..end)
                            .map(|n| Value::Numeric(Numeric::Int(n)))
                            .collect(),
                    ))
                } else {
                    Ok(Value::List(
                        Type::Numeric,
                        (0..(start - end))
                            .map(|n| Value::Numeric(Numeric::Int(start - n)))
                            .collect(),
                    ))
                }
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

                let Value::List(_, list) = items else {
                    return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                };

                let mut result = Value::Numeric(Numeric::Int(0));
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
                    .split(sep.as_str())
                    .map(|piece| Value::Str(text.substr_from(piece)))
                    .collect::<Vec<_>>();

                Ok(Value::List(Type::Str, result))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("starts_with"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text"), id("substr")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                let Value::Str(text) = text else {
                    return Err(RuntimeError(format!(
                        "starts_with() text must be a string, is a: {}",
                        text.ty()
                    )));
                };

                let substr = runtime.scopes[scope].values.get(&id("substr")).unwrap();

                let Value::Str(substr) = substr else {
                    return Err(RuntimeError(format!(
                        "starts_with() substr must be a string, is a: {}",
                        substr.ty()
                    )));
                };

                Ok(Value::Bool(text.starts_with(substr.as_str())))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("slice"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("text"), id("i")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                let Value::Str(text) = text else {
                    return Err(RuntimeError(format!(
                        "starts_with() text must be a string, is a: {}",
                        text.ty()
                    )));
                };

                let i = runtime.scopes[scope].values.get(&id("i")).unwrap();

                let Value::Numeric(i) = i else {
                    return Err(RuntimeError(format!(
                        "starts_with() i must be an int, is a: {}",
                        i.ty()
                    )));
                };

                let i = i.get_int()?;

                if i < 0 {
                    return Err(RuntimeError(format!(
                        "starts_with() i must be a positive int"
                    )));
                }

                Ok(Value::Str(text.substr((i as usize)..)))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("index"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("list"), id("i")],
            body: FnBody::Builtin(|runtime, scope| {
                let list = runtime.scopes[scope].values.get(&id("list")).unwrap();

                let list = match list {
                    Value::List(_, list) => list,
                    Value::Tuple(list) => list,
                    _ => {
                        return Err(RuntimeError(format!(
                            "index() list must be a list or tuple, is a: {}",
                            list.ty()
                        )));
                    }
                };

                let i = runtime.scopes[scope].values.get(&id("i")).unwrap();

                let Value::Numeric(i) = i else {
                    return Err(RuntimeError(format!(
                        "split[#2] must be an int, is a: {}",
                        i.ty()
                    )));
                };

                let i = i.get_int()?;

                let el = match i {
                    i if i >= 0 => list.get(i as usize),
                    _ => list.get((list.len() as i64 + i) as usize),
                };

                Ok(el.cloned().unwrap_or(Value::Nil))
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

                // TODO // substr_from
                Ok(Value::Str(text.trim().to_string().into()))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("len"),
        Value::FnDef(FnDef {
            parent_scope: 0,
            params: vec![id("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.scopes[scope].values.get(&id("data")).unwrap();

                let len = match data {
                    Value::Str(text) => text.len(),
                    Value::List(_, list) => list.len(),
                    Value::Tuple(tuple) => tuple.len(),
                    _ => {
                        return Err(RuntimeError(format!("cannot get len of: {}", data.ty())));
                    }
                };

                Ok(Value::Numeric(Numeric::Int(len as i64)))
            }),
        }),
    );

    runtime.scopes[0].values.insert(
        id("chars"),
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

                Ok(Value::List(
                    Type::Str,
                    text.chars()
                        .map(|c| Value::Str(c.to_string().into()))
                        .collect(),
                ))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    fn list(t: Type, elements: Vec<Value>) -> Value {
        Value::List(t, elements)
    }

    fn tuple(elements: Vec<Value>) -> Value {
        Value::Tuple(elements)
    }

    fn int(n: i64) -> Value {
        Value::Numeric(Numeric::Int(n))
    }

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

    #[test]
    fn list_literals() {
        assert_eq!(
            execute(&parse_document(r#"[0]"#).unwrap(), "".into()),
            Ok(list(Type::Numeric, vec![int(0)]))
        );

        assert_eq!(
            execute(&parse_document(r#"[0, "hello"]"#).unwrap(), "".into()),
            Err(RuntimeError("list contains distinct types".into()))
        );
    }

    #[test]
    fn bla() {
        assert_eq!(
            execute(&parse_document("range(0, 10)").unwrap(), "".into()),
            Ok(list(Type::Numeric, (0..10).map(int).collect()))
        );

        assert_eq!(
            execute(&parse_document("range(6, 2)").unwrap(), "".into()),
            Ok(list(Type::Numeric, vec![int(6), int(5), int(4), int(3)]))
        );
    }

    #[test]
    fn aoc_day01() {
        let document = r#"

let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
}

fn lines(text) {
  text :split "\n"
}

let nums = [
  ("0", "0"),
  ("1", "1"),
  ("2", "2"),
  ("3", "3"),
  ("4", "4"),
  ("5", "5"),
  ("6", "6"),
  ("7", "7"),
  ("8", "8"),
  ("9", "9"),
  ("zero", "0"),
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
  ("ten", "0"),
  ("eleven", "1"),
  ("twelve", "2"),
  ("thirteen", "3"),
  ("fourteen", "4"),
  ("fifteen", "5"),
  ("sixteen", "6"),
  ("seventeen", "7"),
  ("eighteen", "8"),
  ("nineteen", "9"),
  ("twenty", "0"),
]

fn solve(input) {
  let values = input .lines :map |line| {
    let digits = line .chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values.sum
}

fn bonus(input) {
  let values = input .lines :map |line| {
    let digits = range(0, line.len)
      :filter_map |i| {
        nums :find_map |t| {
          if (line.slice(i).starts_with(t[0])) {
            t[1]
          }
        }
      }

    int(digits[0] + digits[-1])
  }

  values.sum
}

let solution = solve("1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet");

let bonus_solution = bonus("two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen");

(solution, bonus_solution)

"#;

        assert_eq!(
            execute(&parse_document(document).unwrap(), "".into()),
            Ok(tuple(vec![int(142), int(281)]))
        );
    }
}
