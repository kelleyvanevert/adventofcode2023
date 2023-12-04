use std::{collections::HashMap, fmt::Display};

use arcstr::Substr;
use either::Either;
use regex::Regex;

use crate::ast::{
    AssignLocation, Block, Document, Expr, Identifier, Item, Pattern, Stmt, StrLiteralPiece, Type,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeError(pub String);

#[derive(Debug, Clone)]
pub struct AlRegex(pub Regex);

impl std::hash::Hash for AlRegex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}

impl Eq for AlRegex {}

impl PartialEq for AlRegex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl AsRef<Regex> for AlRegex {
    fn as_ref(&self) -> &Regex {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct Dict(HashMap<Value, Value>);

impl std::hash::Hash for Dict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo Dict::hash")
    }
}

impl PartialEq for Dict {
    fn eq(&self, other: &Self) -> bool {
        // TODO structural equality comparison ??
        false
    }
}

impl Eq for Dict {}

impl Dict {
    pub fn new() -> Dict {
        Self(HashMap::new())
    }
}

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

impl std::hash::Hash for Numeric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Numeric::Int(n) => n.hash(state),
            Numeric::Double(n) => format!("{}", n).hash(state),
        }
    }
}

impl Eq for Numeric {}

impl Numeric {
    fn negate(&self) -> Result<Numeric, RuntimeError> {
        match self {
            Numeric::Int(n) => Ok(Numeric::Int(-n)),
            Numeric::Double(n) => Ok(Numeric::Double(-n)),
        }
    }

    fn pow(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a.powf(b.get_double())),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double().powf(b)),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a.pow(b as u32)),
        }
    }

    fn add(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a + b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() + b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a + b),
        }
    }

    fn min(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a - b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() - b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a - b),
        }
    }

    fn mul(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a * b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() * b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a * b),
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
    name: Option<Identifier>,
    parent_scope: usize,
    signatures: Vec<FnSig>,
}

impl Eq for FnDef {}

impl std::hash::Hash for FnDef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.parent_scope.hash(state);
        self.signatures.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnSig {
    params: Vec<Pattern>,
    body: FnBody,
}

impl std::hash::Hash for FnSig {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.body.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Code(Block),
    Builtin(fn(&mut Runtime, usize) -> Result<Value, RuntimeError>),
}

impl std::hash::Hash for FnBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo FnBody::hash")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Nil,
    Bool(bool),
    Str(Substr),
    Numeric(Numeric),
    Regex(AlRegex),
    FnDef(FnDef),
    List(Type, Vec<Value>),
    Tuple(Vec<Value>),
    Dict(Dict),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(str) => write!(f, "{}", str),
            Value::Numeric(num) => write!(f, "{num}"),
            Value::Regex(r) => write!(f, "/{}/", r.0.as_str()),
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
            Value::Dict(dict) => {
                write!(f, "@{{")?;
                let pairs = dict.0.iter().collect::<Vec<_>>();
                let len = pairs.len();
                for (i, (key, value)) in pairs.iter().enumerate() {
                    write!(f, "{key} => {value}")?;
                    if i < len - 1 {
                        write!(f, ", ")?;
                    }
                }
                if len == 1 {
                    write!(f, ",")?;
                }
                write!(f, "}}")
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
            _ => Err(RuntimeError(format!("Can't negate {}", self.ty()))),
        }
    }

    fn pow(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.pow(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
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

    fn min(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.min(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    fn mul(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.mul(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    fn max(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Nil) => Ok(Value::Numeric(a.clone())),
            (Value::Nil, Value::Numeric(b)) => Ok(Value::Numeric(b.clone())),
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.max(b.clone()))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} max {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    fn lt(&self, other: &Value) -> Result<Value, RuntimeError> {
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

    fn gt(&self, other: &Value) -> Result<Value, RuntimeError> {
        other.lt(self)
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
            Value::Regex(r) => format!("/{}/", r.0.as_str()),
            Value::FnDef(_) => "<fn>".into(),
            Value::List(_, _) => "<list>".into(),
            Value::Tuple(_) => "<tuple>".into(),
            Value::Dict(_) => "<dict>".into(),
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
            Value::Regex(_) => Type::Regex,
            Value::FnDef(_) => Type::FnDef,
            Value::List(t, _) => Type::List(t.clone().into()),
            Value::Tuple(_) => Type::Tuple,
            Value::Dict(_) => Type::Dict,
        }
    }

    fn auto_coerce_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError(format!("cannot coerce {}", self.ty()))),
        }
    }

    fn truthy(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Nil => Ok(false),
            Value::Bool(b) => Ok(*b),
            Value::List(_, _) => Ok(true),
            Value::Tuple(_) => Ok(true),
            Value::Numeric(n) => Ok(n != &Numeric::Int(0) && n != &Numeric::Double(0.0)),
            Value::Str(str) => Ok(str.len() > 0),
            _ => Err(RuntimeError(format!(
                "cannot check truthiness of {}",
                self.ty()
            ))),
        }
    }
}

// Not the most beautiful situation, but .. oh well
// This type is to `AssignLocation` what `Value` is to `Expr` -- the runtime-internal evaluated version, say
struct Assignable {
    scope: usize,
    id: Identifier,
    indexes: Vec<Value>,
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

    fn lookup(&self, scope_id: usize, id: &Identifier) -> Result<(usize, &Value), RuntimeError> {
        let Some(scope) = self.scopes.get(scope_id) else {
            return Err(RuntimeError("weird: scope does not exist".into()));
        };

        if let Some(value) = scope.values.get(id) {
            return Ok((scope_id, value));
        }

        if let Some(parent_scope_id) = scope.parent_scope {
            self.lookup(parent_scope_id, id)
        } else {
            Err(RuntimeError(format!("variable does not exist: {id}")))
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
                        name: Some(name.clone()),
                        parent_scope: scope,
                        signatures: vec![FnSig {
                            params: params.clone(),
                            body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                        }],
                    }),
                );
            }
        }

        Ok(())
    }

    fn assign(
        &mut self,
        scope: usize,
        location: &AssignLocation,
        value: Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let mut assignable = match self.evaluate_assign_location(scope, &location)? {
            Either::Left(a) => a,
            Either::Right(return_value) => {
                return Ok(Some(return_value));
            }
        };

        let Assignable { scope, id, indexes } = assignable;

        if indexes.len() == 0 {
            self.scopes[scope].values.insert(id.clone(), value.clone());
            return Ok(None);
        }

        let mut location_value = self.scopes[scope]
            .values
            .get_mut(&id)
            .expect("assignable location id get failed");

        for index_value in indexes {
            match location_value {
                Value::List(t, list) => {
                    let Ok(i) = index_value.auto_coerce_int() else {
                        return Err(RuntimeError(format!(
                            "list index must be int, is: {}",
                            index_value.ty(),
                        )));
                    };

                    if i < 0 {
                        return Err(RuntimeError(format!(
                            "list index must be positive int, is: {}",
                            i,
                        )));
                    }

                    let i = i as usize;

                    if !(*t >= value.ty()) {
                        return Err(RuntimeError(format!(
                            "cannot insert value of type {} into list of type {}",
                            value.ty(),
                            Type::List(t.clone().into())
                        )));
                    }

                    if list.len() < i + 1 {
                        list.resize(i + 1, Value::Nil);
                    }

                    location_value = &mut list[i]
                }
                Value::Tuple(list) => {
                    let Ok(i) = index_value.auto_coerce_int() else {
                        return Err(RuntimeError(format!(
                            "tuple index must be int, is: {}",
                            index_value.ty(),
                        )));
                    };

                    if i < 0 {
                        return Err(RuntimeError(format!(
                            "tuple index must be positive int, is: {}",
                            i,
                        )));
                    }

                    let i = i as usize;

                    if list.len() < i + 1 {
                        list.resize(i + 1, Value::Nil);
                    }

                    location_value = &mut list[i]
                }
                Value::Dict(dict) => {
                    location_value = dict.0.entry(index_value).or_insert(Value::Nil);
                }
                _ => {
                    return Err(RuntimeError(format!(
                        "cannot assign into value of type: {}",
                        value.ty()
                    )))
                }
            }
        }

        // and then, finally:
        *location_value = value;

        Ok(None)
    }

    fn declare(
        &mut self,
        scope: usize,
        pattern: &Pattern,
        value: Value,
    ) -> Result<(), RuntimeError> {
        match pattern {
            Pattern::Id(id, _) => {
                self.scopes[scope].values.insert(id.clone(), value);
            }
            Pattern::List(ps) => {
                let Value::List(_, items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to list pattern: {}",
                        value.ty()
                    )));
                };

                for (pattern, value) in ps
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }
            }
            Pattern::Tuple(ps) => {
                let Value::Tuple(items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to tiple pattern: {}",
                        value.ty()
                    )));
                };

                for (pattern, value) in ps
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }
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
            Stmt::Declare { pattern, expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }

                self.declare(scope, pattern, value)?;

                Ok((Value::Nil, None))
            }
            Stmt::Assign { location, expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }

                let ret = self.assign(scope, location, value)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }

                Ok((Value::Nil, None))
            }
        }
    }

    // TODO
    fn matches_signature(
        name: &Option<Identifier>,
        sig: &FnSig,
        args: &Vec<(Option<Identifier>, Value)>,
    ) -> bool {
        if sig.params.len() != args.len() {
            false
        } else {
            // TODO: find arg that does not fit
            for (pat, (_, value)) in sig.params.iter().zip(args) {
                // if name == &Some(Identifier("slice".into())) {
                //     println!("fits? pattern {:?} value {:?}", pat, value);
                // }
                match pat {
                    Pattern::List(_) => {
                        if !(Type::List(Type::Any.into()) >= value.ty()) {
                            // println!("does not match sig: list");
                            return false;
                        }
                    }
                    Pattern::Tuple(_) => {
                        if !(Type::Tuple >= value.ty()) {
                            // println!("does not match sig: tuple");
                            return false;
                        }
                    }
                    Pattern::Id(_, Some(t)) => {
                        if !(*t >= value.ty()) {
                            // println!("does not match sig: type");
                            return false;
                        }
                    }
                    _ => {}
                }
            }

            true
        }
    }

    fn invoke(
        &mut self,
        def: FnDef,
        args: Vec<(Option<Identifier>, Value)>,
    ) -> Result<Value, RuntimeError> {
        let FnDef {
            name,
            parent_scope,
            signatures,
        } = def;

        let mut matching_signatures = signatures
            .into_iter()
            .filter(|sig| Runtime::matches_signature(&name, sig, &args))
            .collect::<Vec<_>>();

        let FnSig { params, body } = if matching_signatures.len() == 0 {
            return Err(RuntimeError(format!("could not find matching signature")));
        } else if matching_signatures.len() == 1 {
            matching_signatures.swap_remove(0)
        } else {
            unimplemented!(
                "todo select fn signature for: {}",
                name.map(|n| n.0).unwrap_or("<anonymous>".into())
            )
        };

        let execution_scope = self.scopes.len();
        self.scopes.push(Scope {
            parent_scope: Some(parent_scope),
            values: HashMap::new(),
        });

        let mut params_remaining = params.clone();
        for (arg_name, arg_value) in args {
            if let Some(name) = arg_name {
                // assign named param
                match params_remaining.iter().position(|p| match p {
                    Pattern::Id(n, _) => n == &name,
                    _ => false,
                }) {
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
            } else if let Some(pattern) = params_remaining.get(0).cloned() {
                // assign next available param
                params_remaining.remove(0);
                self.declare(execution_scope, &pattern, arg_value)?;
            } else {
                // no params left
                return Err(RuntimeError(format!("no param to pass arg to")));
            }
        }

        if params_remaining.len() > 0 {
            // unassigned params
            return Err(RuntimeError(format!("unassigned params left")));
        }

        Ok(match body {
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

    fn evaluate_assign_location(
        &mut self,
        scope: usize,
        location: &AssignLocation,
    ) -> Result<Either<Assignable, Value>, RuntimeError> {
        match location {
            AssignLocation::Id(id) => {
                let (def_scope, _) = self.lookup(scope, id)?;
                Ok(Either::Left(Assignable {
                    scope: def_scope,
                    id: id.clone(),
                    indexes: vec![],
                }))
            }
            AssignLocation::Index(location, index_expr) => {
                let mut assignable = match self.evaluate_assign_location(scope, &location)? {
                    Either::Left(a) => a,
                    Either::Right(return_value) => {
                        return Ok(Either::Right(return_value));
                    }
                };

                let (result, ret) = self.evaluate(scope, index_expr)?;
                if let Some(return_value) = ret {
                    return Ok(Either::Right(return_value));
                }

                assignable.indexes.push(result);

                Ok(Either::Left(assignable))
            }
        }
    }

    fn evaluate(
        &mut self,
        scope: usize,
        expr: &Expr,
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        match expr {
            Expr::Bool(b) => Ok((Value::Bool(*b), None)),
            Expr::NilLiteral => Ok((Value::Nil, None)),
            Expr::DictLiteral {} => Ok((Value::Dict(Dict::new()), None)),
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
                            build += &format!("{}", value);
                            // build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok((Value::Str(Substr::from(build)), None))
            }
            Expr::Numeric(num) => Ok((Value::Numeric(num.clone()), None)),
            Expr::RegexLiteral { regex } => Ok((Value::Regex(regex.clone()), None)),
            Expr::Variable(id) => {
                let (_, value) = self.lookup(scope, id)?;
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
                    "^" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.pow(right_value)?, None))
                    }
                    "+" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.add(right_value)?, None))
                    }
                    "-" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.min(right_value)?, None))
                    }
                    "*" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.mul(right_value)?, None))
                    }
                    "<" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.lt(&right_value)?, None))
                    }
                    ">" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.gt(&right_value)?, None))
                    }
                    "==" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.eq(right_value)?, None))
                    }
                    "&&" => {
                        if !left_value.truthy()? {
                            // short-curcuit
                            return Ok((Value::Bool(false), None));
                        }
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((right_value, None))
                    }
                    "||" => {
                        if left_value.truthy()? {
                            // short-curcuit
                            return Ok((left_value, None));
                        }
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((right_value, None))
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
                    name: None,
                    parent_scope: scope,
                    signatures: vec![FnSig {
                        params: params.clone(),
                        body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                    }],
                }),
                None,
            )),
            Expr::If {
                pattern,
                cond,
                then,
                els,
            } => {
                let (cond_value, ret) = self.evaluate(scope, cond)?;
                if ret.is_some() {
                    return Ok((Value::Nil, ret));
                }

                let mut result = Value::Nil;
                let mut ret = None;
                if cond_value.truthy()? {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    if let Some(pattern) = pattern {
                        self.declare(execution_scope, pattern, cond_value)?;
                    }

                    (result, ret) = self.execute_block(execution_scope, then)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                } else if let Some(els) = els {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    (result, ret) = self.execute_block(execution_scope, els)?;
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
            Expr::For {
                pattern,
                range,
                body,
            } => {
                let (range_value, range_ret) = self.evaluate(scope, range)?;
                if range_ret.is_some() {
                    return Ok((Value::Nil, range_ret));
                }

                let range = match range_value {
                    Value::List(_, values) => values,
                    Value::Tuple(values) => values,
                    _ => {
                        return Err(RuntimeError(format!(
                            "cannot for-loop over {}",
                            range_value.ty()
                        )));
                    }
                };

                for item in range {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    self.declare(execution_scope, pattern, item)?;

                    let (_, ret) = self.execute_block(execution_scope, body)?;
                    if ret.is_some() {
                        return Ok((Value::Nil, ret));
                    }
                }

                Ok((Value::Nil, None))
            }
        }
    }
}

pub fn execute(doc: &Document, stdin: String) -> Result<Value, RuntimeError> {
    fn idpat(id: &str) -> Pattern {
        Pattern::Id(Identifier(id.into()), None)
    }

    fn idpat_ty(id: &str, ty: Type) -> Pattern {
        Pattern::Id(Identifier(id.into()), Some(ty))
    }

    let mut runtime = Runtime::new();

    runtime.scopes[0]
        .values
        .insert(id("stdin"), Value::Str(Substr::from(stdin)));

    runtime.scopes[0].values.insert(
        id("print"),
        Value::FnDef(FnDef {
            name: Some(id("print")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    println!("{}", text);
                    Ok(Value::Nil)
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("run"),
        Value::FnDef(FnDef {
            name: Some(id("run")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("f")],
                body: FnBody::Builtin(|runtime, scope| {
                    let f = runtime.scopes[scope].values.get(&id("f")).unwrap();

                    match f {
                        Value::FnDef(def) => runtime.invoke(def.clone(), vec![]),
                        _ => Err(RuntimeError(format!("cannot run: {}", f.ty()))),
                    }
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("max"),
        Value::FnDef(FnDef {
            name: Some(id("max")),
            parent_scope: 0,
            signatures: vec![
                FnSig {
                    params: vec![idpat_ty("items", Type::List(Type::Any.into()))],
                    body: FnBody::Builtin(|runtime, scope| {
                        let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                        match items {
                            Value::List(_, list) => {
                                let mut result = Value::Nil;
                                for item in list.iter() {
                                    result = result.max(item)?;
                                }

                                Ok(result)
                            }
                            _ => Err(RuntimeError(format!("cannot get max of: {}", items.ty()))),
                        }
                    }),
                },
                FnSig {
                    params: vec![idpat_ty("a", Type::Numeric), idpat_ty("b", Type::Numeric)],
                    body: FnBody::Builtin(|runtime, scope| {
                        let a = runtime.scopes[scope].values.get(&id("a")).unwrap();
                        let b = runtime.scopes[scope].values.get(&id("b")).unwrap();

                        Ok(a.max(b)?)
                    }),
                },
            ],
        }),
    );

    runtime.scopes[0].values.insert(
        id("map"),
        Value::FnDef(FnDef {
            name: Some(id("map")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("flat_map"),
        Value::FnDef(FnDef {
            name: Some(id("flat_map")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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
                        let value = runtime.invoke(def.clone(), vec![(None, item.clone())])?;
                        let Value::List(_, items) = value else {
                            return Err(RuntimeError(format!(
                                "flat_map cb should return lists, returned: {}",
                                value.ty()
                            )));
                        };

                        // TODO type-check

                        result.extend(items);
                    }

                    // TODO type
                    Ok(Value::List(Type::Any, result))
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("in"),
        Value::FnDef(FnDef {
            name: Some(id("in")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("needle"), idpat("haystack")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("filter"),
        Value::FnDef(FnDef {
            name: Some(id("filter")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("filter_map"),
        Value::FnDef(FnDef {
            name: Some(id("filter_map")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("find_map"),
        Value::FnDef(FnDef {
            name: Some(id("find_map")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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

                    for item in list {
                        let item = runtime.invoke(def.clone(), vec![(None, item)])?;

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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("find"),
        Value::FnDef(FnDef {
            name: Some(id("find")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items"), idpat("cb")],
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

                    for item in list {
                        let check = runtime.invoke(def.clone(), vec![(None, item.clone())])?;
                        if check.truthy()? {
                            return Ok(item);
                        }
                    }

                    Ok(Value::Nil)
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("range"),
        Value::FnDef(FnDef {
            name: Some(id("range")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("start"), idpat("end")],
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
                        // TODO allow this conditionally or something?
                        // Ok(Value::List(
                        //     Type::Numeric,
                        //     (0..(start - end))
                        //         .map(|n| Value::Numeric(Numeric::Int(start - n)))
                        //         .collect(),
                        // ))

                        Ok(Value::List(Type::Numeric, vec![]))
                    }
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("enumerate"),
        Value::FnDef(FnDef {
            name: Some(id("enumerate")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                    Ok(match items {
                        Value::List(_, list) => Value::List(
                            Type::Tuple, // TODO generic tuple types
                            list.iter()
                                .cloned()
                                .enumerate()
                                .map(|(i, item)| {
                                    Value::Tuple(vec![Value::Numeric(Numeric::Int(i as i64)), item])
                                })
                                .collect::<Vec<_>>(),
                        ),
                        Value::Tuple(list) => Value::Tuple(
                            list.iter()
                                .cloned()
                                .enumerate()
                                .map(|(i, item)| {
                                    Value::Tuple(vec![Value::Numeric(Numeric::Int(i as i64)), item])
                                })
                                .collect::<Vec<_>>(),
                        ),
                        _ => {
                            return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                        }
                    })
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("sum"),
        Value::FnDef(FnDef {
            name: Some(id("sum")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("items")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                    let list = match items {
                        Value::List(_, list) => list,
                        Value::Tuple(list) => list,
                        _ => {
                            return Err(RuntimeError(format!("cannot get max of: {}", items.ty())));
                        }
                    };

                    let mut result = Value::Numeric(Numeric::Int(0));
                    for item in list.iter() {
                        result = result.add(item.clone())?;
                    }

                    Ok(result)
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("split"),
        Value::FnDef(FnDef {
            name: Some(id("split")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text"), idpat("sep")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("lines"),
        Value::FnDef(FnDef {
            name: Some(id("lines")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "lines() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let result = text
                        .lines()
                        .map(|line| Value::Str(text.substr_from(line)))
                        .collect::<Vec<_>>();

                    Ok(Value::List(Type::Str, result))
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("match"),
        Value::FnDef(FnDef {
            name: Some(id("match")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text"), idpat("regex")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "match() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let regex = runtime.scopes[scope].values.get(&id("regex")).unwrap();

                    let Value::Regex(regex) = regex else {
                        return Err(RuntimeError(format!(
                            "match() regex must be a regex, is a: {}",
                            regex.ty()
                        )));
                    };

                    match regex.0.captures(text) {
                        Some(cap) => {
                            let m = cap.get(0).unwrap();
                            Ok(Value::Tuple(vec![
                                Value::Str(m.as_str().into()),
                                Value::Numeric(Numeric::Int(m.start() as i64)),
                            ]))
                        }
                        None => Ok(Value::Nil),
                    }
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("match_all"),
        Value::FnDef(FnDef {
            name: Some(id("match_all")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text"), idpat("regex")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "match() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let regex = runtime.scopes[scope].values.get(&id("regex")).unwrap();

                    let Value::Regex(regex) = regex else {
                        return Err(RuntimeError(format!(
                            "match() regex must be a regex, is a: {}",
                            regex.ty()
                        )));
                    };

                    Ok(Value::List(
                        Type::Tuple,
                        regex
                            .0
                            .captures_iter(text)
                            .map(|cap| {
                                let m = cap.get(0).unwrap();
                                Value::Tuple(vec![
                                    Value::Str(m.as_str().into()),
                                    Value::Numeric(Numeric::Int(m.start() as i64)),
                                ])
                            })
                            .collect(),
                    ))
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("starts_with"),
        Value::FnDef(FnDef {
            name: Some(id("starts_with")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text"), idpat("substr")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("replace"),
        Value::FnDef(FnDef {
            name: Some(id("replace")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text"), idpat("def")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "replace() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let def = runtime.scopes[scope].values.get(&id("def")).unwrap();

                    let Value::Tuple(def) = def else {
                        return Err(RuntimeError(format!(
                            "replace() def must be a string, is a: {}",
                            def.ty()
                        )));
                    };

                    let Some(Value::Str(find)) = def.get(0) else {
                        return Err(RuntimeError(format!("replace() def[0] must be a string")));
                    };

                    let Some(Value::Str(replace)) = def.get(1) else {
                        return Err(RuntimeError(format!("replace() def[1] must be a string")));
                    };

                    let result = text.replace(find.as_str(), replace).into();

                    Ok(Value::Str(result))
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("slice"),
        Value::FnDef(FnDef {
            name: Some(id("slice")),
            parent_scope: 0,
            signatures: vec![
                FnSig {
                    params: vec![idpat("text"), Pattern::Id(id("i"), Some(Type::Numeric))],
                    body: FnBody::Builtin(|runtime, scope| {
                        let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                        let Value::Str(text) = text else {
                            return Err(RuntimeError(format!(
                                "slice() text must be a string, is a: {}",
                                text.ty()
                            )));
                        };

                        let i = runtime.scopes[scope].values.get(&id("i")).unwrap();

                        let Value::Numeric(i) = i else {
                            return Err(RuntimeError(format!(
                                "slice() i must be an int, is a: {}",
                                i.ty()
                            )));
                        };

                        let i = i.get_int()?;

                        if i < 0 {
                            return Err(RuntimeError(format!("slice() i must be a positive int")));
                        }

                        Ok(Value::Str(text.substr((i as usize)..)))
                    }),
                },
                FnSig {
                    params: vec![idpat("text"), Pattern::Id(id("range"), Some(Type::Tuple))],
                    body: FnBody::Builtin(|runtime, scope| {
                        let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                        let Value::Str(text) = text else {
                            return Err(RuntimeError(format!(
                                "slice() text must be a string, is a: {}",
                                text.ty()
                            )));
                        };

                        let range = runtime.scopes[scope].values.get(&id("range")).unwrap();

                        let Value::Tuple(range) = range else {
                            return Err(RuntimeError(format!(
                                "slice() range must be an (int, int) range, is a: {}",
                                range.ty()
                            )));
                        };

                        let Some(Value::Numeric(start)) = range.get(0) else {
                            return Err(RuntimeError(format!(
                                "slice() range must be an (int, int) range"
                            )));
                        };

                        let start = start.get_int()?;

                        if start < 0 {
                            return Err(RuntimeError(format!(
                                "slice() range must be an (int, int) range"
                            )));
                        }

                        let Some(Value::Numeric(end)) = range.get(1) else {
                            return Err(RuntimeError(format!(
                                "slice() range must be an (int, int) range"
                            )));
                        };

                        let end = end.get_int()?;

                        if end < 0 {
                            return Err(RuntimeError(format!(
                                "slice() range must be an (int, int) range"
                            )));
                        }

                        Ok(Value::Str(
                            text.substr((start as usize)..(end as usize).min(text.len())),
                        ))
                    }),
                },
            ],
        }),
    );

    runtime.scopes[0].values.insert(
        id("index"),
        Value::FnDef(FnDef {
            name: Some(id("index")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("list"), idpat("i")],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.scopes[scope].values.get(&id("list")).unwrap();

                    let list = match list {
                        Value::Dict(dict) => {
                            let key = runtime.scopes[scope].values.get(&id("i")).unwrap();
                            return match dict.0.get(key) {
                                Some(value) => Ok(value.clone()),
                                None => Ok(Value::Nil),
                            };
                        }
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("insert"),
        Value::FnDef(FnDef {
            name: Some(id("insert")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("dict"), idpat("key"), idpat("value")],
                body: FnBody::Builtin(|runtime, scope| {
                    let key = runtime.scopes[scope]
                        .values
                        .get(&id("key"))
                        .unwrap()
                        .clone();

                    let value = runtime.scopes[scope]
                        .values
                        .get(&id("value"))
                        .unwrap()
                        .clone();

                    let dict = runtime.scopes[scope].values.get_mut(&id("dict")).unwrap();

                    let Value::Dict(dict) = dict else {
                        return Err(RuntimeError(format!(
                            "assign() dict must be a dict, is a: {}",
                            dict.ty()
                        )));
                    };

                    println!("found dict, inserting now");

                    dict.0.insert(key, value);

                    Ok(Value::Nil)
                }),
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("trim"),
        Value::FnDef(FnDef {
            name: Some(id("trim")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("len"),
        Value::FnDef(FnDef {
            name: Some(id("len")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("data")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("chars"),
        Value::FnDef(FnDef {
            name: Some(id("chars")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("text")],
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
            }],
        }),
    );

    runtime.scopes[0].values.insert(
        id("int"),
        Value::FnDef(FnDef {
            name: Some(id("int")),
            parent_scope: 0,
            signatures: vec![FnSig {
                params: vec![idpat("data")],
                body: FnBody::Builtin(|runtime, scope| {
                    let data = runtime.scopes[scope].values.get(&id("data")).unwrap();

                    let result = data.auto_coerce_int()?;

                    Ok(Value::Numeric(Numeric::Int(result)))
                }),
            }],
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
            Ok(list(Type::Numeric, vec![]))
        );
    }

    #[test]
    fn patterns() {
        assert_eq!(
            execute(
                &parse_document("let [a, b] = [2, 3]; (a, b)").unwrap(),
                "".into()
            ),
            Ok(tuple(vec![int(2), int(3)]))
        );

        assert_eq!(
            execute(
                &parse_document("let [a, b] = [2]; (a, b)").unwrap(),
                "".into()
            ),
            Ok(tuple(vec![int(2), Value::Nil]))
        );

        assert_eq!(
            execute(
                &parse_document("let [a, b] = [2, 3, 4]; (a, b)").unwrap(),
                "".into()
            ),
            Ok(tuple(vec![int(2), int(3)]))
        );

        assert_eq!(
            execute(&parse_document("let [] = [2, 3, 4]").unwrap(), "".into()),
            Ok(Value::Nil)
        );
    }

    #[test]
    fn if_declare() {
        assert_eq!(
            execute(
                &parse_document("if (let answer = 42) { answer }").unwrap(),
                "".into()
            ),
            Ok(int(42))
        );
    }

    #[test]
    fn aoc_day01() {
        let document = r#"

let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
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
          if line.slice(i).starts_with(t[0]) {
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

    #[test]
    fn aoc_day02() {
        let document = r#"

let example_input = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

fn solve(input, red, green, blue) {
  input
    .trim
    .lines
    :map |game| {
      let [id, sets] = game :split ": "
      let id = id :replace ("Game ", "") .int
      let invalid = sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw.split(" ")
          (num.int, color)
        }
        :find |(num, color)| {
          color == "red" && num > red
          || color == "green" && num > green
          || color == "blue" && num > blue
        }

      if (invalid) {
        0
      } else {
        id
      }
    }
    .sum
}

fn bonus(input) {
  input
    .trim
    .lines
    :map |game| {
      let red = 0;
      let green = 0;
      let blue = 0;

      let sets = (game :split ": ")[1]
      sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw.split(" ")
          (num.int, color)
        }
        :map |(num, color)| {
          if (color == "red") {
            red = red :max num
          }
          if (color == "green") {
            green = green :max num
          }
          if (color == "blue") {
            blue = blue :max num
          }
        }

      red * green * blue
    }
    .sum
}

let solution = solve(example_input, 12, 13, 14)

let bonus_solution = bonus(example_input)

(solution, bonus_solution)

"#;

        assert_eq!(
            execute(&parse_document(document).unwrap(), "".into()),
            Ok(tuple(vec![int(8), int(2286)]))
        );
    }

    #[test]
    fn aoc_day03() {
        let document = r#"

let example_input = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

fn solve(input) {
  let schematic = input.trim.lines
  let total = 0

  fn should_include(y, x, l) {
    // check previous row
    if y > 0 && schematic[y - 1] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check current row
    if schematic[y] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check next row
    if y < schematic.len - 1 && schematic[y + 1] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    false
  }

  for let (y, line) in schematic.enumerate {
    let x = 0
    while x < line.len {
      if let m = line :slice x :match /^[0-9]+/ {
        if should_include(y, x, m[0].len) {
          total = total + (m[0].int)
        }
        x = x + m[0].len
      } else {
        x = x + 1
      }
    }
  }

  total
}

fn bonus(input) {
  let schematic = input.trim.lines
  let total = 0
  let possible_gears = @{}

  fn found_adj(pos, s) {
    if let other = possible_gears[pos] {
      total = total + (s * other)
    } else {
      possible_gears[pos] = s
    }
  }

  fn possible_gear_part(y, x, s) {
    // check previous row
    if y > 0 {
      let start = (x-1) :max 0
      if let m = schematic[y - 1] :slice (start, x + s.len + 1) :match /[*]/ {
        let pos = (y-1, start+m[1])
        found_adj(pos, s.int)
      }
    }

    // check current row
    let start = (x-1) :max 0
    if let m = schematic[y] :slice (start, x + s.len + 1) :match /[*]/ {
      let pos = (y, start+m[1])
      found_adj(pos, s.int)
    }

    // check next row
    if y < schematic.len - 1 {
      let start = (x-1) :max 0
      if let m = schematic[y + 1] :slice (start, x + s.len + 1) :match /[*]/ {
        let pos = (y+1, start+m[1])
        found_adj(pos, s.int)
      }
    }
  }

  for let (y, line) in schematic.enumerate {
    let x = 0
    while x < line.len {
      if let m = line :slice x :match /^[0-9]+/ {
        possible_gear_part(y, x, m[0])
        x = x + m[0].len
      } else {
        x = x + 1
      }
    }
  }

  total
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

        assert_eq!(
            execute(&parse_document(document).unwrap(), "".into()),
            Ok(tuple(vec![int(4361), int(467835)]))
        );
    }

    #[test]
    fn aoc_day04() {
        let document = r#"

let example_input = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"

fn solve(input: str) {
  input
    .trim
    .lines
    :map |line: str| {
      let [_, rest] = line :split ":"
      let [winning, yours] = rest :split "|" :map |seg| {
        seg :match_all /[0-9]+/ :map |t| { t[0].int }
      }
      let wins = yours :filter |n| { n :in winning } .len

      if (wins > 0) {
        2 ^ (wins - 1)
      } else {
        0
      }
    }
    .sum
}

fn bonus(input: str) {
  let card_wins = input.trim.lines :map |line: str| {
    let [_, rest] = line :split ":"
    let [winning, yours] = rest :split "|" :map |seg| {
      seg :match_all /[0-9]+/ :map |t| { t[0].int }
    }
    yours :filter |n| { n :in winning } .len
  }

  let copies = card_wins :map |_| { 1 }

  for let i in range(0, card_wins.len) {
    for let w in range(1, card_wins[i] + 1) {
      copies[i + w] = copies[i + w] + copies[i]
    }
  }

  copies.sum
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

        assert_eq!(
            execute(&parse_document(document).unwrap(), "".into()),
            Ok(tuple(vec![int(13), int(30)]))
        );
    }
}
