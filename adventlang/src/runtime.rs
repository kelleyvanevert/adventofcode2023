use std::{cmp::Ordering, collections::HashMap, fmt::Display};

use arcstr::Substr;
use either::Either;
use regex::Regex;
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignLocation, Block, Document, Expr, Identifier, Item, Pattern, Stmt, StrLiteralPiece,
        Type,
    },
    stdlib::implement_stdlib,
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

impl PartialOrd for AlRegex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for AlRegex {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct Dict(pub HashMap<Value, Value>);

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

#[derive(Debug, Clone, PartialEq)]
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

impl Ord for Numeric {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Numeric::Double(a), b) => a.total_cmp(&b.get_double()),
            (a, Numeric::Double(b)) => a.get_double().total_cmp(b),

            (Numeric::Int(a), Numeric::Int(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Numeric {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Numeric {
    pub fn negate(&self) -> Result<Numeric, RuntimeError> {
        match self {
            Numeric::Int(n) => Ok(Numeric::Int(-n)),
            Numeric::Double(n) => Ok(Numeric::Double(-n)),
        }
    }

    pub fn pow(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a.powf(b.get_double())),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double().powf(b)),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a.pow(b as u32)),
        }
    }

    pub fn add(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a + b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() + b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a + b),
        }
    }

    pub fn sub(&self, other: &Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a - b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() - b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a - b),
        }
    }

    pub fn mul(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a * b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() * b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a * b),
        }
    }

    pub fn get_double(&self) -> f64 {
        match self {
            Numeric::Int(a) => *a as f64,
            Numeric::Double(a) => *a as f64,
        }
    }

    pub fn get_int(&self) -> Result<i64, RuntimeError> {
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
    pub params: Vec<Pattern>,
    pub body: FnBody,
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

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(ord) => ord,
            None => panic!("cannot compare {} with {}", self.ty(), other.ty()),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            (Value::Bool(a), Value::Bool(b)) => Some(a.cmp(b)),
            (Value::Str(a), Value::Str(b)) => Some(a.cmp(b)),
            (Value::Numeric(a), Value::Numeric(b)) => Some(a.cmp(b)),
            (Value::Regex(a), Value::Regex(b)) => Some(a.cmp(b)),

            _ => None,
        }
    }
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
    pub fn negate(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Nil => Ok(Value::Nil),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => Err(RuntimeError(format!("Can't negate str"))),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            _ => Err(RuntimeError(format!("Can't negate {}", self.ty()))),
        }
    }

    pub fn pow(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.pow(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    pub fn add(&self, other: Value) -> Result<Value, RuntimeError> {
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

    pub fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.sub(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    pub fn mul(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.mul(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    // TODO
    pub fn auto_coerce_str(&self) -> String {
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
    pub fn auto_coerce_int(&self) -> Result<i64, RuntimeError> {
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

    pub fn ty(&self) -> Type {
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

    pub fn auto_coerce_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError(format!("cannot coerce {}", self.ty()))),
        }
    }

    pub fn truthy(&self) -> Result<bool, RuntimeError> {
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
pub struct Assignable {
    pub scope: usize,
    pub id: Identifier,
    pub indexes: Vec<Value>,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub parent_scope: Option<usize>,
    pub values: HashMap<Identifier, Value>,
}

pub struct Runtime {
    pub scopes: Vec<Scope>,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime {
            scopes: vec![Scope {
                parent_scope: None,
                values: HashMap::new(),
            }],
        }
    }

    pub fn builtin(&mut self, name: &str, signatures: impl IntoIterator<Item = FnSig>) {
        self.scopes[0].values.insert(
            name.into(),
            Value::FnDef(FnDef {
                name: Some(name.into()),
                parent_scope: 0,
                signatures: signatures.into_iter().collect(),
            }),
        );
    }

    pub fn lookup(
        &self,
        scope_id: usize,
        id: &Identifier,
    ) -> Result<(usize, &Value), RuntimeError> {
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

    pub fn execute_block(
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

    pub fn define(&mut self, scope: usize, item: &Item) -> Result<(), RuntimeError> {
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

    pub fn assign(
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

    pub fn declare(
        &mut self,
        scope: usize,
        pattern: &Pattern,
        value: Value,
    ) -> Result<(), RuntimeError> {
        match pattern {
            Pattern::Id(id, _) => {
                self.scopes[scope].values.insert(id.clone(), value);
            }
            Pattern::List { elements, rest } => {
                let Value::List(t, mut items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to list pattern: {}",
                        value.ty()
                    )));
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    self.declare(scope, &Pattern::Id(id.clone(), t), Value::Nil)?;
                    Ok((id.clone(), items.split_off(elements.len())))
                })?;

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, items)) = assign_rest_later {
                    self.assign(scope, &AssignLocation::Id(id), Value::List(t, items))?;
                }
            }
            Pattern::Tuple { elements, rest } => {
                let Value::Tuple(mut items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to tiple pattern: {}",
                        value.ty()
                    )));
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    self.declare(scope, &Pattern::Id(id.clone(), t), Value::Nil)?;
                    Ok((id.clone(), items.split_off(elements.len())))
                })?;

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, items)) = assign_rest_later {
                    self.assign(scope, &AssignLocation::Id(id), Value::Tuple(items))?;
                }
            }
        }

        Ok(())
    }

    pub fn execute(
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
    pub fn matches_signature(
        _name: &Option<Identifier>,
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
                    Pattern::List { .. } => {
                        if !(Type::List(Type::Any.into()) >= value.ty()) {
                            // println!("does not match sig: list");
                            return false;
                        }
                    }
                    Pattern::Tuple { .. } => {
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

    pub fn invoke(
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
            return Err(RuntimeError(format!(
                "could not find matching signature for {}({:?})",
                name.map(|n| n.0).unwrap_or("<anonymous>".into()),
                args.iter()
                    .map(|arg| format!("{}", arg.1.ty()))
                    .collect::<Vec<_>>()
            )));
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

    pub fn evaluate_assign_location(
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

    pub fn evaluate(
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
                        Ok((left_value.sub(&right_value)?, None))
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
                        match left_value.partial_cmp(&right_value) {
                            None => Err(RuntimeError(format!("cannot compare"))),
                            Some(ord) => Ok((Value::Bool(ord == Ordering::Less), None)),
                        }
                    }
                    ">" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        match left_value.partial_cmp(&right_value) {
                            None => Err(RuntimeError(format!("cannot compare"))),
                            Some(ord) => Ok((Value::Bool(ord == Ordering::Greater), None)),
                        }
                    }
                    "==" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        match left_value.partial_cmp(&right_value) {
                            None => Err(RuntimeError(format!("cannot compare"))),
                            Some(ord) => Ok((Value::Bool(ord == Ordering::Equal), None)),
                        }
                    }
                    ">=" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        match left_value.partial_cmp(&right_value) {
                            None => Err(RuntimeError(format!("cannot compare"))),
                            Some(ord) => Ok((
                                Value::Bool(ord == Ordering::Equal || ord == Ordering::Greater),
                                None,
                            )),
                        }
                    }
                    "<=" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        match left_value.partial_cmp(&right_value) {
                            None => Err(RuntimeError(format!("cannot compare"))),
                            Some(ord) => Ok((
                                Value::Bool(ord == Ordering::Equal || ord == Ordering::Less),
                                None,
                            )),
                        }
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
    let mut runtime = Runtime::new();

    implement_stdlib(&mut runtime);

    runtime.scopes[0]
        .values
        .insert(id("stdin"), Value::Str(Substr::from(stdin)));

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
    fn test_sorting() {
        assert_eq!(
            execute(
                &parse_document("[1, 9, 3, 2, 7] :sort_by_key |n| { n }").unwrap(),
                "".into()
            ),
            Ok(list(
                Type::Numeric,
                vec![int(1), int(2), int(3), int(7), int(9)]
            ))
        );

        assert_eq!(
            execute(
                &parse_document(
                    "[(nil, 1), (nil, 9), (nil, 3), (nil, 2), (nil, 7)] :sort_by_key |n| { n[1] }"
                )
                .unwrap(),
                "".into()
            ),
            Ok(list(
                Type::Tuple,
                vec![
                    tuple(vec![Value::Nil, int(1)]),
                    tuple(vec![Value::Nil, int(2)]),
                    tuple(vec![Value::Nil, int(3)]),
                    tuple(vec![Value::Nil, int(7)]),
                    tuple(vec![Value::Nil, int(9)])
                ]
            ))
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
        x += m[0].len
      } else {
        x += 1
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
      total += s * other
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
        x += m[0].len
      } else {
        x += 1
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
      copies[i + w] += copies[i]
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
