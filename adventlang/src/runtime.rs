use std::{cmp::Ordering, collections::HashMap, fmt::Display};

use arcstr::Substr;
use either::Either;
use regex::Regex;
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignPattern, Block, DeclarePattern, Document, Expr, Identifier, Item, Stmt,
        StrLiteralPiece, Type,
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

    pub fn div(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a / b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() / b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a / b),
        }
    }

    pub fn modulo(&self, other: Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a % b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() % b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a % b),
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
    pub params: Vec<DeclarePattern>,
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

    pub fn div(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.div(b))),
            (a, b) => Err(RuntimeError(format!(
                "can't perform {} + {}",
                a.ty(),
                b.ty()
            ))),
        }
    }

    pub fn modulo(&self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.modulo(b))),
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
pub enum Assignable {
    Loc {
        scope: usize,
        id: Identifier,
        indexes: Vec<Value>,
    },
    List {
        elements: Vec<Assignable>,
    },
    Tuple {
        elements: Vec<Assignable>,
    },
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
            match self.lookup(parent_scope_id, id) {
                Ok(res) => Ok(res),
                Err(_) => Err(RuntimeError(format!(
                    "variable `{id}` does not exist in scope tree: {}",
                    self.debug_scope(scope_id)
                ))),
            }
        } else {
            Err(RuntimeError(format!(
                "variable `{id}` does not exist in scope tree: {}",
                self.debug_scope(scope_id)
            )))
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
        assignable: Assignable,
        value: Value,
    ) -> Result<Option<Value>, RuntimeError> {
        match assignable {
            Assignable::Loc { scope, id, indexes } => {
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
            Assignable::List { elements } => {
                let Value::List(_, items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign into list pattern: {}",
                        value.ty()
                    )));
                };

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.assign(scope, pattern, value)?;
                }

                Ok(None)
            }
            Assignable::Tuple { elements } => {
                let Value::Tuple(items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign into tuple pattern: {}",
                        value.ty()
                    )));
                };

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.assign(scope, pattern, value)?;
                }

                Ok(None)
            }
        }
    }

    pub fn declare(
        &mut self,
        scope: usize,
        pattern: &DeclarePattern,
        value: Value,
    ) -> Result<(), RuntimeError> {
        match pattern {
            DeclarePattern::Id(id, _) => {
                self.scopes[scope].values.insert(id.clone(), value);
            }
            DeclarePattern::List { elements, rest } => {
                let Value::List(item_type, mut items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to list pattern: {}",
                        value.ty()
                    )));
                };

                let assign_rest_later = rest
                    .clone()
                    .try_map(|(id, t)| Ok((id, t, items.split_off(elements.len()))))?;

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    self.declare(
                        scope,
                        &DeclarePattern::Id(id, t),
                        Value::List(item_type, items),
                    )?;
                }
            }
            DeclarePattern::Tuple { elements, rest } => {
                let Value::Tuple(mut items) = value else {
                    return Err(RuntimeError(format!(
                        "cannot assign to tiple pattern: {}",
                        value.ty()
                    )));
                };

                let assign_rest_later = rest
                    .clone()
                    .try_map(|(id, t)| Ok((id, t, items.split_off(elements.len()))))?;

                for (pattern, value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    self.declare(scope, &DeclarePattern::Id(id, t), Value::Tuple(items))?;
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
            Stmt::Assign { pattern, expr } => {
                let (value, ret) = self.evaluate(scope, expr)?;
                if let Some(return_value) = ret {
                    return Ok((Value::Nil, Some(return_value)));
                }

                let assignable = match self.evaluate_assignable(scope, &pattern)? {
                    Either::Left(a) => a,
                    Either::Right(return_value) => {
                        return Ok((Value::Nil, Some(return_value)));
                    }
                };

                let ret = self.assign(scope, assignable, value)?;
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
                    DeclarePattern::List { .. } => {
                        if !(Type::List(Type::Any.into()) >= value.ty()) {
                            // println!("does not match sig: list");
                            return false;
                        }
                    }
                    DeclarePattern::Tuple { .. } => {
                        if !(Type::Tuple >= value.ty()) {
                            // println!("does not match sig: tuple");
                            return false;
                        }
                    }
                    DeclarePattern::Id(_, Some(t)) => {
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
                    DeclarePattern::Id(n, _) => n == &name,
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

    pub fn evaluate_assignable(
        &mut self,
        eval_scope: usize,
        pattern: &AssignPattern,
    ) -> Result<Either<Assignable, Value>, RuntimeError> {
        match pattern {
            AssignPattern::Id(id) => {
                let (def_scope, _) = self.lookup(eval_scope, id)?;
                Ok(Either::Left(Assignable::Loc {
                    scope: def_scope,
                    id: id.clone(),
                    indexes: vec![],
                }))
            }
            AssignPattern::Index(pattern, index_expr) => {
                match self.evaluate_assignable(eval_scope, &pattern)? {
                    Either::Right(return_value) => Ok(Either::Right(return_value)),
                    Either::Left(Assignable::Loc {
                        scope,
                        id,
                        mut indexes,
                    }) => {
                        let (result, ret) = self.evaluate(eval_scope, index_expr)?;
                        if let Some(return_value) = ret {
                            return Ok(Either::Right(return_value));
                        }

                        indexes.push(result);

                        Ok(Either::Left(Assignable::Loc { scope, id, indexes }))
                    }
                    _ => Err(RuntimeError(format!(
                        "cannot index into list or tuple assignable pattern"
                    ))),
                }
            }
            AssignPattern::List { elements } => {
                let mut assignable_elements = Vec::with_capacity(elements.len());
                for pattern in elements {
                    match self.evaluate_assignable(eval_scope, pattern)? {
                        Either::Right(return_value) => {
                            return Ok(Either::Right(return_value));
                        }
                        Either::Left(assignable) => {
                            assignable_elements.push(assignable);
                        }
                    }
                }
                Ok(Either::Left(Assignable::List {
                    elements: assignable_elements,
                }))
            }
            AssignPattern::Tuple { elements } => {
                let mut assignable_elements = Vec::with_capacity(elements.len());
                for pattern in elements {
                    match self.evaluate_assignable(eval_scope, pattern)? {
                        Either::Right(return_value) => {
                            return Ok(Either::Right(return_value));
                        }
                        Either::Left(assignable) => {
                            assignable_elements.push(assignable);
                        }
                    }
                }
                Ok(Either::Left(Assignable::Tuple {
                    elements: assignable_elements,
                }))
            }
        }
    }

    pub fn debug_scope(&self, scope: usize) -> String {
        if scope == 0 {
            "RootScope".into()
        } else {
            format!(
                "Scope({}){}",
                self.scopes[scope]
                    .values
                    .iter()
                    .map(|(key, value)| { format!("{key}: {}", value.ty()) })
                    .collect::<Vec<_>>()
                    .join(", "),
                match self.scopes[scope].parent_scope {
                    None => "".into(),
                    Some(parent_id) => format!(" <- {}", self.debug_scope(parent_id)),
                }
            )
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
                    "/" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.div(right_value)?, None))
                    }
                    "%" => {
                        let (right_value, ret) = self.evaluate(scope, right)?;
                        if let Some(return_value) = ret {
                            return Ok((Value::Nil, Some(return_value)));
                        }
                        Ok((left_value.modulo(right_value)?, None))
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
    use crate::parse::parse_document;

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

        assert_eq!(
            execute(
                &parse_document("let a = 4; let b = 5; [a, b] = [1, 2, 3, 4]; (a, b)").unwrap(),
                "".into()
            ),
            Ok(tuple(vec![int(1), int(2)]))
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
}
