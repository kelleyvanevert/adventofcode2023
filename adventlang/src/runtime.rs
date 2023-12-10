use std::{cmp::Ordering, collections::HashMap, fmt::Display};

use arcstr::Substr;
use either::Either;
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignPattern, Block, DeclarePattern, Document, Expr, Identifier, Item, Stmt,
        StrLiteralPiece, Type,
    },
    stdlib::implement_stdlib,
    value::{AlRegex, EvalOther, EvaluationResult, Numeric, RuntimeError},
};

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
    Builtin(fn(&mut Runtime, usize) -> EvaluationResult<Value>),
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
                    write!(f, ".{key} {value}")?;
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
    pub fn negate(&self) -> EvaluationResult<Value> {
        match self {
            Value::Nil => Ok(Value::Nil),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => RuntimeError(format!("Can't negate str")).into(),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            _ => RuntimeError(format!("Can't negate {}", self.ty())).into(),
        }
    }

    pub fn left_shift(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(Numeric::Int(a)), Value::Numeric(Numeric::Int(b))) if b >= 0 => {
                Ok(Value::Numeric(Numeric::Int(a << b)))
            }
            (a, b) => RuntimeError(format!("can't perform {} << {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn pow(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.pow(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn add(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => {
                let new = Substr::from(a.to_string() + &b);
                Ok(Value::Str(new))
            }
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.add(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn sub(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.sub(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn mul(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.mul(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn div(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.div(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn modulo(&self, other: Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.modulo(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
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
    pub fn auto_coerce_int(&self) -> EvaluationResult<i64> {
        match self {
            Value::Nil => RuntimeError(format!("cannot coerce {} to int", self.ty())).into(),
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
                    .map_err(|_| RuntimeError(format!("cannot coerce '{}' to int", str)).into());
            }
            Value::Numeric(n) => n.get_int(),
            _ => RuntimeError(format!("cannot coerce {} to int", self.ty())).into(),
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

    pub fn truthy(&self) -> EvaluationResult<bool> {
        match self {
            Value::Nil => Ok(false),
            Value::Bool(b) => Ok(*b),
            Value::List(_, _) => Ok(true),
            Value::Tuple(_) => Ok(true),
            Value::Dict(_) => Ok(true),
            Value::Numeric(n) => Ok(n != &Numeric::Int(0) && n != &Numeric::Double(0.0)),
            Value::Str(str) => Ok(str.len() > 0),
            _ => RuntimeError(format!("cannot check truthiness of {}", self.ty())).into(),
        }
    }
}

// Not the most beautiful situation, but .. oh well
// This type is to `AssignLocation` what `Value` is to `Expr` -- the runtime-internal evaluated version, say
pub enum Assignable {
    Loc {
        scope: usize,
        id: Identifier,
        indexes: Vec<Either<Value, ()>>, // () = "push in array"
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

    pub fn lookup(&self, scope_id: usize, id: &Identifier) -> EvaluationResult<(usize, &Value)> {
        let Some(scope) = self.scopes.get(scope_id) else {
            return RuntimeError("weird: scope does not exist".into()).into();
        };

        if let Some(value) = scope.values.get(id) {
            return Ok((scope_id, value));
        }

        if let Some(parent_scope_id) = scope.parent_scope {
            match self.lookup(parent_scope_id, id) {
                Ok(res) => Ok(res),
                Err(_) => RuntimeError(format!(
                    "variable `{id}` does not exist in scope tree: {}",
                    self.debug_scope(scope_id)
                ))
                .into(),
            }
        } else {
            RuntimeError(format!(
                "variable `{id}` does not exist in scope tree: {}",
                self.debug_scope(scope_id)
            ))
            .into()
        }
    }

    pub fn execute_block(&mut self, scope: usize, block: &Block) -> EvaluationResult<Value> {
        let mut result = Value::Nil;

        for item in &block.items {
            self.define(scope, &item)?;
        }

        for stmt in &block.stmts {
            result = self.execute(scope, &stmt)?;
        }

        Ok(result)
    }

    pub fn define(&mut self, scope: usize, item: &Item) -> EvaluationResult<()> {
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
    ) -> EvaluationResult<Option<Value>> {
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
                    match (index_value, location_value) {
                        (Either::Left(index_value), Value::List(t, list)) => {
                            let Ok(i) = index_value.auto_coerce_int() else {
                                return RuntimeError(format!(
                                    "list index must be int, is: {}",
                                    index_value.ty(),
                                ))
                                .into();
                            };

                            if i < 0 {
                                return RuntimeError(format!(
                                    "list index must be positive int, is: {}",
                                    i,
                                ))
                                .into();
                            }

                            let i = i as usize;

                            // if !(*t >= value.ty()) {
                            //     return RuntimeError(format!(
                            //         "cannot insert value of type {} into list of type {}",
                            //         value.ty(),
                            //         Type::List(t.clone().into())
                            //     ))
                            //     .into();
                            // }

                            if list.len() < i + 1 {
                                list.resize(i + 1, Value::Nil);
                            }

                            location_value = &mut list[i];
                        }
                        (Either::Right(()), Value::List(_, list)) => {
                            list.push(Value::Nil);
                            let i = list.len() - 1;
                            location_value = &mut list[i];
                        }
                        (Either::Left(index_value), Value::Tuple(list)) => {
                            let Ok(i) = index_value.auto_coerce_int() else {
                                return RuntimeError(format!(
                                    "tuple index must be int, is: {}",
                                    index_value.ty(),
                                ))
                                .into();
                            };

                            if i < 0 {
                                return RuntimeError(format!(
                                    "tuple index must be positive int, is: {}",
                                    i,
                                ))
                                .into();
                            }

                            let i = i as usize;

                            if list.len() < i + 1 {
                                list.resize(i + 1, Value::Nil);
                            }

                            location_value = &mut list[i]
                        }
                        (Either::Right(()), Value::Tuple(list)) => {
                            list.push(Value::Nil);
                            let i = list.len() - 1;
                            location_value = &mut list[i];
                        }
                        (Either::Left(index_value), Value::Dict(dict)) => {
                            location_value = dict.0.entry(index_value).or_insert(Value::Nil);
                        }
                        (Either::Right(()), Value::Dict(_)) => {
                            return RuntimeError(format!(
                                "cannot push value into dict with []= syntax"
                            ))
                            .into()
                        }
                        _ => {
                            return RuntimeError(format!(
                                "cannot assign into value of type: {}",
                                value.ty()
                            ))
                            .into()
                        }
                    }
                }

                // and then, finally:
                *location_value = value;

                Ok(None)
            }
            Assignable::List { elements } => {
                let Value::List(_, items) = value else {
                    return RuntimeError(format!(
                        "cannot assign into list pattern: {}",
                        value.ty()
                    ))
                    .into();
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
                    return RuntimeError(format!(
                        "cannot assign into tuple pattern: {}",
                        value.ty()
                    ))
                    .into();
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
    ) -> EvaluationResult<()> {
        match pattern {
            DeclarePattern::Id(id, _) => {
                self.scopes[scope].values.insert(id.clone(), value);
            }
            DeclarePattern::List { elements, rest } => {
                let Value::List(item_type, mut items) = value else {
                    return RuntimeError(format!("cannot assign to list pattern: {}", value.ty()))
                        .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<Value>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len()),
                    ))
                })?;

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
                    return RuntimeError(format!("cannot assign to tiple pattern: {}", value.ty()))
                        .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<Value>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len()),
                    ))
                })?;

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

    pub fn execute(&mut self, scope: usize, stmt: &Stmt) -> EvaluationResult<Value> {
        match stmt {
            Stmt::Break { expr } => {
                let value = expr
                    .clone()
                    .try_map(|expr| self.evaluate(scope, &expr))?
                    .unwrap_or(Value::Nil);

                Err(EvalOther::Break(value))
            }
            Stmt::Return { expr } => {
                let value = expr
                    .clone()
                    .try_map(|expr| self.evaluate(scope, &expr))?
                    .unwrap_or(Value::Nil);

                Err(EvalOther::Return(value))
            }
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Declare { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                self.declare(scope, pattern, value)?;

                Ok(Value::Nil)
            }
            Stmt::Assign { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                let assignable = self.evaluate_assignable(scope, &pattern)?;

                let _ = self.assign(scope, assignable, value)?;

                Ok(Value::Nil)
            }
        }
    }

    // TODO
    pub fn matches_signature(
        name: &Option<Identifier>,
        sig: &FnSig,
        args: &Vec<(Option<Identifier>, Value)>,
    ) -> bool {
        if sig.params.len() != args.len() {
            false
        } else {
            // TODO: find arg that does not fit
            for (pat, (_, arg)) in sig.params.iter().zip(args) {
                // if name == &Some(Identifier("slice".into())) {
                //     println!("fits? pattern {:?} value {:?}", pat, value);
                // }
                match pat {
                    DeclarePattern::List { .. } => {
                        if !(Type::List(Type::Any.into()) >= arg.ty()) {
                            // println!("[{name:?}] does not match sig: list");
                            return false;
                        }
                    }
                    DeclarePattern::Tuple { .. } => {
                        if !(Type::Tuple >= arg.ty()) {
                            // println!("[{name:?}] does not match sig: tuple");
                            return false;
                        }
                    }
                    DeclarePattern::Id(_, Some(param_type)) => {
                        if !(*param_type >= arg.ty()) {
                            // println!(
                            //     "[{name:?}] does not match sig: type, because NOT {} >= {}",
                            //     param_type,
                            //     arg.ty()
                            // );
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
    ) -> EvaluationResult<Value> {
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
            return RuntimeError(format!(
                "could not find matching signature for {}({:?})",
                name.map(|n| n.0).unwrap_or("<anonymous>".into()),
                args.iter()
                    .map(|arg| format!("{}", arg.1.ty()))
                    .collect::<Vec<_>>()
            ))
            .into();
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
                        return RuntimeError(format!(
                            "cannot pass named arg {name} (no param with that name)"
                        ))
                        .into();
                    }
                }
            } else if let Some(pattern) = params_remaining.get(0).cloned() {
                // assign next available param
                params_remaining.remove(0);
                self.declare(execution_scope, &pattern, arg_value)?;
            } else {
                // no params left
                return RuntimeError(format!("no param to pass arg to")).into();
            }
        }

        if params_remaining.len() > 0 {
            // unassigned params
            return RuntimeError(format!("unassigned params left")).into();
        }

        match body {
            FnBody::Code(block) => match self.execute_block(execution_scope, &block) {
                Ok(value) => Ok(value),
                Err(EvalOther::Return(return_value)) => Ok(return_value),
                other => other,
            },
            FnBody::Builtin(f) => f(self, execution_scope),
        }
    }

    pub fn evaluate_assignable(
        &mut self,
        eval_scope: usize,
        pattern: &AssignPattern,
    ) -> EvaluationResult<Assignable> {
        match pattern {
            AssignPattern::Id(id) => {
                let (def_scope, _) = self.lookup(eval_scope, id)?;
                Ok(Assignable::Loc {
                    scope: def_scope,
                    id: id.clone(),
                    indexes: vec![],
                })
            }
            AssignPattern::Index(pattern, index_expr) => {
                match self.evaluate_assignable(eval_scope, &pattern)? {
                    Assignable::Loc {
                        scope,
                        id,
                        mut indexes,
                    } => {
                        indexes.push(match index_expr {
                            Some(index_expr) => {
                                Either::Left(self.evaluate(eval_scope, index_expr)?)
                            }
                            None => Either::Right(()),
                        });

                        Ok(Assignable::Loc { scope, id, indexes })
                    }
                    _ => RuntimeError(format!(
                        "cannot index into list or tuple assignable pattern"
                    ))
                    .into(),
                }
            }
            AssignPattern::List { elements } => {
                let mut assignable_elements = Vec::with_capacity(elements.len());
                for pattern in elements {
                    assignable_elements.push(self.evaluate_assignable(eval_scope, pattern)?);
                }
                Ok(Assignable::List {
                    elements: assignable_elements,
                })
            }
            AssignPattern::Tuple { elements } => {
                let mut assignable_elements = Vec::with_capacity(elements.len());
                for pattern in elements {
                    assignable_elements.push(self.evaluate_assignable(eval_scope, pattern)?);
                }
                Ok(Assignable::Tuple {
                    elements: assignable_elements,
                })
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

    pub fn evaluate(&mut self, scope: usize, expr: &Expr) -> EvaluationResult<Value> {
        match expr {
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::NilLiteral => Ok(Value::Nil),
            Expr::DictLiteral { elements } => {
                let mut dict = Dict::new();
                for (key, value_expr) in elements {
                    let key_value = match key {
                        Either::Left(name) => Value::Str(name.0.to_string().into()),
                        Either::Right(key_expr) => self.evaluate(scope, key_expr)?,
                    };

                    let expr_value = self.evaluate(scope, value_expr)?;

                    dict.0.insert(key_value, expr_value);
                }

                Ok(Value::Dict(dict))
            }
            Expr::StrLiteral { pieces } => {
                let mut build = "".to_string();

                for piece in pieces {
                    match piece {
                        StrLiteralPiece::Fragment(fragment) => {
                            build += fragment;
                        }
                        StrLiteralPiece::Interpolation(expr) => {
                            let value = self.evaluate(scope, expr)?;
                            build += &format!("{}", value);
                            // build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok(Value::Str(Substr::from(build)))
            }
            Expr::Numeric(num) => Ok(Value::Numeric(num.clone())),
            Expr::RegexLiteral { regex } => Ok(Value::Regex(regex.clone())),
            Expr::Variable(id) => {
                let (_, value) = self.lookup(scope, id)?;
                Ok(value.clone())
            }
            Expr::UnaryExpr { expr, op } => {
                let value = self.evaluate(scope, expr)?;
                match op.as_str() {
                    "!" => Ok(value.negate()?),
                    _ => RuntimeError(format!("Unknown unary operation: {op}")).into(),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let left_value = self.evaluate(scope, left)?;

                match op.as_str() {
                    "^" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.pow(right_value)?)
                    }
                    "<<" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.left_shift(right_value)?)
                    }
                    "+" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.add(right_value)?)
                    }
                    "-" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.sub(&right_value)?)
                    }
                    "*" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.mul(right_value)?)
                    }
                    "/" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.div(right_value)?)
                    }
                    "%" => {
                        let right_value = self.evaluate(scope, right)?;
                        Ok(left_value.modulo(right_value)?)
                    }
                    "<" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => Ok(Value::Bool(ord == Ordering::Less)),
                        }
                    }
                    ">" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => Ok(Value::Bool(ord == Ordering::Greater)),
                        }
                    }
                    "==" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => Ok(Value::Bool(ord == Ordering::Equal)),
                        }
                    }
                    "!=" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => Ok(Value::Bool(ord != Ordering::Equal)),
                        }
                    }
                    ">=" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => Ok(Value::Bool(
                                ord == Ordering::Equal || ord == Ordering::Greater,
                            )),
                        }
                    }
                    "<=" => {
                        let right_value = self.evaluate(scope, right)?;
                        match left_value.partial_cmp(&right_value) {
                            None => RuntimeError(format!("cannot compare")).into(),
                            Some(ord) => {
                                Ok(Value::Bool(ord == Ordering::Equal || ord == Ordering::Less))
                            }
                        }
                    }
                    "&&" => {
                        if !left_value.truthy()? {
                            // short-curcuit
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate(scope, right)?;
                        Ok(right_value)
                    }
                    "||" => {
                        if left_value.truthy()? {
                            // short-curcuit
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate(scope, right)?;
                        Ok(right_value)
                    }
                    _ => RuntimeError(format!("Unknown binary operation: {op}")).into(),
                }
            }
            Expr::ListLiteral { elements } => {
                let mut ty = Type::Any;
                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    if let Some(narrowed) = ty.narrow(&expr_value.ty()) {
                        ty = narrowed;
                    } else {
                        return RuntimeError("list contains distinct types".into()).into();
                    }
                    element_values.push(expr_value);
                }

                let list = Value::List(ty, element_values);
                Ok(list)
            }
            Expr::TupleLiteral { elements } => {
                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    element_values.push(expr_value);
                }

                let list = Value::Tuple(element_values);
                Ok(list)
            }
            Expr::Invocation { expr, args } => {
                let expr_value = self.evaluate(scope, expr)?;
                let Value::FnDef(def) = expr_value else {
                    return RuntimeError(format!(
                        "cannot call {} ({} args)",
                        expr_value.ty(),
                        args.len()
                    ))
                    .into();
                };

                let mut evaluated_args = vec![];
                for arg in args {
                    let arg_value = self.evaluate(scope, &arg.expr)?;
                    evaluated_args.push((arg.name.clone(), arg_value));
                }

                Ok(self.invoke(def, evaluated_args)?)
            }
            Expr::AnonymousFn { params, body } => Ok(Value::FnDef(FnDef {
                name: None,
                parent_scope: scope,
                signatures: vec![FnSig {
                    params: params.clone(),
                    body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                }],
            })),
            Expr::If {
                pattern,
                cond,
                then,
                els,
            } => {
                let cond_value = self.evaluate(scope, cond)?;

                let result = if cond_value.truthy()? {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    if let Some(pattern) = pattern {
                        self.declare(execution_scope, pattern, cond_value)?;
                    }

                    self.execute_block(execution_scope, then)?
                } else if let Some(els) = els {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    self.execute_block(execution_scope, els)?
                } else {
                    Value::Nil
                };

                Ok(result)
            }
            Expr::While { cond, body } => {
                let mut result = Value::Nil;
                loop {
                    let cond_value = self.evaluate(scope, cond)?;
                    if !cond_value.truthy()? {
                        return Ok(result);
                    }

                    result = self.execute_block(scope, body)?;
                }
            }
            Expr::DoWhile { cond, body } => loop {
                loop {
                    let mut result = Value::Nil;

                    result = self.execute_block(scope, body)?;

                    if let Some(cond) = cond {
                        let cond_value = self.evaluate(scope, cond)?;
                        if !cond_value.truthy()? {
                            return Ok(result);
                        }
                    } else {
                        return Ok(result);
                    }
                }
            },
            Expr::Loop { body } => Ok(loop {
                match self.execute_block(scope, body) {
                    Err(EvalOther::Break(value)) => break value,
                    Err(other) => return Err(other),
                    Ok(_) => {}
                }
            }),
            Expr::For {
                pattern,
                range,
                body,
            } => {
                let range_value = self.evaluate(scope, range)?;

                let range = match range_value {
                    Value::List(_, values) => values,
                    Value::Tuple(values) => values,
                    _ => {
                        return RuntimeError(format!("cannot for-loop over {}", range_value.ty()))
                            .into();
                    }
                };

                for item in range {
                    let execution_scope = self.scopes.len();
                    self.scopes.push(Scope {
                        parent_scope: Some(scope),
                        values: HashMap::new(),
                    });

                    self.declare(execution_scope, pattern, item)?;

                    let _ = self.execute_block(execution_scope, body)?;
                }

                Ok(Value::Nil)
            }
        }
    }
}

pub fn execute(doc: &Document, stdin: String) -> EvaluationResult<Value> {
    let mut runtime = Runtime::new();

    implement_stdlib(&mut runtime);

    runtime.scopes[0]
        .values
        .insert(id("stdin"), Value::Str(Substr::from(stdin)));

    Ok(runtime.execute_block(0, &doc.body)?)
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
            RuntimeError("list contains distinct types".into()).into()
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
