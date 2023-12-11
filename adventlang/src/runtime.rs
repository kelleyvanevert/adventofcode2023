use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use arcstr::Substr;
use either::Either;
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignPattern, Block, DeclarePattern, Document, Expr, Identifier, Item, Stmt,
        StrLiteralPiece, Type,
    },
    fmt::Fmt,
    parse::parse_document,
    stdlib::implement_stdlib,
    value::{AlRegex, EvalOther, EvaluationResult, Numeric, RuntimeError},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dict(pub Vec<(usize, usize)>);

impl std::hash::Hash for Dict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo Dict::hash")
    }
}

impl Dict {
    pub fn new() -> Dict {
        Self(vec![])
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
    Builtin(fn(&mut Runtime, usize) -> EvaluationResult<(usize, bool)>),
}

impl std::hash::Hash for FnBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo FnBody::hash")
    }
}

#[derive(Debug, Clone, PartialEq)]
/// External value, for testing purposes, and maybe in the future to pass data to and from the runtime
pub enum Ev {
    Nil,
    Bool(bool),
    Str(String),
    Int(i64),
    Double(f64),
    Regex,
    FnDef,
    List(Vec<Ev>),
    Tuple(Vec<Ev>),
    Dict(HashMap<Ev, Ev>),
}

impl Eq for Ev {}

impl std::hash::Hash for Ev {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self == &Ev::Nil {
            return "nil".hash(state);
        } else {
            "value".hash(state);
        }

        match self {
            Ev::Nil => "nil".hash(state),
            Ev::Bool(b) => b.hash(state),
            Ev::Str(s) => s.hash(state),
            Ev::Int(n) => n.hash(state),
            Ev::Double(_) => panic!("cannot hash f64"),
            Ev::Regex => panic!("cannot hash regex"),
            Ev::FnDef => panic!("cannot hash fndef"),
            Ev::List(els) => {
                for el in els {
                    el.hash(state);
                }
            }
            Ev::Tuple(els) => {
                for el in els {
                    el.hash(state);
                }
            }
            Ev::Dict(_) => panic!("cannot hash dict"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Str(Substr),
    Numeric(Numeric),
    Regex(AlRegex),
    FnDef(FnDef),
    List(Type, Vec<usize>),
    Tuple(Vec<usize>),
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
            Value::Tuple(_) => Ok(Value::Bool(false)),
            Value::List(_, _) => Ok(Value::Bool(false)),
            Value::Dict(_) => Ok(Value::Bool(false)),
            _ => RuntimeError(format!("Can't negate {}", self.ty())).into(),
        }
    }

    pub fn left_shift(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(Numeric::Int(a)), Value::Numeric(Numeric::Int(b))) if *b >= 0 => {
                Ok(Value::Numeric(Numeric::Int(a << b)))
            }
            (a, b) => RuntimeError(format!("can't perform {} << {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn pow(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.pow(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn add(&self, other: &Value) -> EvaluationResult<Value> {
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

    pub fn mul(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.mul(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn div(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.div(b))),
            (a, b) => RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn modulo(&self, other: &Value) -> EvaluationResult<Value> {
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

// This type is to `AssignLocation` what `Value` is to `Expr` -- the runtime-internal evaluated version, say
pub enum Location {
    Single(usize),
    List(Vec<Location>),
    Tuple(Vec<Location>),
}

#[derive(Debug)]
pub struct Scope {
    pub parent_scope: Option<usize>,
    pub values: HashMap<Identifier, usize>,
}

impl Scope {
    pub fn root() -> Scope {
        Scope {
            parent_scope: None,
            values: HashMap::new(),
        }
    }

    pub fn get_unchecked(&self, name: &str) -> usize {
        *self.values.get(&Identifier(name.into())).unwrap()
    }
}

pub struct Runtime {
    scopes: Vec<Scope>,
    heap: Vec<Option<Value>>,
    reclaimed: Vec<usize>,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime {
            scopes: vec![Scope::root()],
            heap: vec![],
            reclaimed: vec![],
        }
    }

    pub fn get_scope(&self, id: usize) -> &Scope {
        &self.scopes[id]
    }

    pub fn get_scope_mut(&mut self, id: usize) -> &mut Scope {
        &mut self.scopes[id]
    }

    pub fn new_scope(&mut self, parent_id: usize) -> usize {
        let id = self.scopes.len();
        self.scopes.push(Scope {
            parent_scope: Some(parent_id),
            values: HashMap::new(),
        });
        id
    }

    pub fn get_value(&self, id: usize) -> &Value {
        match self.heap.get(id) {
            Some(Some(value)) => value,
            _ => panic!("no value at position: {}", id),
        }
    }

    pub fn get_value_mut(&mut self, id: usize) -> &mut Value {
        match self.heap.get_mut(id) {
            Some(Some(value)) => value,
            _ => panic!("no value at position: {}", id),
        }
    }

    pub fn get_ty(&self, id: usize) -> Type {
        self.get_value(id).ty()
    }

    pub fn new_value(&mut self, value: Value) -> (usize, bool) {
        if let Some(i) = self.reclaimed.pop() {
            self.heap[i] = Some(value);
            (i, true)
        } else {
            let i = self.heap.len();
            self.heap.push(Some(value));
            (i, true)
        }
    }

    pub fn get_value_ext(&self, loc: usize) -> Result<Ev, String> {
        match self.get_value(loc) {
            Value::Nil => Ok(Ev::Nil),
            Value::Bool(b) => Ok(Ev::Bool(*b)),
            Value::Str(s) => Ok(Ev::Str(s.to_string())),
            Value::Numeric(Numeric::Int(n)) => Ok(Ev::Int(*n)),
            Value::Numeric(Numeric::Double(d)) => Ok(Ev::Double(*d)),
            Value::Regex(_) => Ok(Ev::Regex),
            Value::FnDef(_) => Ok(Ev::FnDef),
            Value::List(_, items) => Ok(Ev::List(
                items
                    .into_iter()
                    .map(|loc| self.get_value_ext(*loc))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Tuple(items) => Ok(Ev::Tuple(
                items
                    .into_iter()
                    .map(|loc| self.get_value_ext(*loc))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Dict(dict) => {
                let pairs = dict
                    .0
                    .iter()
                    .map(|(key_loc, value_loc)| {
                        Ok((
                            self.get_value_ext(*key_loc)?,
                            self.get_value_ext(*value_loc)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, String>>()?;

                Ok(Ev::Dict(HashMap::from_iter(pairs.into_iter())))
            }
        }
    }

    pub fn ensure_new(&mut self, (orig, is_new): (usize, bool)) -> usize {
        if is_new {
            orig
        } else {
            self.new_value(self.get_value(orig).clone()).0
        }
    }

    pub fn clone(&mut self, orig: usize) -> (usize, bool) {
        match self.get_value(orig).clone() {
            Value::List(t, items) => {
                let cloned_items = items.into_iter().map(|item| self.clone(item).0).collect();
                self.new_value(Value::List(t, cloned_items))
            }
            Value::Tuple(items) => {
                let cloned_items = items.into_iter().map(|item| self.clone(item).0).collect();
                self.new_value(Value::Tuple(cloned_items))
            }
            other => self.new_value(other),
        }
    }

    pub fn cmp(&self, a: usize, b: usize) -> Ordering {
        match (self.get_value(a), self.get_value(b)) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Str(a), Value::Str(b)) => a.cmp(b),
            (Value::Numeric(a), Value::Numeric(b)) => a.cmp(b),
            (Value::Regex(a), Value::Regex(b)) => a.0.as_str().cmp(b.0.as_str()),
            (Value::Tuple(a), Value::Tuple(b)) => {
                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    match self.cmp(*a, *b) {
                        Ordering::Equal => {}
                        Ordering::Greater => {
                            return Ordering::Greater;
                        }
                        Ordering::Less => {
                            return Ordering::Less;
                        }
                    }
                }

                a.len().cmp(&b.len())
            }
            (Value::List(_, a), Value::List(_, b)) => {
                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    match self.cmp(*a, *b) {
                        Ordering::Equal => {}
                        Ordering::Greater => {
                            return Ordering::Greater;
                        }
                        Ordering::Less => {
                            return Ordering::Less;
                        }
                    }
                }

                a.len().cmp(&b.len())
            }
            (Value::Dict(a), Value::Dict(b)) => {
                todo!("comparing dicts")
                // a.0.len() == b.0.len()
                //     && a.0.iter().all(|(k, v)| {
                //         if let Some(w) = b.0.get(k) && self.eq(*v, *w) {
                //             true
                //         } else {
                //             false
                //         }
                //     })
                //     && b.0.iter().all(|(k, v)| {
                //         if let Some(w) = a.0.get(k) && self.eq(*v, *w) {
                //             true
                //         } else {
                //             false
                //         }
                //     })
            }

            (Value::Nil, _) => Ordering::Less,
            (_, Value::Nil) => Ordering::Greater,

            _ => panic!("cannot compare values of disparate types"),
        }
    }

    pub fn eq(&self, a: usize, b: usize) -> bool {
        self.cmp(a, b) == Ordering::Equal
    }

    pub fn display_fmt(&self, f: &mut Formatter, loc: usize, toplevel: bool) -> fmt::Result {
        match self.get_value(loc) {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(str) => {
                if toplevel {
                    write!(f, "{}", str)
                } else {
                    write!(f, "\"{}\"", str)
                }
            }
            Value::Numeric(num) => write!(f, "{num}"),
            Value::Regex(r) => write!(f, "/{}/", r.0.as_str()),
            Value::FnDef(_) => write!(f, "[fn]"),
            Value::List(_, list) => {
                write!(f, "[")?;
                let len = list.len();
                for (i, item) in list.iter().enumerate() {
                    self.display_fmt(f, *item, false)?;
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
                    self.display_fmt(f, *item, false)?;
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
                    write!(f, ".")?;
                    self.display_fmt(f, *key, true)?;
                    write!(f, " ")?;
                    self.display_fmt(f, *value, false)?;
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

    pub fn display(&self, loc: usize, toplevel: bool) -> String {
        format!("{}", Fmt(|f| self.display_fmt(f, loc, toplevel)))
    }

    pub fn gc(&mut self, keep: impl IntoIterator<Item = usize>) {
        let mut reachable = vec![false; self.heap.len()];

        for v in keep {
            self.gc_reach(v, &mut reachable);
        }

        for scope in &self.scopes {
            for &v in scope.values.values() {
                self.gc_reach(v, &mut reachable);
            }
        }

        let mut num_reclaimed = 0;
        let mut num_kept = 0;

        for (i, &is_reachable) in reachable.iter().enumerate() {
            if !is_reachable {
                self.reclaimed.push(i);
                self.heap[i] = None;
                num_reclaimed += 1;
            } else {
                num_kept += 1;
            }
        }

        //println!("GC stats:");
        //println!("- reclaimed: {num_reclaimed}");
        //println!("- heap size: {num_kept}");
    }

    fn gc_reach(&self, v: usize, reachable: &mut Vec<bool>) {
        reachable[v] = true;

        match self.get_value(v) {
            Value::List(_, els) => {
                for &el in els {
                    self.gc_reach(el, reachable);
                }
            }
            Value::Tuple(els) => {
                for &el in els {
                    self.gc_reach(el, reachable);
                }
            }
            Value::Dict(dict) => {
                for &(k, v) in &dict.0 {
                    self.gc_reach(k, reachable);
                    self.gc_reach(v, reachable);
                }
            }
            _ => {}
        }
    }

    pub fn builtin(&mut self, name: &str, signatures: impl IntoIterator<Item = FnSig>) {
        let (def, _) = self.new_value(Value::FnDef(FnDef {
            name: Some(name.into()),
            parent_scope: 0,
            signatures: signatures.into_iter().collect(),
        }));

        self.get_scope_mut(0).values.insert(name.into(), def);
    }

    pub fn lookup(&self, scope_id: usize, id: &Identifier) -> EvaluationResult<(usize, usize)> {
        let scope = self.get_scope(scope_id);

        if let Some(&value) = scope.values.get(id) {
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

    pub fn execute_block(
        &mut self,
        scope: usize,
        block: &Block,
    ) -> EvaluationResult<(usize, bool)> {
        let mut result = self.new_value(Value::Nil);

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
                let (def, _) = self.new_value(Value::FnDef(FnDef {
                    name: Some(name.clone()),
                    parent_scope: scope,
                    signatures: vec![FnSig {
                        params: params.clone(),
                        body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                    }],
                }));

                self.get_scope_mut(scope).values.insert(name.clone(), def);
            }
        }

        Ok(())
    }

    pub fn assign(
        &mut self,
        scope: usize,
        assignable: Location,
        value: usize,
    ) -> EvaluationResult<()> {
        match assignable {
            Location::Single(loc) => {
                self.heap[loc] = self.heap[value].clone();
                Ok(())
            }
            Location::List(elements) => todo!("assign list pattern"),
            Location::Tuple(elements) => todo!("assign tuple pattern"),
        }

        // match assignable {
        //     Assignable::Loc { scope, id, indexes } => {
        //         if indexes.len() == 0 {
        //             self.scopes[scope].values.insert(id.clone(), value.clone());
        //             return Ok(None);
        //         }

        //         let mut location_value = self.scopes[scope]
        //             .values
        //             .get_mut(&id)
        //             .expect("assignable location id get failed");

        //         for index_value in indexes {
        //             match location_value {
        //                 Value::List(t, list) => {
        //                     let Ok(i) = index_value.auto_coerce_int() else {
        //                         return RuntimeError(format!(
        //                             "list index must be int, is: {}",
        //                             index_value.ty(),
        //                         ))
        //                         .into();
        //                     };

        //                     if i < 0 {
        //                         return RuntimeError(format!(
        //                             "list index must be positive int, is: {}",
        //                             i,
        //                         ))
        //                         .into();
        //                     }

        //                     let i = i as usize;

        //                     if !(*t >= value.ty()) {
        //                         return RuntimeError(format!(
        //                             "cannot insert value of type {} into list of type {}",
        //                             value.ty(),
        //                             Type::List(t.clone().into())
        //                         ))
        //                         .into();
        //                     }

        //                     if list.len() < i + 1 {
        //                         list.resize(i + 1, Value::Nil);
        //                     }

        //                     location_value = &mut list[i]
        //                 }
        //                 Value::Tuple(list) => {
        //                     let Ok(i) = index_value.auto_coerce_int() else {
        //                         return RuntimeError(format!(
        //                             "tuple index must be int, is: {}",
        //                             index_value.ty(),
        //                         ))
        //                         .into();
        //                     };

        //                     if i < 0 {
        //                         return RuntimeError(format!(
        //                             "tuple index must be positive int, is: {}",
        //                             i,
        //                         ))
        //                         .into();
        //                     }

        //                     let i = i as usize;

        //                     if list.len() < i + 1 {
        //                         list.resize(i + 1, Value::Nil);
        //                     }

        //                     location_value = &mut list[i]
        //                 }
        //                 Value::Dict(dict) => {
        //                     location_value = dict.0.entry(index_value).or_insert(Value::Nil);
        //                 }
        //                 _ => {
        //                     return RuntimeError(format!(
        //                         "cannot assign into value of type: {}",
        //                         value.ty()
        //                     ))
        //                     .into()
        //                 }
        //             }
        //         }

        //         // and then, finally:
        //         *location_value = value;

        //         Ok(None)
        //     }
        //     Assignable::List { elements } => {
        //         let Value::List(_, items) = value else {
        //             return RuntimeError(format!(
        //                 "cannot assign into list pattern: {}",
        //                 value.ty()
        //             ))
        //             .into();
        //         };

        //         for (pattern, value) in elements
        //             .into_iter()
        //             .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
        //         {
        //             self.assign(scope, pattern, value)?;
        //         }

        //         Ok(None)
        //     }
        //     Assignable::Tuple { elements } => {
        //         let Value::Tuple(items) = value else {
        //             return RuntimeError(format!(
        //                 "cannot assign into tuple pattern: {}",
        //                 value.ty()
        //             ))
        //             .into();
        //         };

        //         for (pattern, value) in elements
        //             .into_iter()
        //             .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
        //         {
        //             self.assign(scope, pattern, value)?;
        //         }

        //         Ok(None)
        //     }
        // }
    }

    pub fn declare(
        &mut self,
        scope: usize,
        pattern: &DeclarePattern,
        value: usize,
    ) -> EvaluationResult<()> {
        match pattern {
            DeclarePattern::Id(id, _) => {
                self.get_scope_mut(scope).values.insert(id.clone(), value);
            }
            DeclarePattern::List { elements, rest } => {
                let value = self.get_value(value).clone();
                let Value::List(item_type, mut items) = value else {
                    return RuntimeError(format!("cannot assign to list pattern: {}", value.ty()))
                        .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<usize>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len()),
                    ))
                })?;

                let items = items.clone();

                for (pattern, value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    let (list, _) = self.new_value(Value::List(item_type.clone(), items));
                    self.declare(scope, &DeclarePattern::Id(id, t), list)?;
                }
            }
            DeclarePattern::Tuple { elements, rest } => {
                let value = self.get_value(value).clone();
                let Value::Tuple(mut items) = value else {
                    return RuntimeError(format!("cannot assign to tiple pattern: {}", value.ty()))
                        .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<usize>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len()),
                    ))
                })?;

                for (pattern, value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    let (tuple, _) = self.new_value(Value::Tuple(items));
                    self.declare(scope, &DeclarePattern::Id(id, t), tuple)?;
                }
            }
        }

        Ok(())
    }

    pub fn execute(&mut self, scope: usize, stmt: &Stmt) -> EvaluationResult<(usize, bool)> {
        match stmt {
            Stmt::Break { expr } => {
                let value = expr
                    .clone()
                    .try_map(|expr| self.evaluate(scope, &expr))?
                    .unwrap_or(self.new_value(Value::Nil));

                Err(EvalOther::Break(value))
            }
            Stmt::Return { expr } => {
                let value = expr
                    .clone()
                    .try_map(|expr| self.evaluate(scope, &expr))?
                    .unwrap_or(self.new_value(Value::Nil));

                Err(EvalOther::Return(value))
            }
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Declare { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                let value = self.ensure_new(value);

                self.declare(scope, pattern, value)?;

                Ok(self.new_value(Value::Nil))
            }
            Stmt::Assign { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                let assignable = self.resolve(scope, &pattern)?;

                let value = self.ensure_new(value);

                self.assign(scope, assignable, value)?;

                Ok(self.new_value(Value::Nil))
            }
        }
    }

    // TODO
    pub fn matches_signature(
        &self,
        name: &Option<Identifier>,
        sig: &FnSig,
        args: &Vec<(Option<Identifier>, usize)>,
    ) -> bool {
        if sig.params.len() != args.len() {
            false
        } else {
            // TODO: find arg that does not fit
            for (pat, (_, arg)) in sig.params.iter().zip(args) {
                let value = self.get_value(*arg);
                // if name == &Some(Identifier("slice".into())) {
                //     println!("fits? pattern {:?} value {:?}", pat, value);
                // }
                match pat {
                    DeclarePattern::List { .. } => {
                        if !(Type::List(Type::Any.into()) >= value.ty()) {
                            // println!("[{name:?}] does not match sig: list");
                            return false;
                        }
                    }
                    DeclarePattern::Tuple { .. } => {
                        if !(Type::Tuple >= value.ty()) {
                            // println!("[{name:?}] does not match sig: tuple");
                            return false;
                        }
                    }
                    DeclarePattern::Id(_, Some(param_type)) => {
                        if !(*param_type >= value.ty()) {
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
        def: usize,
        args: Vec<(Option<Identifier>, usize)>,
    ) -> EvaluationResult<(usize, bool)> {
        let Value::FnDef(def) = self.get_value(def) else {
            return RuntimeError(format!(
                "cannot call {} ({} args)",
                self.get_value(def).ty(),
                args.len()
            ))
            .into();
        };

        let FnDef {
            name,
            parent_scope,
            signatures,
        } = def.clone();

        let mut matching_signatures = signatures
            .into_iter()
            .filter(|sig| self.matches_signature(&name, sig, &args))
            .collect::<Vec<_>>();

        let FnSig { params, body } = if matching_signatures.len() == 0 {
            return RuntimeError(format!(
                "could not find matching signature for {}({:?})",
                name.map(|n| n.0).unwrap_or("<anonymous>".into()),
                args.iter()
                    .map(|arg| format!("{}", self.get_value(arg.1).ty()))
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

        let execution_scope = self.new_scope(parent_scope);

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
                        self.get_scope_mut(execution_scope)
                            .values
                            .insert(name, arg_value);
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

    pub fn resolve(&mut self, scope: usize, pattern: &AssignPattern) -> EvaluationResult<Location> {
        match pattern {
            AssignPattern::Id(id) => {
                let (_, location) = self.lookup(scope, id)?;
                Ok(Location::Single(location))
            }
            AssignPattern::Index(pattern, maybe_index_expr) => {
                match self.resolve(scope, &pattern)? {
                    Location::Single(parent) => {
                        let index_loc = maybe_index_expr
                            .clone()
                            .try_map(|expr| self.evaluate(scope, &expr))?
                            .map(|(v, i)| v);

                        let index_val = index_loc.map(|i| self.get_value(i)).cloned();

                        let nil = self.new_value(Value::Nil).0;

                        let matching_dict_key = match (self.get_value(parent), index_loc) {
                            (Value::Dict(dict), Some(i)) => {
                                if let Some(pair) = dict.0.iter().find(|(k, v)| self.eq(*k, i)) {
                                    Some(pair.0)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        };

                        let array_pushed_loc = match (self.get_value_mut(parent), index_loc) {
                            (Value::List(_, els), None) => {
                                els.push(nil);
                                Some(nil)
                            }
                            (Value::Tuple(els), None) => {
                                els.push(nil);
                                Some(nil)
                            }
                            _ => None,
                        };

                        match self.get_value_mut(parent) {
                            Value::List(_, items) | Value::Tuple(items) => {
                                if let Some(index) = index_val {
                                    let Value::Numeric(Numeric::Int(index)) = index else {
                                        return RuntimeError(format!(
                                            "list/tuple assign index must be an int, is a: {}",
                                            index.ty()
                                        ))
                                        .into();
                                    };

                                    let i = if index >= 0 {
                                        if index >= items.len() as i64 {
                                            items.resize(index as usize + 1, nil);
                                        }
                                        index as usize
                                    } else if items.len() as i64 + index >= 0 {
                                        (items.len() as i64 + index) as usize
                                    } else {
                                        return RuntimeError(format!(
                                            "negative index out of bounds"
                                        ))
                                        .into();
                                    };

                                    Ok(Location::Single(items[i]))
                                } else {
                                    Ok(Location::Single(
                                        array_pushed_loc.expect("array pushed location"),
                                    ))
                                }
                            }
                            Value::Dict(dict) => {
                                if let Some(index_loc) = index_loc {
                                    if let Some(loc) = matching_dict_key {
                                        Ok(Location::Single(loc))
                                    } else {
                                        dict.0.push((index_loc, nil));
                                        Ok(Location::Single(nil))
                                    }
                                } else {
                                    RuntimeError(format!("cannot push new items into dict")).into()
                                }
                            }
                            v => RuntimeError(format!(
                                "cannot assign index of value other than list/tuple/dict, is a: {}",
                                v.ty()
                            ))
                            .into(),
                        }
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
                    assignable_elements.push(self.resolve(scope, pattern)?);
                }
                Ok(Location::List(assignable_elements))
            }
            AssignPattern::Tuple { elements } => {
                let mut assignable_elements = Vec::with_capacity(elements.len());
                for pattern in elements {
                    assignable_elements.push(self.resolve(scope, pattern)?);
                }
                Ok(Location::Tuple(assignable_elements))
            }
        }
    }

    pub fn debug_scope(&self, scope: usize) -> String {
        if scope == 0 {
            "RootScope".into()
        } else {
            format!(
                "Scope({}){}",
                self.get_scope(scope)
                    .values
                    .iter()
                    .map(|(key, k)| { format!("{key}: {}", self.get_value(*k).ty()) })
                    .collect::<Vec<_>>()
                    .join(", "),
                match self.get_scope(scope).parent_scope {
                    None => "".into(),
                    Some(parent_id) => format!(" <- {}", self.debug_scope(parent_id)),
                }
            )
        }
    }

    /// (location of the value, whether it was copied or newly computed already during evaluation)
    pub fn evaluate(&mut self, scope: usize, expr: &Expr) -> EvaluationResult<(usize, bool)> {
        match expr {
            Expr::Bool(b) => Ok(self.new_value(Value::Bool(*b))),
            Expr::NilLiteral => Ok(self.new_value(Value::Nil)),
            Expr::DictLiteral { elements } => {
                let mut dict = Dict::new();
                for (key, value_expr) in elements {
                    let (k, _) = match key {
                        Either::Left(name) => self.new_value(Value::Str(name.0.to_string().into())),
                        Either::Right(key_expr) => self.evaluate(scope, key_expr)?,
                    };

                    let (expr_value, _) = self.evaluate(scope, value_expr)?;

                    dict.0.push((k, expr_value));
                }

                Ok(self.new_value(Value::Dict(dict)))
            }
            Expr::StrLiteral { pieces } => {
                let mut build = "".to_string();

                for piece in pieces {
                    match piece {
                        StrLiteralPiece::Fragment(fragment) => {
                            build += fragment;
                        }
                        StrLiteralPiece::Interpolation(expr) => {
                            let (value_loc, _) = self.evaluate(scope, expr)?;
                            build += &format!("{}", self.display(value_loc, true));
                            // build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok(self.new_value(Value::Str(Substr::from(build))))
            }
            Expr::Numeric(num) => Ok(self.new_value(Value::Numeric(num.clone()))),
            Expr::RegexLiteral { regex } => Ok(self.new_value(Value::Regex(regex.clone()))),
            Expr::Variable(id) => {
                let (_, k) = self.lookup(scope, id)?;

                Ok((k, false))
            }
            Expr::UnaryExpr { expr, op } => {
                let k = self.evaluate(scope, expr)?;
                let value = self.get_value(k.0);
                match op.as_str() {
                    "!" => Ok(self.new_value(value.negate()?)),
                    _ => RuntimeError(format!("Unknown unary operation: {op}")).into(),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let le = self.evaluate(scope, left)?;

                // short-circuiting ops
                match op.as_str() {
                    "&&" => {
                        if !self.get_value(le.0).truthy()? {
                            return Ok(le);
                        } else {
                            return Ok(self.evaluate(scope, right)?);
                        }
                    }
                    "||" => {
                        if self.get_value(le.0).truthy()? {
                            return Ok(le);
                        } else {
                            return Ok(self.evaluate(scope, right)?);
                        }
                    }
                    _ => {}
                }

                let ri = self.evaluate(scope, right)?;

                let left_value = self.get_value(le.0);
                let right_value = self.get_value(ri.0);

                match op.as_str() {
                    "^" => return Ok(self.new_value(left_value.pow(right_value)?)),
                    "<<" => return Ok(self.new_value(left_value.left_shift(right_value)?)),
                    "+" => return Ok(self.new_value(left_value.add(right_value)?)),
                    "-" => return Ok(self.new_value(left_value.sub(right_value)?)),
                    "*" => return Ok(self.new_value(left_value.mul(right_value)?)),
                    "/" => return Ok(self.new_value(left_value.div(right_value)?)),
                    "%" => return Ok(self.new_value(left_value.modulo(right_value)?)),
                    _ => {}
                };

                let ord = self.cmp(le.0, ri.0);

                match op.as_str() {
                    "<" => Ok(self.new_value(Value::Bool(ord == Ordering::Less))),
                    ">" => Ok(self.new_value(Value::Bool(ord == Ordering::Greater))),
                    "==" => Ok(self.new_value(Value::Bool(ord == Ordering::Equal))),
                    "!=" => Ok(self.new_value(Value::Bool(ord != Ordering::Equal))),
                    ">=" => Ok(self.new_value(Value::Bool(
                        ord == Ordering::Equal || ord == Ordering::Greater,
                    ))),
                    "<=" => Ok(self
                        .new_value(Value::Bool(ord == Ordering::Equal || ord == Ordering::Less))),
                    _ => RuntimeError(format!("Unknown binary operation: {op}")).into(),
                }
            }
            Expr::ListLiteral { elements } => {
                let mut ty = Type::Any;
                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    if let Some(narrowed) = ty.narrow(&self.get_value(expr_value.0).ty()) {
                        ty = narrowed;
                    } else {
                        return RuntimeError("list contains distinct types".into()).into();
                    }
                    element_values.push(expr_value.0);
                }

                Ok(self.new_value(Value::List(ty, element_values)))
            }
            Expr::TupleLiteral { elements } => {
                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    element_values.push(expr_value.0);
                }

                Ok(self.new_value(Value::Tuple(element_values)))
            }
            Expr::Invocation { expr, args } => {
                let fn_expr_value = self.evaluate(scope, expr)?;

                let mut evaluated_args = vec![];
                for arg in args {
                    let arg_value = self.evaluate(scope, &arg.expr)?;
                    evaluated_args.push((arg.name.clone(), arg_value.0));
                }

                Ok(self.invoke(fn_expr_value.0, evaluated_args)?)
            }
            Expr::AnonymousFn { params, body } => Ok(self.new_value(Value::FnDef(FnDef {
                name: None,
                parent_scope: scope,
                signatures: vec![FnSig {
                    params: params.clone(),
                    body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                }],
            }))),
            Expr::If {
                pattern,
                cond,
                then,
                els,
            } => {
                let cond_value = self.evaluate(scope, cond)?;

                let result = if self.get_value(cond_value.0).truthy()? {
                    let execution_scope = self.new_scope(scope);

                    if let Some(pattern) = pattern {
                        self.declare(execution_scope, pattern, cond_value.0)?;
                    }

                    self.execute_block(execution_scope, then)?
                } else if let Some(els) = els {
                    let execution_scope = self.new_scope(scope);

                    self.execute_block(execution_scope, els)?
                } else {
                    self.new_value(Value::Nil)
                };

                Ok(result)
            }
            Expr::While { cond, body } => {
                let mut result = self.new_value(Value::Nil);
                loop {
                    let cond_value = self.evaluate(scope, cond)?;
                    if !self.get_value(cond_value.0).truthy()? {
                        return Ok(result);
                    }

                    result = self.execute_block(scope, body)?;
                }
            }
            Expr::DoWhile { cond, body } => loop {
                loop {
                    let result = self.execute_block(scope, body)?;

                    if let Some(cond) = cond {
                        let cond_value = self.evaluate(scope, cond)?;
                        if !self.get_value(cond_value.0).truthy()? {
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
                let k = self.evaluate(scope, range)?;
                let range_value = self.get_value(k.0).clone();

                let range = match range_value {
                    Value::List(_, values) => values,
                    Value::Tuple(values) => values,
                    _ => {
                        return RuntimeError(format!("cannot for-loop over {}", range_value.ty()))
                            .into();
                    }
                };

                for item in range {
                    let execution_scope = self.new_scope(scope);

                    self.declare(execution_scope, pattern, item)?;

                    let _ = self.execute_block(execution_scope, body)?;
                }

                Ok(self.new_value(Value::Nil))
            }
        }
    }
}

pub fn execute(doc: &Document, stdin: String) -> EvaluationResult<(Runtime, usize)> {
    let mut runtime = Runtime::new();

    implement_stdlib(&mut runtime);

    let s = runtime.new_value(Value::Str(Substr::from(stdin)));
    runtime.get_scope_mut(0).values.insert(id("stdin"), s.0);

    let r = runtime.execute_block(0, &doc.body)?;
    Ok((runtime, r.0))
}

pub fn execute_simple(code: &str) -> EvaluationResult<Ev> {
    let Some(doc) = parse_document(code) else {
        return RuntimeError("could not parse code".into()).into();
    };

    let (mut runtime, res_loc) = execute(&doc, "".into())?;

    runtime.gc([res_loc]);

    let value = match runtime.get_value_ext(res_loc) {
        Err(e) => {
            return RuntimeError(format!("could not get runtime result: {}", e)).into();
        }
        Ok(value) => value,
    };

    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn list(items: impl IntoIterator<Item = Ev>) -> Ev {
        Ev::List(items.into_iter().collect())
    }

    fn tuple(items: impl IntoIterator<Item = Ev>) -> Ev {
        Ev::Tuple(items.into_iter().collect())
    }

    fn int(n: i64) -> Ev {
        Ev::Int(n)
    }

    fn bool(b: bool) -> Ev {
        Ev::Bool(b)
    }

    fn str(s: &'static str) -> Ev {
        Ev::Str(s.to_string())
    }

    #[test]
    fn some_simple_tests() {
        assert_eq!(
            execute_simple("[1, 2, 3]"),
            Ok(list([int(1), int(2), int(3)]))
        );

        assert_eq!(
            execute_simple(r#""hello {"world"}""#),
            Ok(str("hello world"))
        );

        assert_eq!(
            execute_simple("[1, 2, 3] :map |n| { n * 2 }"),
            Ok(list([int(2), int(4), int(6)]))
        );

        assert_eq!(
            execute_simple("let a = [1,2,3]; let b = a; a[0] = 5; b"),
            Ok(list([int(5), int(2), int(3)]))
        );

        assert_eq!(
            execute_simple("let a = [1,2,3]; let b = clone(a); a[0] = 5; b"),
            Ok(list([int(1), int(2), int(3)]))
        );

        assert_eq!(
            execute_simple(r#""hello world bla" :match / [a-z]{2}/"#),
            Ok(tuple([str(" wo"), int(5)]))
        );

        assert_eq!(execute_simple("[1,2,3] == [1,2,3]"), Ok(bool(true)));

        assert_eq!(execute_simple("[1,2,3] != [1,2,50]"), Ok(bool(true)));

        assert_eq!(
            execute_simple("let d = @{}; d[(2,6)] = 4; d[(2,6)]"),
            Ok(int(4))
        );

        assert_eq!(
            execute_simple("let a = 1; if true { a = 2 }; a"),
            Ok(int(2))
        );

        assert_eq!(execute_simple("let a = 1; let b = a; a = 2; b"), Ok(int(1)));

        assert_eq!(
            execute_simple("let a = []; a []= (1, 2); let i = 0; a[i]"),
            Ok(tuple([int(1), int(2)]))
        );

        assert_eq!(
            execute_simple("let out = []; for let t in [(1,2), (3,4)] { out []= t }; out"),
            Ok(list([tuple([int(1), int(2)]), tuple([int(3), int(4)])]))
        );

        // assert_eq!(
        //     execute_simple(
        //         "
        //         let out = 0;
        //         for let a in [1,2,3] {
        //             let i = 0
        //             while i < 2 {
        //                 i += 1
        //                 a += 10
        //             }
        //             out += a
        //         }
        //         out
        //         "
        //     ),
        //     Ok(int(2))
        // );
    }

    //     #[test]
    //     fn list_literals() {
    //         assert_eq!(
    //             execute(&parse_document(r#"[0]"#).unwrap(), "".into()),
    //             Ok(list(Type::Numeric, vec![int(0)]))
    //         );

    //         assert_eq!(
    //             execute(&parse_document(r#"[0, "hello"]"#).unwrap(), "".into()),
    //             RuntimeError("list contains distinct types".into()).into()
    //         );
    //     }

    //     #[test]
    //     fn bla() {
    //         assert_eq!(
    //             execute(&parse_document("range(0, 10)").unwrap(), "".into()),
    //             Ok(list(Type::Numeric, (0..10).map(int).collect()))
    //         );

    //         assert_eq!(
    //             execute(&parse_document("range(6, 2)").unwrap(), "".into()),
    //             Ok(list(Type::Numeric, vec![]))
    //         );
    //     }

    //     #[test]
    //     fn patterns() {
    //         assert_eq!(
    //             execute(
    //                 &parse_document("let [a, b] = [2, 3]; (a, b)").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(tuple(vec![int(2), int(3)]))
    //         );

    //         assert_eq!(
    //             execute(
    //                 &parse_document("let [a, b] = [2]; (a, b)").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(tuple(vec![int(2), Value::Nil]))
    //         );

    //         assert_eq!(
    //             execute(
    //                 &parse_document("let [a, b] = [2, 3, 4]; (a, b)").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(tuple(vec![int(2), int(3)]))
    //         );

    //         assert_eq!(
    //             execute(&parse_document("let [] = [2, 3, 4]").unwrap(), "".into()),
    //             Ok(Value::Nil)
    //         );

    //         assert_eq!(
    //             execute(
    //                 &parse_document("let a = 4; let b = 5; [a, b] = [1, 2, 3, 4]; (a, b)").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(tuple(vec![int(1), int(2)]))
    //         );
    //     }

    //     #[test]
    //     fn if_declare() {
    //         assert_eq!(
    //             execute(
    //                 &parse_document("if (let answer = 42) { answer }").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(int(42))
    //         );
    //     }

    //     #[test]
    //     fn test_sorting() {
    //         assert_eq!(
    //             execute(
    //                 &parse_document("[1, 9, 3, 2, 7] :sort_by_key |n| { n }").unwrap(),
    //                 "".into()
    //             ),
    //             Ok(list(
    //                 Type::Numeric,
    //                 vec![int(1), int(2), int(3), int(7), int(9)]
    //             ))
    //         );

    //         assert_eq!(
    //             execute(
    //                 &parse_document(
    //                     "[(nil, 1), (nil, 9), (nil, 3), (nil, 2), (nil, 7)] :sort_by_key |n| { n[1] }"
    //                 )
    //                 .unwrap(),
    //                 "".into()
    //             ),
    //             Ok(list(
    //                 Type::Tuple,
    //                 vec![
    //                     tuple(vec![Value::Nil, int(1)]),
    //                     tuple(vec![Value::Nil, int(2)]),
    //                     tuple(vec![Value::Nil, int(3)]),
    //                     tuple(vec![Value::Nil, int(7)]),
    //                     tuple(vec![Value::Nil, int(9)])
    //                 ]
    //             ))
    //         );
    //     }
}
