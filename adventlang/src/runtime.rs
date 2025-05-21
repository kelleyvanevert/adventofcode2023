use std::{
    cmp::Ordering,
    collections::hash_map::RandomState,
    fmt::{self, Display, Formatter},
    hash::{BuildHasher, Hash, Hasher},
};

use arcstr::Substr;
use either::Either;
use fxhash::FxHashMap;
use safe_gc::{Gc, Heap, Root, Trace};
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignPattern, Block, Declarable, DeclareGuardExpr, DeclarePattern, Document, Expr,
        Identifier, Item, Stmt, StrLiteralPiece, Type,
    },
    fmt::Fmt,
    parse::parse_document,
    runtime_builtins::RuntimeLike,
    stdlib::implement_stdlib,
    types::param_permits_arg,
    value::{AlRegex, EvalOther, EvaluationResult, Numeric, RuntimeError},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    ty: Option<(Type, Type)>,
    data: FxHashMap<u64, Vec<(Value, Value)>>,
}

impl Trace for Dict {
    fn trace(&self, collector: &mut safe_gc::Collector) {
        for (key, value) in self.entries() {
            key.trace(collector);
            value.trace(collector);
        }
    }
}

impl std::hash::Hash for Dict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo Dict::hash")
    }
}

impl Dict {
    pub fn new() -> Dict {
        Self {
            ty: None,
            data: FxHashMap::default(),
        }
    }

    pub fn entries<'a>(&'a self) -> impl Iterator<Item = (&'a Value, &'a Value)> + 'a {
        self.data
            .iter()
            .flat_map(|(_, bucket_entries)| bucket_entries.iter())
            .map(|(k, v)| (k, v))
    }

    pub fn get(&self, runtime: &Runtime, k: Value) -> Option<(Value, Value)> {
        if let Some(bucket) = self.data.get(&runtime.hash(&k)) {
            for (q, v) in bucket {
                if runtime.eq(&k, q) {
                    return Some((k, v.clone()));
                }
            }
        }

        None
    }

    pub fn insert(&mut self, runtime: &Runtime, k: Value, v: Value) {
        let key_hash = runtime.hash(&k);
        self.insert_known_hash(key_hash, k, v);
    }

    pub fn insert_known_hash(&mut self, key_hash: u64, k: Value, v: Value) {
        let vec = self.data.entry(key_hash).or_default().push((k, v));
    }

    // pub fn entry(&self, runtime: &Runtime) {}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str(pub String);

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    name: Option<Identifier>,
    parent_scope: Gc<Scope>,
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
    pub params: Vec<Declarable>,
    pub result: Type,
    pub body: FnBody,
}

impl Display for FnSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        let mut i = 0;
        for param in &self.params {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
            i += 1;
        }
        write!(f, ") -> {}", self.result)?;
        Ok(())
    }
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
    Builtin(fn(&mut Runtime, Gc<Scope>) -> EvaluationResult<Value>),
}

impl std::hash::Hash for FnBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo FnBody::hash")
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Dict(FxHashMap<Ev, Ev>),
}

impl Display for Ev {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ev::Nil => write!(f, "nil"),
            Ev::Bool(b) => write!(f, "{b}"),
            Ev::Str(s) => {
                write!(f, "\"{}\"", s)
            }
            Ev::Int(n) => write!(f, "{n}"),
            Ev::Double(d) => write!(f, "{d}"),
            Ev::Regex => write!(f, "[regex]",),
            Ev::FnDef => write!(f, "[fn]"),
            Ev::List(list) => {
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
            Ev::Tuple(list) => {
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
            Ev::Dict(dict) => {
                write!(f, "@{{")?;
                let mut i = 0;
                for (key, value) in dict.iter() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, ".")?;
                    write!(f, "{key}")?;
                    write!(f, " ")?;
                    write!(f, "{value}")?;
                    i += 1;
                }
                if i == 1 {
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
        }
    }
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
    List(Gc<List>),
    Tuple(Gc<Tuple>),
    Dict(Gc<Dict>),
}

impl Value {
    // intentionally not as implementation of `Trace`, so I don't accidentally put a `Value` on the heap
    fn trace(&self, collector: &mut safe_gc::Collector) {
        match self {
            Value::List(list) => collector.edge(*list),
            Value::Tuple(tuple) => collector.edge(*tuple),
            Value::Dict(dict) => collector.edge(*dict),
            Value::FnDef(def) => collector.edge(def.parent_scope),
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub ty: Type,
    pub elements: Vec<Value>,
}

impl Trace for List {
    fn trace(&self, collector: &mut safe_gc::Collector) {
        for el in &self.elements {
            el.trace(collector);
        }
    }
}

impl List {
    fn ty(&self) -> Type {
        Type::List(self.ty.clone().into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub ty: Option<Vec<Type>>,
    pub elements: Vec<Value>,
}

impl Trace for Tuple {
    fn trace(&self, collector: &mut safe_gc::Collector) {
        for el in &self.elements {
            el.trace(collector);
        }
    }
}

impl Tuple {
    fn ty(&self) -> Type {
        Type::Tuple(self.ty.clone())
    }
}

impl Value {
    pub fn negate(&self) -> EvaluationResult<Value> {
        match self {
            Value::Nil => Ok(Value::Bool(true)),
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Str(_) => RuntimeError(format!("Can't negate str")).into(),
            Value::Numeric(n) => Ok(Value::Numeric(n.negate()?)),
            Value::Tuple(_) => Ok(Value::Bool(false)),
            Value::List(_) => Ok(Value::Bool(false)),
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
            (a, b) => RuntimeError(format!("can't perform {} ^ {}", a.ty(), b.ty())).into(),
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
            (a, b) => RuntimeError(format!("can't perform {} - {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn mul(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.mul(b))),
            (a, b) => RuntimeError(format!("can't perform {} * {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn div(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.div(b))),
            (a, b) => RuntimeError(format!("can't perform {} / {}", a.ty(), b.ty())).into(),
        }
    }

    pub fn modulo(&self, other: &Value) -> EvaluationResult<Value> {
        match (self, other) {
            (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.modulo(b))),
            (a, b) => RuntimeError(format!("can't perform {} % {}", a.ty(), b.ty())).into(),
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
            Value::List(_) => "<list>".into(),
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
            Value::List(_) => Type::List(Type::Any.into()),
            Value::Tuple(_) => Type::Tuple(None),
            Value::Dict(_) => Type::Dict(None),
        }
    }

    // pub fn ty(&self, heap: &Heap) -> Type {
    //     match self {
    //         Value::Nil => Type::Nil,
    //         Value::Bool(_) => Type::Bool,
    //         Value::Str(_) => Type::Str,
    //         Value::Numeric(_) => Type::Numeric,
    //         Value::Regex(_) => Type::Regex,
    //         Value::FnDef(_) => Type::FnDef,
    //         Value::List(list) => heap[*list].ty(),
    //         Value::Tuple(tuple) => heap[*tuple].ty(),
    //         Value::Dict(_) => Type::Dict(None),
    //     }
    // }

    pub fn is_nil(&self) -> bool {
        self == &Value::Nil
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}

// This type is to `AssignLocation` what `Value` is to `Expr` -- the runtime-internal evaluated version, say
pub enum Location {
    Single(Gc<Scope>, Identifier),
    ListIndex(Gc<List>, usize),
    TupleIndex(Gc<Tuple>, usize),
    DictIndex(Gc<Dict>, Value),
    List(Vec<Location>),
    Tuple(Vec<Location>),
}

#[derive(Debug)]
pub struct Scope {
    pub parent_scope: Option<Gc<Scope>>,
    pub values: FxHashMap<Identifier, Value>,
}

impl Trace for Scope {
    fn trace(&self, collector: &mut safe_gc::Collector) {
        if let Some(parent_scope) = self.parent_scope {
            collector.edge(parent_scope);
        }
        for val in self.values.values() {
            val.trace(collector);
        }
    }
}

impl Scope {
    pub fn root() -> Scope {
        Scope {
            parent_scope: None,
            values: FxHashMap::default(),
        }
    }
}

pub struct Runtime {
    random_state: RandomState,
    root_scope: Root<Scope>,
    pub heap: Heap,
}

impl Runtime {
    pub fn new() -> Runtime {
        let mut heap = Heap::new();

        let mut runtime = Runtime {
            random_state: RandomState::new(),
            root_scope: heap.alloc(Scope::root()),
            heap,
        };

        implement_stdlib(&mut runtime);

        runtime
    }

    pub fn execute_document(
        &mut self,
        doc: &Document,
        stdin: Option<String>,
    ) -> EvaluationResult<Ev> {
        let scope = &self.root_scope;

        if let Some(stdin) = stdin {
            let val = Value::Str(Substr::from(stdin));
            self.heap[scope].values.insert(id("stdin"), val);
        }

        let val = self.execute_block(scope.unrooted(), &doc.body)?;

        let value = match self.convert_value(&val) {
            Err(e) => {
                return RuntimeError(format!("could not externalize runtime result: {}", e)).into();
            }
            Ok(value) => value,
        };

        Ok(value)
    }

    pub fn get_unchecked(&self, scope: Gc<Scope>, name: &str) -> &Value {
        self.heap[scope]
            .values
            .get(&Identifier(name.into()))
            .unwrap()
    }

    pub fn create_subscope(&mut self, parent_scope: Gc<Scope>) -> Gc<Scope> {
        self.heap
            .alloc(Scope {
                parent_scope: Some(parent_scope),
                values: FxHashMap::default(),
            })
            .unrooted()
    }

    pub fn convert_value(&self, val: &Value) -> Result<Ev, String> {
        match val {
            Value::Nil => Ok(Ev::Nil),
            Value::Bool(b) => Ok(Ev::Bool(*b)),
            Value::Str(s) => Ok(Ev::Str(s.to_string())),
            Value::Numeric(Numeric::Int(n)) => Ok(Ev::Int(*n)),
            Value::Numeric(Numeric::Double(d)) => Ok(Ev::Double(*d)),
            Value::Regex(_) => Ok(Ev::Regex),
            Value::FnDef(_) => Ok(Ev::FnDef),
            Value::List(items) => Ok(Ev::List(
                self.heap[*items]
                    .elements
                    .iter()
                    .map(|val| self.convert_value(val))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Tuple(items) => Ok(Ev::Tuple(
                self.heap[*items]
                    .elements
                    .iter()
                    .map(|val| self.convert_value(val))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Dict(dict) => {
                let pairs = self.heap[*dict]
                    .entries()
                    .map(|(key, val)| Ok((self.convert_value(&key)?, self.convert_value(&val)?)))
                    .collect::<Result<Vec<_>, String>>()?;

                Ok(Ev::Dict(FxHashMap::from_iter(pairs.into_iter())))
            }
        }
    }

    // currently a shallow clone
    pub fn clone(&mut self, val: Value) -> Value {
        match val {
            Value::List(list) => {
                let list = self.heap[list].clone();
                Value::List(self.heap.alloc(list).unrooted())
            }

            // primitive
            _ => val.clone(),
        }
        // match self.get_value(orig).clone() {
        //     Value::List(t, items) => {
        //         let cloned_items = items.into_iter().map(|item| self.clone(item).0).collect();
        //         self.new_value(Value::List(t, cloned_items))
        //     }
        //     Value::Tuple(ts, items) => {
        //         let cloned_items = items.into_iter().map(|item| self.clone(item).0).collect();
        //         self.new_value(Value::Tuple(ts, cloned_items))
        //     }
        //     other => self.new_value(other),
        // }
    }

    pub fn hash(&self, value: &Value) -> u64 {
        let mut h = self.random_state.build_hasher();
        self.hash_do(&mut h, value);
        let hash = h.finish();
        // println!("runtime hash {} => {}", self.display(v, false), hash);
        hash
    }

    fn hash_do<H: Hasher>(&self, h: &mut H, value: &Value) {
        match value {
            Value::Nil => ().hash(h),
            Value::Bool(b) => b.hash(h),
            Value::Str(s) => s.hash(h),
            Value::Numeric(n) => n.hash(h),
            Value::Regex(a) => a.hash(h),
            Value::Tuple(els) => {
                for el in &self.heap[*els].elements {
                    self.hash_do(h, el);
                }
            }
            Value::List(els) => {
                for el in &self.heap[*els].elements {
                    self.hash_do(h, el);
                }
            }
            Value::Dict(dict) => {
                for (k, v) in self.heap[*dict].entries() {
                    self.hash_do(h, k);
                    self.hash_do(h, v);
                }
            }
            Value::FnDef(def) => {
                panic!("hash fndefs")
            }
        }
    }

    pub fn cmp(&self, a: &Value, b: &Value) -> Ordering {
        match (a, b) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Str(a), Value::Str(b)) => a.cmp(b),
            (Value::Numeric(a), Value::Numeric(b)) => a.cmp(b),
            (Value::Regex(a), Value::Regex(b)) => a.0.as_str().cmp(b.0.as_str()),
            (Value::Tuple(a), Value::Tuple(b)) => {
                let a = &self.heap[*a];
                let b = &self.heap[*b];

                for (a, b) in a.elements.iter().zip(b.elements.iter()) {
                    match self.cmp(a, b) {
                        Ordering::Equal => {}
                        Ordering::Greater => {
                            return Ordering::Greater;
                        }
                        Ordering::Less => {
                            return Ordering::Less;
                        }
                    }
                }

                a.elements.len().cmp(&b.elements.len())
            }
            (Value::List(a), Value::List(b)) => {
                let a = &self.heap[*a];
                let b = &self.heap[*b];

                for (a, b) in a.elements.iter().zip(b.elements.iter()) {
                    match self.cmp(a, b) {
                        Ordering::Equal => {}
                        Ordering::Greater => {
                            return Ordering::Greater;
                        }
                        Ordering::Less => {
                            return Ordering::Less;
                        }
                    }
                }

                a.elements.len().cmp(&b.elements.len())
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

            (a, b) => panic!(
                "cannot compare values of disparate types: {} v {}",
                // a,
                a.ty(),
                // b,
                b.ty()
            ),
        }
    }

    pub fn eq(&self, a: &Value, b: &Value) -> bool {
        self.cmp(a, b) == Ordering::Equal
    }

    fn display_fmt(&self, f: &mut Formatter, val: &Value, toplevel: bool) -> fmt::Result {
        match val {
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
            Value::List(list) => {
                let els = &self.heap[*list].elements;

                write!(f, "[")?;
                let len = els.len();
                for (i, item) in els.iter().enumerate() {
                    self.display_fmt(f, item, false)?;
                    if i < len - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Tuple(list) => {
                let els = &self.heap[*list].elements;

                write!(f, "(")?;
                let len = els.len();
                for (i, item) in els.iter().enumerate() {
                    self.display_fmt(f, item, false)?;
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
                let mut i = 0;
                for (key, val) in self.heap[*dict].entries() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, ".")?;
                    self.display_fmt(f, &key, true)?;
                    write!(f, " ")?;
                    self.display_fmt(f, &val, false)?;
                    i += 1;
                }
                if i == 1 {
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
        }
    }

    pub fn display(&self, val: &Value, toplevel: bool) -> String {
        format!("{}", Fmt(|f| self.display_fmt(f, val, toplevel)))
    }

    pub fn lookup(
        &self,
        scope: Gc<Scope>,
        id: &Identifier,
    ) -> EvaluationResult<(Gc<Scope>, Value)> {
        if let Some(val) = self.heap[scope].values.get(id) {
            return Ok((scope, val.clone()));
        }

        if let Some(parent_scope) = self.heap[scope].parent_scope {
            match self.lookup(parent_scope, id) {
                Ok(res) => Ok(res),
                Err(_) => RuntimeError(format!(
                    "variable `{id}` does not exist in scope tree",
                    // self.debug_scope(scope_id)
                ))
                .into(),
            }
        } else {
            RuntimeError(format!(
                "variable `{id}` does not exist in scope tree",
                // self.debug_scope(scope_id)
            ))
            .into()
        }
    }

    pub fn execute_block(&mut self, scope: Gc<Scope>, block: &Block) -> EvaluationResult<Value> {
        let mut result = Value::Nil;

        for item in &block.items {
            self.define(scope, item)?;
        }

        for stmt in &block.stmts {
            result = self.execute(scope, stmt)?;
        }

        Ok(result)
    }

    pub fn define(&mut self, scope: Gc<Scope>, item: &Item) -> EvaluationResult<()> {
        match item {
            Item::NamedFn { name, params, body } => {
                let def = Value::FnDef(FnDef {
                    name: Some(name.clone()),
                    parent_scope: scope,
                    signatures: vec![FnSig {
                        params: params.clone(),
                        body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                        result: Type::Any,
                    }],
                });

                self.heap[scope].values.insert(name.clone(), def);
            }
        }

        Ok(())
    }

    pub fn assign(&mut self, location: Location, value: Value) -> EvaluationResult<()> {
        match location {
            Location::Single(scope, id) => {
                self.heap[scope].values.insert(id, value);
                Ok(())
            }
            Location::ListIndex(list, index) => {
                self.heap[list].elements[index] = value;
                Ok(())
            }
            Location::TupleIndex(tuple, index) => {
                self.heap[tuple].elements[index] = value;
                Ok(())
            }
            Location::DictIndex(dict, key) => {
                let key_hash = self.hash(&key);
                self.heap[dict].insert_known_hash(key_hash, key, value);
                Ok(())
            }
            Location::List(elements) => {
                let Value::List(list) = value else {
                    return RuntimeError(format!("cannot assign to list pattern: {}", value.ty()))
                        .into();
                };

                for (location, value) in elements.into_iter().zip(
                    self.heap[list]
                        .elements
                        .clone()
                        .into_iter()
                        .chain(std::iter::repeat(Value::Nil)),
                ) {
                    self.assign(location, value)?;
                }

                Ok(())
            }
            Location::Tuple(elements) => {
                let Value::Tuple(tuple) = value else {
                    return RuntimeError(format!("cannot assign to tuple pattern: {}", value.ty()))
                        .into();
                };

                for (location, value) in elements.into_iter().zip(
                    self.heap[tuple]
                        .elements
                        .clone()
                        .into_iter()
                        .chain(std::iter::repeat(Value::Nil)),
                ) {
                    self.assign(location, value)?;
                }

                Ok(())
            }
        }
    }

    pub fn check_declare_guard(
        &mut self,
        _scope: Gc<Scope>,
        guard: &DeclareGuardExpr,
        value: &Value,
    ) -> (Identifier, bool) {
        match guard {
            DeclareGuardExpr::Unguarded(id) => (id.clone(), true),
            DeclareGuardExpr::Some(id) => (id.clone(), value != &Value::Nil),
        }
    }

    /// returns:
    /// - not successful -> None
    /// - else -> Some(whether evaluates truthy)
    pub fn declare(
        &mut self,
        scope: Gc<Scope>,
        pattern: &DeclarePattern,
        value: Value,
    ) -> EvaluationResult<Option<bool>> {
        match pattern {
            DeclarePattern::Declare { guard, .. } => {
                let (id, evaluates_truthy) = self.check_declare_guard(scope, guard, &value);
                self.heap[scope].values.insert(id, value);
                Ok(Some(evaluates_truthy))
            }
            DeclarePattern::List { elements, rest } => {
                // list = { item_type, mut items}
                let Value::List(list) = value else {
                    return Ok(None);
                };

                let item_type = self.heap[list].ty.clone();
                let list = &mut self.heap[list];

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<Value>), EvalOther>((
                        id,
                        t,
                        list.elements
                            .split_off(elements.len().min(list.elements.len())),
                    ))
                })?;

                let items = list.elements.clone();

                for (Declarable { pattern, fallback }, mut value) in elements
                    .into_iter()
                    .zip(items.into_iter().chain(std::iter::repeat(Value::Nil)))
                {
                    if let Some(fallback_expr) = fallback
                        && &value == &Value::Nil
                    {
                        value = self.evaluate(scope, fallback_expr)?;
                    }

                    if self.declare(scope, pattern, value)?.is_none() {
                        return Ok(None);
                    }
                }

                if let Some((id, ty, items)) = assign_rest_later {
                    let list = Value::List(
                        self.heap
                            .alloc(List {
                                ty: item_type.clone(),
                                elements: items,
                            })
                            .unrooted(),
                    );
                    if self
                        .declare(
                            scope,
                            &DeclarePattern::Declare {
                                guard: DeclareGuardExpr::Unguarded(id),
                                ty,
                            },
                            list,
                        )?
                        .is_none()
                    {
                        return Ok(None);
                    }
                }

                Ok(Some(true))
            }
            DeclarePattern::Tuple { elements, rest } => {
                todo!()
                // let value = self.get_value(value).clone();
                // let Value::Tuple(ts, mut items) = value else {
                //     return Ok(None);
                // };

                // let assign_rest_later = rest.clone().try_map(|(id, t)| {
                //     Ok::<(Identifier, Option<Type>, Vec<usize>), EvalOther>((
                //         id,
                //         t,
                //         items.split_off(elements.len().min(items.len())),
                //     ))
                // })?;

                // for (Declarable { pattern, fallback }, mut value) in elements.into_iter().zip(
                //     items
                //         .into_iter()
                //         .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                // ) {
                //     if let Some(fallback_expr) = fallback
                //         && self.get_value(value) == &Value::Nil
                //     {
                //         value = self.evaluate(scope, fallback_expr)?.0;
                //     }

                //     if self.declare(scope, pattern, value)?.is_none() {
                //         return Ok(None);
                //     }
                // }

                // if let Some((id, ty, items)) = assign_rest_later {
                //     let (tuple, _) = self.new_value(Value::Tuple(ts, items));
                //     if self
                //         .declare(
                //             scope,
                //             &DeclarePattern::Declare {
                //                 guard: DeclareGuardExpr::Unguarded(id),
                //                 ty,
                //             },
                //             tuple,
                //         )?
                //         .is_none()
                //     {
                //         return Ok(None);
                //     }
                // }

                // Ok(Some(true))
            }
        }
    }

    pub fn execute(&mut self, scope: Gc<Scope>, stmt: &Stmt) -> EvaluationResult<Value> {
        match stmt {
            Stmt::Continue { label } => {
                return Err(EvalOther::Continue(label.clone()));
            }
            Stmt::Break { expr } => {
                return Err(EvalOther::Break(
                    expr.clone()
                        .try_map(|expr| self.evaluate(scope, &expr))?
                        .unwrap_or(Value::Nil),
                ));
            }
            Stmt::Return { expr } => {
                return Err(EvalOther::Return(
                    expr.clone()
                        .try_map(|expr| self.evaluate(scope, &expr))?
                        .unwrap_or(Value::Nil),
                ));
            }
            Stmt::Expr { expr } => self.evaluate(scope, expr),
            Stmt::Declare { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                if self.declare(scope, pattern, value)?.is_none() {
                    return RuntimeError(format!(
                        "could not declare value into pattern: {}",
                        pattern
                    ))
                    .into();
                }

                Ok(Value::Nil)
            }
            Stmt::Assign { pattern, expr } => {
                let value = self.evaluate(scope, expr)?;

                let assignable = self.resolve(scope, &pattern)?;

                self.assign(assignable, value)?;

                Ok(Value::Nil)
            }
        }
    }

    pub fn select_signature(
        &self,
        name: Option<Identifier>,
        signatures: Vec<FnSig>,
        args: Vec<(&Option<Identifier>, Type)>,
    ) -> Option<(FnSig, Vec<Option<usize>>)> {
        let mut matches = signatures
            .into_iter()
            .filter_map(|sig| {
                // println!(
                //     "does this sig match? fn: {}, args: ({}), sig: ({})",
                //     name.as_ref().map(|id| id.0.as_str()).unwrap_or("<unknown>"),
                //     args.iter()
                //         .map(|(_, arg)| self.display(*arg, false))
                //         .collect::<Vec<_>>()
                //         .join(", "),
                //     sig.params
                //         .iter()
                //         .map(|t| { format!("{}", t) })
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // );

                let min_args = sig.params.iter().filter(|p| p.fallback.is_none()).count();
                let max_args = sig.params.len();

                // index of param => index of arg to be assigned to it (none if default)
                let mut assign_params = vec![None; sig.params.len()];

                if args.len() < min_args || args.len() > max_args {
                    return None;
                }

                // try to assign and type each arg:
                for (arg_index, (name, ty)) in args.iter().enumerate() {
                    let param_index;

                    // try to assign it
                    if let Some(name) = name {
                        // if named, assign to the name (if doesn't exist, fail)
                        if let Some(i) = sig
                            .params
                            .iter()
                            .position(|decl| decl.pattern.is_named(name.clone()))
                        {
                            param_index = i;
                            assign_params[param_index] = Some(arg_index);
                            // (we could also check if double-assigning named param,
                            //  but, ideally that's already detected during parse)
                        } else {
                            // cannot pass named arg, because no param with that name
                            return None;
                        }
                    } else {
                        // else if unnamed, assign to next slot
                        if let Some(i) = assign_params.iter().position(|p| p.is_none()) {
                            param_index = i;
                            assign_params[param_index] = Some(arg_index);
                        } else {
                            // cannot pass arg, because no params remaining
                            return None;
                        }
                    }

                    // type-check it
                    if !param_permits_arg(&sig.params[param_index].pattern, ty) {
                        return None;
                    }
                }

                for i in 0..assign_params.len() {
                    if assign_params[i].is_none() && sig.params[i].fallback.is_none() {
                        // param remained unassigned and there's no fallback
                        return None;
                    }
                }

                Some((sig, assign_params))
            })
            .collect::<Vec<_>>();

        if matches.len() == 1 {
            matches.pop()
        } else {
            None
            // unimplemented!(
            //     "todo select best fn signature for: {}, arg types: ({}), possible signatures: {}",
            //     name.map(|n| n.0).unwrap_or("<unknown>".into()),
            //     args.iter()
            //         .map(|(_, ty)| format!("{}", ty))
            //         .collect::<Vec<_>>()
            //         .join(", "),
            //     matches
            //         .iter()
            //         .enumerate()
            //         .map(|(i, (sig, _))| format!("#{} {}:", i + 1, sig))
            //         .collect::<Vec<_>>()
            //         .join(", ")
            // )
        }
    }

    pub fn invoke(
        &mut self,
        def: &Value,
        args: Vec<(Option<Identifier>, Value)>,
    ) -> EvaluationResult<Value> {
        let Value::FnDef(def) = def else {
            return RuntimeError(format!("cannot call {} ({} args)", def.ty(), args.len())).into();
        };

        // todo find a way to not have to clone this
        let FnDef {
            name,
            parent_scope,
            signatures,
        } = def.clone();

        let Some((
            FnSig {
                params,
                body,
                result,
            },
            assignments,
        )) = self.select_signature(
            name.clone(),
            signatures,
            args.iter().map(|(id, v)| (id, v.ty())).collect::<Vec<_>>(),
        )
        else {
            return RuntimeError(format!(
                "could not find matching signature, fn: {}, arg types: ({})",
                name.map(|n| n.0).unwrap_or("<unknown>".into()),
                args.iter()
                    .map(|arg| format!("{}", arg.1.ty()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .into();
        };

        let execution_scope = self.create_subscope(parent_scope);

        for (param_index, assign_arg) in assignments.into_iter().enumerate() {
            if let Some(arg_index) = assign_arg {
                let mut arg_val = args[arg_index].1.clone();
                if let Some(fallback_expr) = &params[param_index].fallback
                    && arg_val == Value::Nil
                {
                    arg_val = self.evaluate(parent_scope, &fallback_expr)?;
                }
                if self
                    .declare(execution_scope, &params[param_index].pattern, arg_val)?
                    .is_none()
                {
                    // TODO maybe also check the guard?

                    return RuntimeError(format!(
                        "could not declare argument into parameter pattern"
                    ))
                    .into();
                }
            } else {
                let fallback_expr = &params[param_index]
                    .fallback
                    .as_ref()
                    .expect("already checked that this param has a fallback");

                let arg_val = self.evaluate(parent_scope, &fallback_expr)?;

                if self
                    .declare(execution_scope, &params[param_index].pattern, arg_val)?
                    .is_none()
                {
                    // TODO maybe also check the guard?

                    return RuntimeError(format!(
                        "could not declare argument into parameter pattern"
                    ))
                    .into();
                }
            }
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

    pub fn resolve(
        &mut self,
        scope: Gc<Scope>,
        pattern: &AssignPattern,
    ) -> EvaluationResult<Location> {
        match pattern {
            AssignPattern::Id(id) => {
                let (scope, _) = self.lookup(scope, id)?;
                Ok(Location::Single(scope, id.clone()))
            }
            AssignPattern::Index(pattern, maybe_index_expr) => {
                match self.resolve(scope, &pattern)? {
                    Location::Single(container_scope, container_id) => {
                        let index = maybe_index_expr
                            .clone()
                            .try_map(|expr| self.evaluate(scope, &expr))?;

                        let container = self.heap[container_scope]
                            .values
                            .get(&container_id)
                            .unwrap()
                            .clone();

                        // This is a push operation, e.g.:
                        // arr []= el
                        if index.is_none() {
                            match container {
                                Value::List(list) => {
                                    let i = self.heap[list].elements.len();
                                    self.heap[list].elements.push(Value::Nil);
                                    return Ok(Location::ListIndex(list, i));
                                }
                                Value::Tuple(tuple) => {
                                    let i = self.heap[tuple].elements.len();
                                    self.heap[tuple].elements.push(Value::Nil);
                                    return Ok(Location::TupleIndex(tuple, i));
                                }
                                _ => panic!("cannot push into {}", container.ty()),
                            }
                        }

                        let index = index.unwrap();

                        // let matching_dict_key = match container {
                        //     Value::Dict(dict) => self.heap[dict].get(&self, index).clone(),
                        //     _ => None,
                        // };

                        let listy_items = match container {
                            Value::List(list) => Some(self.heap[list].elements.clone()),
                            Value::Tuple(tuple) => Some(self.heap[tuple].elements.clone()),
                            _ => None,
                        };

                        // EXTEND IF NECESSARY
                        let mut listy_index = None;
                        let mut resized_items = None;
                        if let Some(items) = listy_items {
                            let Value::Numeric(Numeric::Int(index)) = index else {
                                return RuntimeError(format!(
                                    "list/tuple assign index must be an int, is a: {}",
                                    index.ty()
                                ))
                                .into();
                            };

                            let i = if index >= 0 {
                                index as usize
                            } else if items.len() as i64 + index >= 0 {
                                (items.len() as i64 + index) as usize
                            } else {
                                return RuntimeError(format!("negative index out of bounds"))
                                    .into();
                            };

                            listy_index = Some(i);

                            if i >= items.len() {
                                let mut new_items = vec![];
                                for _ in items.len()..(i + 1) {
                                    new_items.push(Value::Nil);
                                }
                                resized_items = Some(new_items);
                            }
                        }

                        let index_hash = self.hash(&index);

                        match container.clone() {
                            Value::List(list) => {
                                let i = listy_index.unwrap();

                                if let Some(new_items) = resized_items {
                                    self.heap[list].elements.extend(new_items);
                                }

                                Ok(Location::ListIndex(list, i))
                            }
                            Value::Tuple(tuple) => {
                                let i = listy_index.unwrap();

                                if let Some(new_items) = resized_items {
                                    self.heap[tuple].elements.extend(new_items);
                                }

                                Ok(Location::TupleIndex(tuple, i))
                            }
                            Value::Dict(dict) => {
                                Ok(Location::DictIndex(dict, index))
                                // if let Some((key, _)) = matching_dict_key {
                                //     Ok(Location::DictIndex(dict, index))
                                // } else {
                                //     dict.insert_known_hash(
                                //         index_hash.expect("known index hash"),
                                //         index,
                                //         nil,
                                //     );
                                //     Ok(Location::Single(nil))
                                // }
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
                // let mut assignable_elements = Vec::with_capacity(elements.len());
                // for pattern in elements {
                //     assignable_elements.push(self.resolve(scope, pattern)?);
                // }
                // Ok(Location::Tuple(assignable_elements))
                todo!()
            }
        }
    }

    // pub fn debug_scope(&self, scope: usize) -> String {
    //     if scope == 0 {
    //         "RootScope".into()
    //     } else {
    //         format!(
    //             "Scope({}){}",
    //             self.get_scope(scope)
    //                 .values
    //                 .iter()
    //                 .map(|(key, k)| { format!("{key}: {}", self.get_value(*k).ty()) })
    //                 .collect::<Vec<_>>()
    //                 .join(", "),
    //             match self.get_scope(scope).parent_scope {
    //                 None => "".into(),
    //                 Some(parent_id) => format!(" <- {}", self.debug_scope(parent_id)),
    //             }
    //         )
    //     }
    // }

    /// (location of the value, whether it was copied or newly computed already during evaluation)
    pub fn evaluate(&mut self, scope: Gc<Scope>, expr: &Expr) -> EvaluationResult<Value> {
        match expr {
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::NilLiteral => Ok(Value::Nil),
            Expr::DictLiteral { elements } => {
                let mut dict = Dict::new();

                for (key, value_expr) in elements {
                    let key = match key {
                        Either::Left(name) => Value::Str(name.0.to_string().into()),
                        Either::Right(key_expr) => self.evaluate(scope, key_expr)?,
                    };

                    let val = self.evaluate(scope, value_expr)?;

                    dict.insert(&self, key, val);
                }

                Ok(Value::Dict(self.heap.alloc(dict).unrooted()))
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
                            build += &format!("{}", self.display(&value, true));
                            // build += &value.auto_coerce_str();
                        }
                    }
                }

                Ok(Value::Str(Substr::from(build)))
            }
            Expr::Numeric(num) => Ok(Value::Numeric(num.clone())),
            Expr::RegexLiteral { regex } => Ok(Value::Regex(regex.clone())),
            Expr::Variable(id) => {
                let (_, val) = self.lookup(scope, id)?;

                Ok(val)
            }
            Expr::UnaryExpr { expr, op } => {
                let value = self.evaluate(scope, expr)?;
                match op.as_str() {
                    "!" => Ok(value.negate()?),
                    _ => RuntimeError(format!("Unknown unary operation: {op}")).into(),
                }
            }
            Expr::BinaryExpr { left, op, right } => {
                let le = self.evaluate(scope, left)?;

                // short-circuiting ops
                match op.as_str() {
                    "&&" => {
                        if !le.truthy() {
                            return Ok(le);
                        } else {
                            return Ok(self.evaluate(scope, right)?);
                        }
                    }
                    "||" => {
                        if le.truthy() {
                            return Ok(le);
                        } else {
                            return Ok(self.evaluate(scope, right)?);
                        }
                    }
                    "??" => {
                        if le != Value::Nil {
                            return Ok(le);
                        } else {
                            return Ok(self.evaluate(scope, right)?);
                        }
                    }
                    _ => {}
                }

                let ri = self.evaluate(scope, right)?;

                match op.as_str() {
                    "+" => {
                        let (_, add) = self.lookup(scope, &Identifier("add".into()))?;
                        return Ok(self.invoke(&add, vec![(None, le), (None, ri)])?);
                    }
                    _ => {}
                }

                match op.as_str() {
                    "^" => return Ok(le.pow(&ri)?),
                    "<<" => return Ok(le.left_shift(&ri)?),
                    // "+" => return Ok(le.add(ri)?),
                    "-" => return Ok(le.sub(&ri)?),
                    "*" => return Ok(le.mul(&ri)?),
                    "/" => return Ok(le.div(&ri)?),
                    "%" => return Ok(le.modulo(&ri)?),
                    _ => {}
                };

                let ord = self.cmp(&le, &ri);

                match op.as_str() {
                    "<" => Ok(Value::Bool(ord == Ordering::Less)),
                    ">" => Ok(Value::Bool(ord == Ordering::Greater)),
                    "==" => Ok(Value::Bool(ord == Ordering::Equal)),
                    "!=" => Ok(Value::Bool(ord != Ordering::Equal)),
                    ">=" => Ok(Value::Bool(
                        ord == Ordering::Equal || ord == Ordering::Greater,
                    )),
                    "<=" => Ok(Value::Bool(ord == Ordering::Equal || ord == Ordering::Less)),
                    _ => RuntimeError(format!("Unknown binary operation: {op}")).into(),
                }
            }
            Expr::ListLiteral { elements, splat } => {
                let mut ty = Type::Any;
                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    // if let Some(narrowed) = ty.narrow(&self.get_value(expr_value.0).ty()) {
                    //     ty = narrowed;
                    // } else {
                    //     return RuntimeError("list contains distinct types".into()).into();
                    // }
                    element_values.push(expr_value);
                }

                if let Some(splat) = splat {
                    let splat_value = self.evaluate(scope, splat)?;
                    match splat_value.clone() {
                        Value::List(list) => {
                            for el in &self.heap[list].elements {
                                element_values.push(el.clone());
                            }
                        }
                        Value::Nil => {
                            // that's OK
                        }
                        _ => {
                            return RuntimeError(format!("cannot splat: {}", splat_value.ty()))
                                .into()
                        }
                    }
                }

                let list = self
                    .heap
                    .alloc(List {
                        ty,
                        elements: element_values,
                    })
                    .unrooted();

                Ok(Value::List(list))
            }
            Expr::TupleLiteral { elements } => {
                let ts = None;

                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    element_values.push(expr_value);
                }

                let tuple = self
                    .heap
                    .alloc(Tuple {
                        ty: ts,
                        elements: element_values,
                    })
                    .unrooted();

                Ok(Value::Tuple(tuple))
            }
            Expr::Index {
                expr,
                coalesce,
                index,
            } => {
                return self.evaluate_indexed(scope, expr, *coalesce, index);
            }
            Expr::Invocation {
                expr,
                postfix,
                coalesce,
                args,
            } => {
                let fn_expr_value = self.evaluate(scope, expr)?;

                if *coalesce && !postfix && fn_expr_value == Value::Nil {
                    return Ok(fn_expr_value);
                }

                let mut evaluated_args = vec![];
                for (i, arg) in args.into_iter().enumerate() {
                    let arg_value = self.evaluate(scope, &arg.expr)?;

                    if i == 0 && *coalesce && *postfix && arg_value == Value::Nil {
                        return Ok(arg_value);
                    }

                    evaluated_args.push((arg.name.clone(), arg_value));
                }

                Ok(self.invoke(&fn_expr_value, evaluated_args)?)
            }
            Expr::AnonymousFn { params, body } => {
                return Ok(Value::FnDef(FnDef {
                    name: None,
                    parent_scope: scope,
                    signatures: vec![FnSig {
                        params: params.clone(),
                        body: FnBody::Code(body.clone()), // TODO somehow avoid clone
                        result: Type::Any,
                    }],
                }));
            }
            Expr::If {
                pattern,
                cond,
                then,
                els,
            } => {
                let cond_value = self.evaluate(scope, cond)?;

                let execution_scope = self.create_subscope(scope);

                let ok = match pattern {
                    None => cond_value.truthy(),
                    Some(pattern) => {
                        self.declare(execution_scope, pattern, cond_value)? == Some(true)
                    }
                };

                let result = if ok {
                    self.execute_block(execution_scope, then)?
                } else if let Some(els) = els {
                    self.execute_block(execution_scope, els)?
                } else {
                    Value::Nil
                };

                Ok(result)
            }
            Expr::While {
                label,
                pattern,
                cond,
                body,
            } => {
                let mut result = Value::Nil;

                loop {
                    let cond_value = self.evaluate(scope, cond)?;

                    let execution_scope = self.create_subscope(scope);

                    let ok = match pattern {
                        None => cond_value.truthy(),
                        Some(pattern) => {
                            self.declare(execution_scope, pattern, cond_value)? == Some(true)
                        }
                    };

                    if !ok {
                        return Ok(result);
                    }

                    match self.execute_block(execution_scope, body) {
                        Ok(res) => {
                            result = res;
                        }
                        Err(EvalOther::Break(res)) => {
                            result = res;
                            return Ok(result);
                        }
                        Err(EvalOther::Continue(continue_label))
                            if continue_label.is_none()
                                || continue_label.is_some() && continue_label == *label =>
                        {
                            continue;
                        }
                        Err(err) => return Err(err),
                    }
                }
            }
            Expr::DoWhile { label, cond, body } => loop {
                loop {
                    let result = match self.execute_block(scope, body) {
                        Ok(res) => res,
                        Err(EvalOther::Break(res)) => res,
                        Err(EvalOther::Continue(continue_label))
                            if continue_label.is_none()
                                || continue_label.is_some() && continue_label == *label =>
                        {
                            continue;
                        }
                        Err(err) => return Err(err),
                    };

                    if let Some(cond) = cond {
                        let cond_value = self.evaluate(scope, cond)?;
                        if !cond_value.truthy() {
                            return Ok(result);
                        }
                    } else {
                        return Ok(result);
                    }
                }
            },
            Expr::Loop { label, body } => Ok(loop {
                match self.execute_block(scope, body) {
                    Ok(_) => {}
                    Err(EvalOther::Break(value)) => break value,
                    Err(EvalOther::Continue(continue_label))
                        if continue_label.is_none()
                            || continue_label.is_some() && continue_label == *label =>
                    {
                        continue;
                    }
                    Err(other) => return Err(other),
                }
            }),
            Expr::For {
                label,
                pattern,
                range,
                body,
            } => {
                let k = self.evaluate(scope, range)?;
                let range_value = k.clone();

                let range = match range_value {
                    Value::List(list) => self.heap[list].elements.clone(),
                    Value::Tuple(tuple) => self.heap[tuple].elements.clone(),
                    _ => {
                        return RuntimeError(format!("cannot for-loop over {}", range_value.ty()))
                            .into();
                    }
                };

                let mut result = None;
                for item in range {
                    let execution_scope = self.create_subscope(scope);

                    self.declare(execution_scope, pattern, item)?;

                    match self.execute_block(execution_scope, body) {
                        Ok(_) => {}
                        Err(EvalOther::Break(res)) => {
                            result = Some(res);
                            break;
                        }
                        Err(EvalOther::Continue(continue_label))
                            if continue_label.is_none()
                                || continue_label.is_some() && continue_label == *label =>
                        {
                            continue;
                        }
                        Err(err) => return Err(err),
                    }
                }

                Ok(result.unwrap_or(Value::Nil))
            }
        }
    }

    pub fn evaluate_indexed(
        &mut self,
        scope: Gc<Scope>,
        expr: &Expr,
        coalesce: bool,
        index: &Expr,
    ) -> EvaluationResult<Value> {
        let collection = self.evaluate(scope, expr)?;

        match collection {
            Value::Nil if coalesce => Ok(Value::Nil),
            Value::Dict(dict) => {
                let key = self.evaluate(scope, index)?;
                Ok(self.heap[dict]
                    .get(&self, key)
                    .map(|(_, value)| value)
                    .unwrap_or(Value::Nil))
            }
            Value::List(list) => {
                let i = self.evaluate(scope, index)?;

                let Value::Numeric(i) = i else {
                    return RuntimeError(format!("index() i must be an int, is a: {}", i.ty()))
                        .into();
                };

                let i = i.get_int()?;

                let elements = &self.heap[list].elements;

                let el = (match i {
                    i if i >= 0 => elements.get(i as usize).cloned(),
                    i if elements.len() as i64 + i >= 0 => {
                        elements.get((elements.len() as i64 + i) as usize).cloned()
                    }
                    _ => None,
                })
                .map(|v| v)
                .unwrap_or(Value::Nil);

                Ok(el)
            }
            Value::Tuple(tuple) => {
                let i = self.evaluate(scope, index)?;

                let Value::Numeric(i) = i else {
                    return RuntimeError(format!("index() i must be an int, is a: {}", i.ty()))
                        .into();
                };

                let i = i.get_int()?;

                let elements = &self.heap[tuple].elements;

                let el = (match i {
                    i if i >= 0 => elements.get(i as usize).cloned(),
                    i if elements.len() as i64 + i >= 0 => {
                        elements.get((elements.len() as i64 + i) as usize).cloned()
                    }
                    _ => None,
                })
                .map(|v| v)
                .unwrap_or(Value::Nil);

                Ok(el)
            }
            Value::Str(text) => {
                let i = self.evaluate(scope, index)?;

                let Value::Numeric(i) = i else {
                    return RuntimeError(format!("index() i must be an int, is a: {}", i.ty()))
                        .into();
                };

                let i = i.get_int()?;

                let result = match i {
                    i if i >= 0 => {
                        let i = i as usize;
                        text.get(i..(i + 1))
                    }
                    i if text.len() as i64 + i >= 0 => {
                        let i = (text.len() as i64 + i) as usize;
                        text.get(i..(i + 1))
                    }
                    _ => None,
                }
                .map(|substr| Value::Str(text.substr_from(substr)))
                .unwrap_or(Value::Nil);

                Ok(result)
            }
            _ => RuntimeError(format!("cannot index into: {}", collection.ty())).into(),
        }
    }
}

impl RuntimeLike for Runtime {
    fn builtin(&mut self, name: &str, signatures: impl IntoIterator<Item = FnSig>) {
        let def = Value::FnDef(FnDef {
            name: Some(name.into()),
            parent_scope: self.root_scope.unrooted(),
            signatures: signatures.into_iter().collect(),
        });

        self.heap[self.root_scope.unrooted()]
            .values
            .insert(name.into(), def);
    }
}

pub fn execute_simple(code: &str) -> EvaluationResult<Ev> {
    let Some(doc) = parse_document(code) else {
        return RuntimeError("could not parse code".into()).into();
    };

    let mut runtime = Runtime::new();

    let value = runtime.execute_document(&doc, None)?;

    runtime.heap.gc();

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
    fn list_index_stack_overflow() {
        // stack overflow, why?!
        assert_eq!(execute_simple("let a = [2]; a[0]"), Ok(int(2)));
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

        assert_eq!(execute_simple("(1, 2)"), Ok(tuple([int(1), int(2)])));

        assert_eq!(execute_simple("let a = []; a []= (1, 2); 2"), Ok(int(2)));

        assert_eq!(
            execute_simple("let a = []; a []= (1, 2); let i = 0; a[i]"),
            Ok(tuple([int(1), int(2)]))
        );

        assert_eq!(
            execute_simple("let out = []; for let t in [(1,2), (3,4)] { out []= t }; out"),
            Ok(list([tuple([int(1), int(2)]), tuple([int(3), int(4)])]))
        );

        assert_eq!(execute_simple("let [a, b] = [1, 2]; a"), Ok(int(1)));

        assert_eq!(
            execute_simple("let a = 1; let b = 1; [a, b] = [2, 2]; a"),
            Ok(int(2))
        );

        assert_eq!(
            execute_simple("let arr = [3, 4]; [1, 2, ..arr]"),
            Ok(list([int(1), int(2), int(3), int(4)]))
        );

        assert_eq!(execute_simple("3 + 4"), Ok(int(7)));

        assert_eq!(execute_simple("[3] + [4]"), Ok(list([int(3), int(4)])));

        assert_eq!(
            execute_simple(
                r#"
                    let d = @{}
                    d["bla"] = 2
                    d["bla"]
                "#
            ),
            Ok(int(2))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let res = []
                    for let i in range(0, 6) {
                        if i == 4 {
                            continue
                        }

                        res []= i
                    }
                    res
                "#
            ),
            Ok(list([int(0), int(1), int(2), int(3), int(5)]))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let res = []
                    'outer: for let y in [0, 10] {
                        for let i in range(0, 6) {
                            if i == 4 {
                                continue 'outer
                            }

                            res []= y + i
                        }
                    }
                    res
                "#
            ),
            Ok(list([
                int(0),
                int(1),
                int(2),
                int(3),
                int(10),
                int(11),
                int(12),
                int(13)
            ]))
        );

        assert_eq!(
            execute_simple(
                r#"
                    fn greet(name = "kelley") {
                        "hello {name}"
                    }

                    greet(nil)
                "#
            ),
            Ok(str("hello kelley"))
        );

        assert_eq!(
            execute_simple(
                r#"
                    fn greet(name = "kelley") {
                        "hello {name}"
                    }

                    greet()
                "#
            ),
            Ok(str("hello kelley"))
        );

        assert_eq!(
            execute_simple(
                r#"
                    any([1, 2, 3])
                "#
            ),
            Ok(bool(true))
        );

        assert_eq!(
            execute_simple(
                r#"
                    [1, 2, 3] :any |n| { n > 1 }
                "#
            ),
            Ok(bool(true))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let [a = 5, b = 6, ..rest] = [1]
                    (a, b, rest)
                "#
            ),
            Ok(tuple([int(1), int(6), list([])]))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let [a = 5, b = 6, ..rest] = [1, nil]
                    (a, b, rest)
                "#
            ),
            Ok(tuple([int(1), int(6), list([])]))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let [a = 5, b = 6, ..rest] = [1, false]
                    (a, b, rest)
                "#
            ),
            Ok(tuple([int(1), bool(false), list([])]))
        );

        assert_eq!(
            execute_simple(
                r#"
                    let [a = 5, b = 6, ..rest] = [1, false, "a", "b"]
                    (a, b, rest)
                "#
            ),
            Ok(tuple([int(1), bool(false), list([str("a"), str("b")])]))
        );
    }

    #[test]
    fn lists() {
        assert_eq!(
            execute_simple(
                r#"
                    let list = []
                    list[4] = 2
                    list
                "#
            ),
            Ok(list([Ev::Nil, Ev::Nil, Ev::Nil, Ev::Nil, int(2)]))
        );
    }

    #[test]
    fn coalesce() {
        assert_eq!(execute_simple(r#""5":int"#), Ok(int(5)));

        assert_eq!(
            execute_simple(r#"nil :int"#),
            RuntimeError("cannot coerce nil to int".into()).into()
        );

        assert_eq!(execute_simple(r#"nil ?:int"#), Ok(Ev::Nil));
    }

    #[test]
    fn if_let() {
        assert_eq!(
            execute_simple(
                r#"
                    if let i = nil {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(1))
        );

        assert_eq!(
            execute_simple(
                r#"
                    if let some i = nil {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(2))
        );

        assert_eq!(
            execute_simple(
                r#"
                    if let i = 0 {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(1))
        );

        assert_eq!(
            execute_simple(
                r#"
                    if let [a, b] = 2 {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(2))
        );

        assert_eq!(
            execute_simple(
                r#"
                    if let [a, b] = [] {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(1))
        );

        assert_eq!(
            execute_simple(
                r#"
                    if let [a, [b, c]] = [] {
                        1
                    } else {
                        2
                    }
                "#
            ),
            Ok(int(2))
        );
    }
}
