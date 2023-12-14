use std::{
    cmp::Ordering,
    collections::{hash_map::RandomState, HashMap},
    fmt::{self, Display, Formatter},
    hash::{BuildHasher, Hash, Hasher},
};

use arcstr::Substr;
use either::Either;
use try_map::FallibleMapExt;

use crate::{
    ast::{
        AssignPattern, Block, Declarable, DeclarePattern, Document, Expr, Identifier, Item, Stmt,
        StrLiteralPiece, Type,
    },
    fmt::Fmt,
    parse::parse_document,
    stdlib::implement_stdlib,
    types::param_permits_arg,
    value::{AlRegex, EvalOther, EvaluationResult, Numeric, RuntimeError},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dict(HashMap<u64, Vec<(usize, usize)>>);

impl std::hash::Hash for Dict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!("todo Dict::hash")
    }
}

impl Dict {
    pub fn new() -> Dict {
        Self(HashMap::new())
    }

    pub fn entries<'a>(&'a self) -> impl Iterator<Item = (usize, usize)> + 'a {
        self.0
            .iter()
            .flat_map(|(_, bucket_entries)| bucket_entries.iter())
            .map(|(k, v)| (*k, *v))
    }

    pub fn get(&self, runtime: &Runtime, k: usize) -> Option<(usize, usize)> {
        if let Some(bucket) = self.0.get(&runtime.hash(k)) {
            for &(q, v) in bucket {
                if runtime.eq(k, q) {
                    return Some((k, v));
                }
            }
        }

        None
    }

    pub fn insert(&mut self, runtime: &Runtime, k: usize, v: usize) {
        let key_hash = runtime.hash(k);
        self.0.entry(key_hash).or_default().push((k, v));
    }

    pub fn insert_known_hash(&mut self, key_hash: u64, k: usize, v: usize) {
        self.0.entry(key_hash).or_default().push((k, v));
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
    pub params: Vec<Declarable>,
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
        write!(f, ") <body>")?;
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
    Builtin(fn(&mut Runtime, usize) -> EvaluationResult<(usize, bool)>),
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
    Tuple(Option<Vec<Type>>, Vec<usize>),
    Dict(Option<(Type, Type)>, Dict),
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
            Value::Tuple(_, list) => {
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
            Value::Dict(_, dict) => {
                write!(f, "@{{")?;
                let pairs = dict.entries().collect::<Vec<_>>();
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
            Value::Tuple(_, _) => Ok(Value::Bool(false)),
            Value::List(_, _) => Ok(Value::Bool(false)),
            Value::Dict(_, _) => Ok(Value::Bool(false)),
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
            Value::Tuple(_, _) => "<tuple>".into(),
            Value::Dict(_, _) => "<dict>".into(),
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
            Value::Tuple(ts, _) => Type::Tuple(ts.clone()),
            Value::Dict(_, _) => Type::Dict(None),
        }
    }

    pub fn truthy(&self) -> EvaluationResult<bool> {
        match self {
            Value::Nil => Ok(false),
            Value::Bool(b) => Ok(*b),
            Value::List(_, _) => Ok(true),
            Value::Tuple(_, _) => Ok(true),
            Value::Dict(_, _) => Ok(true),
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
    random_state: RandomState,
    scopes: Vec<Scope>,
    heap: Vec<Option<Value>>,
    reclaimed: Vec<usize>,
}

impl Runtime {
    pub fn new() -> Runtime {
        let mut runtime = Runtime {
            random_state: RandomState::new(),
            scopes: vec![Scope::root()],
            heap: vec![],
            reclaimed: vec![],
        };

        implement_stdlib(&mut runtime);

        runtime
    }

    pub fn execute_document(&mut self, doc: &Document, stdin: String) -> EvaluationResult<Ev> {
        let stdin_loc = self.new_value(Value::Str(Substr::from(stdin))).0;
        self.get_scope_mut(0).values.insert(id("stdin"), stdin_loc);

        let res_loc = self.execute_block(0, &doc.body)?.0;

        let value = match self.get_value_ext(res_loc) {
            Err(e) => {
                return RuntimeError(format!("could not externalize runtime result: {}", e)).into();
            }
            Ok(value) => value,
        };

        Ok(value)
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
            Value::Tuple(_, items) => Ok(Ev::Tuple(
                items
                    .into_iter()
                    .map(|loc| self.get_value_ext(*loc))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Value::Dict(_, dict) => {
                let pairs = dict
                    .entries()
                    .map(|(key_loc, value_loc)| {
                        Ok((self.get_value_ext(key_loc)?, self.get_value_ext(value_loc)?))
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
            Value::Tuple(ts, items) => {
                let cloned_items = items.into_iter().map(|item| self.clone(item).0).collect();
                self.new_value(Value::Tuple(ts, cloned_items))
            }
            other => self.new_value(other),
        }
    }

    pub fn hash(&self, v: usize) -> u64 {
        let mut h = self.random_state.build_hasher();
        self.hash_do(&mut h, v);
        let hash = h.finish();
        // println!("runtime hash {} => {}", self.display(v, false), hash);
        hash
    }

    fn hash_do<H: Hasher>(&self, h: &mut H, v: usize) {
        match self.get_value(v) {
            Value::Nil => ().hash(h),
            Value::Bool(b) => b.hash(h),
            Value::Str(s) => s.hash(h),
            Value::Numeric(n) => n.hash(h),
            Value::Regex(a) => a.hash(h),
            Value::Tuple(_, els) => {
                for &el in els {
                    self.hash_do(h, el);
                }
            }
            Value::List(_, els) => {
                for &el in els {
                    self.hash_do(h, el);
                }
            }
            Value::Dict(_, dict) => {
                for (k, v) in dict.entries() {
                    self.hash_do(h, k);
                    self.hash_do(h, v);
                }
            }
            Value::FnDef(def) => {
                panic!("hash fndefs")
            }
        }
    }

    pub fn cmp(&self, a: usize, b: usize) -> Ordering {
        match (self.get_value(a), self.get_value(b)) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Str(a), Value::Str(b)) => a.cmp(b),
            (Value::Numeric(a), Value::Numeric(b)) => a.cmp(b),
            (Value::Regex(a), Value::Regex(b)) => a.0.as_str().cmp(b.0.as_str()),
            (Value::Tuple(_, a), Value::Tuple(_, b)) => {
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
            (Value::Dict(_, a), Value::Dict(_, b)) => {
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
                "cannot compare values of disparate types: {} (is a {}) v {} (is a {})",
                a,
                a.ty(),
                b,
                b.ty()
            ),
        }
    }

    pub fn eq(&self, a: usize, b: usize) -> bool {
        self.cmp(a, b) == Ordering::Equal
    }

    fn display_fmt(&self, f: &mut Formatter, loc: usize, toplevel: bool) -> fmt::Result {
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
            Value::Tuple(_, list) => {
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
            Value::Dict(_, dict) => {
                write!(f, "@{{")?;
                let mut i = 0;
                for (key, value) in dict.entries() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, ".")?;
                    self.display_fmt(f, key, true)?;
                    write!(f, " ")?;
                    self.display_fmt(f, value, false)?;
                    i += 1;
                }
                if i == 1 {
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
            Value::Tuple(_, els) => {
                for &el in els {
                    self.gc_reach(el, reachable);
                }
            }
            Value::Dict(_, dict) => {
                for (k, v) in dict.entries() {
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

    pub fn assign(&mut self, location: Location, value: usize) -> EvaluationResult<()> {
        match location {
            Location::Single(loc) => {
                self.heap[loc] = self.heap[value].clone();
                Ok(())
            }
            Location::List(elements) => {
                let value = self.get_value(value).clone();
                let Value::List(_, items) = value else {
                    return RuntimeError(format!("cannot assign to list pattern: {}", value.ty()))
                        .into();
                };

                for (location, value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    self.assign(location, value)?;
                }

                Ok(())
            }
            Location::Tuple(elements) => {
                let value = self.get_value(value).clone();
                let Value::Tuple(_, items) = value else {
                    return RuntimeError(format!("cannot assign to tuple pattern: {}", value.ty()))
                        .into();
                };

                for (location, value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    self.assign(location, value)?;
                }

                Ok(())
            }
        }
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
                    return RuntimeError(format!("cannot declare to list pattern: {}", value.ty()))
                        .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<usize>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len().min(items.len())),
                    ))
                })?;

                let items = items.clone();

                for (Declarable { pattern, fallback }, mut value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    if let Some(fallback_expr) = fallback
                        && self.get_value(value) == &Value::Nil {
                        value = self.evaluate(scope, fallback_expr)?.0;
                    }

                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    let (list, _) = self.new_value(Value::List(item_type.clone(), items));
                    self.declare(scope, &DeclarePattern::Id(id, t), list)?;
                }
            }
            DeclarePattern::Tuple { elements, rest } => {
                let value = self.get_value(value).clone();
                let Value::Tuple(ts, mut items) = value else {
                    return RuntimeError(format!(
                        "cannot declare to tiple pattern: {}",
                        value.ty()
                    ))
                    .into();
                };

                let assign_rest_later = rest.clone().try_map(|(id, t)| {
                    Ok::<(Identifier, Option<Type>, Vec<usize>), EvalOther>((
                        id,
                        t,
                        items.split_off(elements.len().min(items.len())),
                    ))
                })?;

                for (Declarable { pattern, fallback }, mut value) in elements.into_iter().zip(
                    items
                        .into_iter()
                        .chain(std::iter::repeat(self.new_value(Value::Nil).0)),
                ) {
                    if let Some(fallback_expr) = fallback
                        && self.get_value(value) == &Value::Nil {
                        value = self.evaluate(scope, fallback_expr)?.0;
                    }

                    self.declare(scope, pattern, value)?;
                }

                if let Some((id, t, items)) = assign_rest_later {
                    let (tuple, _) = self.new_value(Value::Tuple(ts, items));
                    self.declare(scope, &DeclarePattern::Id(id, t), tuple)?;
                }
            }
        }

        Ok(())
    }

    pub fn execute(&mut self, scope: usize, stmt: &Stmt) -> EvaluationResult<(usize, bool)> {
        match stmt {
            Stmt::Continue { label } => Err(EvalOther::Continue(label.clone())),
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

                self.assign(assignable, value)?;

                Ok(self.new_value(Value::Nil))
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
                        if let Some(i) = sig.params.iter().position(|p| match p {
                            Declarable {
                                pattern: DeclarePattern::Id(n, _),
                                ..
                            } => n == name,
                            _ => false,
                        }) {
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
            unimplemented!(
                "todo select best fn signature for: {}, arg types: ({}), possible signatures: {}",
                name.map(|n| n.0).unwrap_or("<unknown>".into()),
                args.iter()
                    .map(|(_, ty)| format!("{}", ty))
                    .collect::<Vec<_>>()
                    .join(", "),
                matches
                    .iter()
                    .enumerate()
                    .map(|(i, (sig, _))| format!("#{} {}:", i + 1, sig))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
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

        // todo find a way to not have to clone this
        let FnDef {
            name,
            parent_scope,
            signatures,
        } = def.clone();

        let Some((FnSig { params, body }, assignments)) = self.select_signature(
            name.clone(),
            signatures,
            args.iter()
                .map(|(id, v)| (id, self.get_ty(*v)))
                .collect::<Vec<_>>(),
        ) else {
            return RuntimeError(format!(
                "could not find matching signature, fn: {}, arg types: ({})",
                name.map(|n| n.0).unwrap_or("<unknown>".into()),
                args.iter()
                    .map(|arg| format!("{}", self.get_ty(arg.1)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .into();
        };

        let execution_scope = self.new_scope(parent_scope);

        for (param_index, assign_arg) in assignments.into_iter().enumerate() {
            if let Some(arg_index) = assign_arg {
                let mut arg_val = args[arg_index].1;
                if let Some(fallback_expr) = &params[param_index].fallback
                    && self.get_value(arg_val) == &Value::Nil {
                    arg_val = self.evaluate(parent_scope, &fallback_expr)?.0;
                }
                self.declare(execution_scope, &params[param_index].pattern, arg_val)?;
            } else {
                let fallback_expr = &params[param_index]
                    .fallback
                    .as_ref()
                    .expect("already checked that this param has a fallback");

                let arg_val = self.evaluate(parent_scope, &fallback_expr)?.0;
                self.declare(execution_scope, &params[param_index].pattern, arg_val)?;
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
                            .map(|(v, _)| v);

                        let index_val = index_loc.map(|i| self.get_value(i)).cloned();

                        let nil = self.new_value(Value::Nil).0;

                        let matching_dict_key = match (self.get_value(parent), index_loc) {
                            (Value::Dict(_, dict), Some(i)) => {
                                dict.get(&self, i).map(|(key, _)| key)
                            }
                            _ => None,
                        };

                        let array_pushed_loc = match (self.get_value_mut(parent), index_loc) {
                            (Value::List(_, els), None) => {
                                els.push(nil);
                                Some(nil)
                            }
                            (Value::Tuple(_, els), None) => {
                                els.push(nil);
                                Some(nil)
                            }
                            _ => None,
                        };

                        let index_value_hash = index_loc.map(|i| self.hash(i));

                        match self.get_value_mut(parent) {
                            Value::List(_, items) | Value::Tuple(_, items) => {
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
                            Value::Dict(_, ref mut dict) => {
                                if let Some(index_loc) = index_loc {
                                    if let Some(loc) = matching_dict_key {
                                        Ok(Location::Single(loc))
                                    } else {
                                        dict.insert_known_hash(
                                            index_value_hash.expect("known index hash"),
                                            index_loc,
                                            nil,
                                        );
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

                    dict.insert(&self, k, expr_value);
                }

                Ok(self.new_value(Value::Dict(None, dict)))
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
                    "??" => {
                        if self.get_value(le.0) != &Value::Nil {
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
                        return Ok(self.invoke(add, vec![(None, le.0), (None, ri.0)])?);
                    }
                    _ => {}
                }

                let left_value = self.get_value(le.0);
                let right_value = self.get_value(ri.0);

                match op.as_str() {
                    "^" => return Ok(self.new_value(left_value.pow(right_value)?)),
                    "<<" => return Ok(self.new_value(left_value.left_shift(right_value)?)),
                    // "+" => return Ok(self.new_value(left_value.add(right_value)?)),
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
                    element_values.push(expr_value.0);
                }

                if let Some(splat) = splat {
                    let splat_value = self.evaluate(scope, splat)?;
                    match self.get_value(splat_value.0).clone() {
                        Value::List(_, copy_els) => {
                            for el in copy_els {
                                element_values.push(self.ensure_new((el, splat_value.1)));
                            }
                        }
                        Value::Nil => {
                            // that's OK
                        }
                        _ => {
                            return RuntimeError(format!(
                                "cannot splat: {}",
                                self.get_ty(splat_value.0)
                            ))
                            .into()
                        }
                    }
                }

                Ok(self.new_value(Value::List(ty, element_values)))
            }
            Expr::TupleLiteral { elements } => {
                let ts = None;

                let mut element_values = vec![];
                for expr in elements {
                    let expr_value = self.evaluate(scope, expr)?;
                    element_values.push(expr_value.0);
                }

                Ok(self.new_value(Value::Tuple(ts, element_values)))
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
            Expr::While { label, cond, body } => {
                let mut result = self.new_value(Value::Nil);
                loop {
                    let cond_value = self.evaluate(scope, cond)?;
                    if !self.get_value(cond_value.0).truthy()? {
                        return Ok(result);
                    }

                    match self.execute_block(scope, body) {
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
                        if !self.get_value(cond_value.0).truthy()? {
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
                let range_value = self.get_value(k.0).clone();

                let range = match range_value {
                    Value::List(_, values) => values,
                    Value::Tuple(_, values) => values,
                    _ => {
                        return RuntimeError(format!("cannot for-loop over {}", range_value.ty()))
                            .into();
                    }
                };

                let mut result = None;
                for item in range {
                    let execution_scope = self.new_scope(scope);

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

                Ok(result.unwrap_or_else(|| self.new_value(Value::Nil)))
            }
        }
    }
}

pub fn execute_simple(code: &str) -> EvaluationResult<Ev> {
    let Some(doc) = parse_document(code) else {
        return RuntimeError("could not parse code".into()).into();
    };

    let mut runtime = Runtime::new();

    let value = runtime.execute_document(&doc, "".into())?;

    // runtime.gc([res_loc]);

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
}
