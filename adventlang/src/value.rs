use std::{cmp::Ordering, fmt::Display};

use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeError(pub String);

pub type EvaluationResult<T> = Result<T, EvalOther>;

#[derive(Debug, PartialEq)]
pub enum EvalOther {
    RuntimeError(RuntimeError),
    Break((usize, bool)),
    Return((usize, bool)),
}

impl From<RuntimeError> for EvalOther {
    fn from(err: RuntimeError) -> Self {
        EvalOther::RuntimeError(err)
    }
}

impl<T> From<RuntimeError> for Result<T, EvalOther> {
    fn from(err: RuntimeError) -> Self {
        Err(EvalOther::RuntimeError(err))
    }
}

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
    pub fn eq(&self, other: &Numeric) -> bool {
        match (self, other) {
            (Numeric::Double(a), Numeric::Double(b)) => a == b,
            (Numeric::Int(a), Numeric::Int(b)) => a == b,
            (a, Numeric::Double(b)) => a.get_double() == *b,
            (Numeric::Double(a), b) => *a == b.get_double(),
        }
    }

    pub fn negate(&self) -> Result<Numeric, RuntimeError> {
        match self {
            Numeric::Int(n) => Ok(Numeric::Int(-n)),
            Numeric::Double(n) => Ok(Numeric::Double(-n)),
        }
    }

    pub fn pow(&self, other: &Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a.powf(b.get_double())),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double().powf(*b)),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a.pow(*b as u32)),
        }
    }

    pub fn add(&self, other: &Numeric) -> Numeric {
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

    pub fn mul(&self, other: &Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a * b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() * b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a * b),
        }
    }

    pub fn div(&self, other: &Numeric) -> Numeric {
        match (self, other) {
            (Numeric::Double(a), b) => Numeric::Double(a / b.get_double()),
            (a, Numeric::Double(b)) => Numeric::Double(a.get_double() / b),

            (Numeric::Int(a), Numeric::Int(b)) => Numeric::Int(a / b),
        }
    }

    pub fn modulo(&self, other: &Numeric) -> Numeric {
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

    pub fn get_int(&self) -> EvaluationResult<i64> {
        match self {
            Numeric::Int(a) => Ok(*a as i64),
            Numeric::Double(_) => {
                RuntimeError("value is a double, cannot be converted to an int".to_string()).into()
            }
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
