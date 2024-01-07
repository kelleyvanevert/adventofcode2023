use std::fmt;
use std::marker::PhantomData;

use fxhash::FxHashMap;
use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, SerializeTuple, Serializer};

use crate::runtime::Ev;

impl Serialize for Ev {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Ev::Nil => serializer.serialize_unit(),
            Ev::Bool(b) => serializer.serialize_bool(*b),
            Ev::Int(n) => serializer.serialize_i64(*n),
            Ev::Double(d) => serializer.serialize_f64(*d),
            Ev::Str(s) => serializer.serialize_str(s),
            Ev::Regex => serializer.serialize_str("<regex>"),
            Ev::FnDef => serializer.serialize_str("<fn>"),
            Ev::List(elements) => {
                let mut seq = serializer.serialize_seq(Some(elements.len()))?;
                for el in elements {
                    seq.serialize_element(el)?;
                }
                seq.end()
            }
            Ev::Tuple(elements) => {
                let mut tup = serializer.serialize_tuple(elements.len())?;
                for el in elements {
                    tup.serialize_element(el)?;
                }
                tup.end()
            }
            Ev::Dict(dict) => {
                let mut map = serializer.serialize_map(Some(dict.len()))?;
                for (key, value) in dict {
                    map.serialize_entry(key, value)?;
                }
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for Ev {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(EvVisitor::new())
    }
}

struct EvVisitor {
    marker: PhantomData<fn() -> Ev>,
}

impl EvVisitor {
    fn new() -> Self {
        EvVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for EvVisitor {
    type Value = Ev;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("some values")
    }

    fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Bool(b))
    }

    fn visit_f64<E>(self, d: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Double(d))
    }

    fn visit_i64<E>(self, n: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Int(n))
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Str(s.to_string()))
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Nil)
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Ev::Nil)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(seq.size_hint().unwrap_or(0));

        while let Some(element) = seq.next_element()? {
            values.push(element);
        }

        Ok(Ev::List(values))
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut map = FxHashMap::default();

        while let Some((key, value)) = access.next_entry()? {
            map.insert(key, value);
        }

        Ok(Ev::Dict(map))
    }
}
