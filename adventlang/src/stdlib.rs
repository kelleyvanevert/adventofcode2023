use crate::{
    ast::{Identifier, Pattern, Type},
    runtime::{FnBody, FnSig, Numeric, Runtime, RuntimeError, Value},
};

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

fn idpat(id: &str) -> Pattern {
    Pattern::Id(Identifier(id.into()), None)
}

fn idpat_ty(id: &str, ty: Type) -> Pattern {
    Pattern::Id(Identifier(id.into()), Some(ty))
}

pub fn implement_stdlib(runtime: &mut Runtime) {
    runtime.builtin(
        "print",
        [FnSig {
            params: vec![idpat("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                println!("{}", text);
                Ok(Value::Nil)
            }),
        }],
    );

    runtime.builtin(
        "run",
        [FnSig {
            params: vec![idpat("f")],
            body: FnBody::Builtin(|runtime, scope| {
                let f = runtime.scopes[scope].values.get(&id("f")).unwrap();

                match f {
                    Value::FnDef(def) => runtime.invoke(def.clone(), vec![]),
                    _ => Err(RuntimeError(format!("cannot run: {}", f.ty()))),
                }
            }),
        }],
    );

    runtime.builtin(
        "min",
        [
            FnSig {
                params: vec![idpat_ty("items", Type::List(Type::Any.into()))],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                    match items {
                        Value::List(_, list) => {
                            let mut result = Value::Nil;
                            for item in list.iter() {
                                result = result.min(item)?;
                            }

                            Ok(result)
                        }
                        _ => Err(RuntimeError(format!("cannot get min of: {}", items.ty()))),
                    }
                }),
            },
            FnSig {
                params: vec![idpat_ty("a", Type::Numeric), idpat_ty("b", Type::Numeric)],
                body: FnBody::Builtin(|runtime, scope| {
                    let a = runtime.scopes[scope].values.get(&id("a")).unwrap();
                    let b = runtime.scopes[scope].values.get(&id("b")).unwrap();

                    Ok(a.min(b)?)
                }),
            },
        ],
    );

    runtime.builtin(
        "max",
        [
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
    );

    runtime.builtin(
        "map",
        [FnSig {
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
    );

    runtime.builtin(
        "flat_map",
        [FnSig {
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
    );

    runtime.builtin(
        "in",
        [FnSig {
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
    );

    runtime.builtin(
        "filter",
        [FnSig {
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
    );

    runtime.builtin(
        "filter_map",
        [FnSig {
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
    );

    runtime.builtin(
        "find_map",
        [FnSig {
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
    );

    runtime.builtin(
        "find",
        [FnSig {
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
    );

    runtime.builtin(
        "range",
        [FnSig {
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
    );

    runtime.builtin(
        "enumerate",
        [FnSig {
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
    );

    runtime.builtin(
        "sum",
        [FnSig {
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
    );

    runtime.builtin(
        "split",
        [FnSig {
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
    );

    runtime.builtin(
        "lines",
        [FnSig {
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
    );

    runtime.builtin(
        "match",
        [FnSig {
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
    );

    runtime.builtin(
        "match_all",
        [FnSig {
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
    );

    runtime.builtin(
        "starts_with",
        [FnSig {
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
    );

    runtime.builtin(
        "replace",
        [FnSig {
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
    );

    runtime.builtin(
        "slice",
        [
            FnSig {
                params: vec![
                    Pattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
                    Pattern::Id(id("i"), Some(Type::Numeric)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.scopes[scope].values.get(&id("list")).unwrap();

                    let Value::List(t, list) = list else {
                        return Err(RuntimeError(format!(
                            "slice() list must be a list, is a: {}",
                            list.ty()
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

                    Ok(Value::List(
                        t.clone(),
                        list.clone().split_off((i as usize).min(list.len())),
                    ))
                }),
            },
            FnSig {
                params: vec![
                    Pattern::Id(id("text"), Some(Type::Str)),
                    Pattern::Id(id("i"), Some(Type::Numeric)),
                ],
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
                params: vec![
                    Pattern::Id(id("text"), Some(Type::Str)),
                    Pattern::Id(id("range"), Some(Type::Tuple)),
                ],
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
    );

    runtime.builtin(
        "index",
        [FnSig {
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
    );

    runtime.builtin(
        "insert",
        [FnSig {
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
    );

    runtime.builtin(
        "trim",
        [FnSig {
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
    );

    runtime.builtin(
        "len",
        [FnSig {
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
    );

    runtime.builtin(
        "chars",
        [FnSig {
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
    );

    runtime.builtin(
        "int",
        [FnSig {
            params: vec![idpat("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.scopes[scope].values.get(&id("data")).unwrap();

                let result = data.auto_coerce_int()?;

                Ok(Value::Numeric(Numeric::Int(result)))
            }),
        }],
    );
}
