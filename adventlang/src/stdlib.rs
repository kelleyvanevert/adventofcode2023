use crate::{
    ast::{DeclarePattern, Identifier, Type},
    runtime::{Dict, FnBody, FnSig, Runtime, Value},
    value::{Numeric, RuntimeError},
};

fn id(id: &str) -> Identifier {
    Identifier(id.into())
}

fn idpat(id: &str) -> DeclarePattern {
    DeclarePattern::Id(Identifier(id.into()), None)
}

fn idpat_ty(id: &str, ty: Type) -> DeclarePattern {
    DeclarePattern::Id(Identifier(id.into()), Some(ty))
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
                            if list.len() == 0 {
                                return Ok(Value::Nil);
                            }

                            match list.iter().min().cloned() {
                                Some(result) => Ok(result),
                                None => Err(RuntimeError(
                                    "error getting min: could not compare all elements".into(),
                                )),
                            }
                        }
                        _ => Err(RuntimeError(format!("cannot get min of: {}", items.ty()))),
                    }
                }),
            },
            FnSig {
                params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
                body: FnBody::Builtin(|runtime, scope| {
                    let a = runtime.scopes[scope].values.get(&id("a")).unwrap();
                    let b = runtime.scopes[scope].values.get(&id("b")).unwrap();

                    match (a, b) {
                        (&Value::Nil, b) => Ok(b.clone()),
                        (a, &Value::Nil) => Ok(a.clone()),
                        _ => Ok(a.min(b).clone()),
                    }
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
                            if list.len() == 0 {
                                return Ok(Value::Nil);
                            }

                            match list.iter().max().cloned() {
                                Some(result) => Ok(result),
                                None => Err(RuntimeError(
                                    "error getting max: could not compare all elements".into(),
                                )),
                            }
                        }
                        _ => Err(RuntimeError(format!("cannot get max of: {}", items.ty()))),
                    }
                }),
            },
            FnSig {
                params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
                body: FnBody::Builtin(|runtime, scope| {
                    let a = runtime.scopes[scope].values.get(&id("a")).unwrap();
                    let b = runtime.scopes[scope].values.get(&id("b")).unwrap();

                    match (a, b) {
                        (&Value::Nil, b) => Ok(b.clone()),
                        (a, &Value::Nil) => Ok(a.clone()),
                        _ => Ok(a.max(b).clone()),
                    }
                }),
            },
        ],
    );

    runtime.builtin(
        "chunks",
        [FnSig {
            params: vec![
                DeclarePattern::Id("items".into(), Some(Type::List(Type::Any.into()))),
                DeclarePattern::Id("size".into(), Some(Type::Numeric)),
            ],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(t, list) = items else {
                    return Err(RuntimeError(format!(
                        "cannot get chunks of: {}",
                        items.ty()
                    )));
                };

                let size = runtime.scopes[scope].values.get(&id("size")).unwrap();

                let Value::Numeric(Numeric::Int(size)) = size else {
                    return Err(RuntimeError(format!(
                        "chunks() size must be int >= 1, is a: {}",
                        size.ty()
                    )));
                };

                if size < &1 {
                    return Err(RuntimeError(format!(
                        "chunks() size must be int >= 1, is: {}",
                        size
                    )));
                }

                Ok(Value::List(
                    Type::List(t.clone().into()),
                    list.chunks_exact(*size as usize)
                        .map(|chunk| Value::List(t.clone(), chunk.to_vec()))
                        .collect::<Vec<_>>(),
                ))
            }),
        }],
    );

    // holy fuck so many clones..
    // :|
    // I can do better!
    runtime.builtin(
        "sort_by_key",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(t, list) = items.clone() else {
                    return Err(RuntimeError(format!(
                        "sort_by_key() items must be a list, is a: {}",
                        items.ty()
                    )));
                };

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "sort_by_key() cb must be a fn, is a: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                let mut sorting_keys = Vec::with_capacity(list.len());

                for (i, item) in list.clone().into_iter().enumerate() {
                    let key = runtime.invoke(def.clone(), vec![(None, item)])?;
                    sorting_keys.push((i, key));
                }

                sorting_keys.sort_by_cached_key(|t| t.1.clone());

                let mut result = vec![Value::Nil; list.len()];

                for (dest, (source, _)) in sorting_keys.iter().enumerate() {
                    result[dest] = list[*source].clone();
                }

                Ok(Value::List(t.clone(), result))
            }),
        }],
    );

    runtime.builtin(
        "reverse",
        [FnSig {
            params: vec![idpat("items")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                match items.clone() {
                    Value::List(t, mut list) => {
                        list.reverse();
                        Ok(Value::List(t, list))
                    }
                    Value::Tuple(mut list) => {
                        list.reverse();
                        Ok(Value::Tuple(list))
                    }
                    _ => {
                        return Err(RuntimeError(format!(
                            "reverse() items must be a list or tuple, is a: {}",
                            items.ty()
                        )));
                    }
                }
            }),
        }],
    );

    runtime.builtin(
        "zip",
        [FnSig {
            params: vec![idpat("xs"), idpat("ys")],
            body: FnBody::Builtin(|runtime, scope| {
                let xs = runtime.scopes[scope].values.get(&id("xs")).unwrap();
                let ys = runtime.scopes[scope].values.get(&id("ys")).unwrap();

                match (&xs, &ys) {
                    (Value::List(_, x_els), Value::List(_, y_els)) => Ok(Value::List(
                        Type::Tuple,
                        x_els
                            .into_iter()
                            .zip(y_els.into_iter())
                            .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
                            .collect(),
                    )),
                    (Value::Tuple(x_els), Value::Tuple(y_els)) => Ok(Value::Tuple(
                        x_els
                            .into_iter()
                            .zip(y_els.into_iter())
                            .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
                            .collect(),
                    )),
                    _ => {
                        return Err(RuntimeError(format!(
                            "cannot apply zip to these types: {}, {}",
                            xs.ty(),
                            ys.ty()
                        )))
                    }
                }
            }),
        }],
    );

    runtime.builtin(
        "fold",
        [FnSig {
            params: vec![idpat("items"), idpat("init"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();
                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();
                let init = runtime.scopes[scope].values.get(&id("init")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "fold() cb must be a fn, is a: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                match items {
                    Value::List(_, els) => {
                        Ok(els.clone().into_iter().try_fold(init.clone(), |acc, el| {
                            runtime.invoke(def.clone(), vec![(None, acc), (None, el.clone())])
                        })?)
                    }
                    Value::Tuple(els) => {
                        Ok(els.clone().into_iter().try_fold(init.clone(), |acc, el| {
                            runtime.invoke(def.clone(), vec![(None, acc), (None, el.clone())])
                        })?)
                    }
                    _ => {
                        return Err(RuntimeError(format!(
                            "cannot apply fold to these types: {}, {}, {}",
                            items.ty(),
                            cb.ty(),
                            init.ty()
                        )))
                    }
                }
            }),
        }],
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
        "dict",
        [FnSig {
            params: vec![DeclarePattern::Id(
                id("pairs"),
                Some(Type::List(Type::Any.into())),
            )],
            body: FnBody::Builtin(|runtime, scope| {
                let pairs = runtime.scopes[scope].values.get(&id("pairs")).unwrap();

                let Value::List(_, pairs) = pairs.clone() else {
                    return Err(RuntimeError(format!(
                        "dict() pairs must be list of tuples, is a: {}",
                        pairs.ty()
                    )));
                };

                let mut dict = Dict::new();

                for pair in pairs {
                    let Value::Tuple(elements) = pair else {
                        return Err(RuntimeError(format!(
                            "each dict() pair must be a tuple, is a: {}",
                            pair.ty()
                        )));
                    };

                    let mut elements = elements.into_iter();

                    let Some(key) = elements.next() else {
                        return Err(RuntimeError(format!("dict() pair without key")));
                    };

                    let Some(value) = elements.next() else {
                        return Err(RuntimeError(format!("dict() pair without key")));
                    };

                    dict.0.insert(key, value);
                }

                Ok(Value::Dict(dict))
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
        "any",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.scopes[scope].values.get(&id("items")).unwrap();

                let Value::List(_, list) = items else {
                    return Err(RuntimeError(format!(
                        "any() items must be a list, is a: {}",
                        items.ty()
                    )));
                };

                let list = list.clone();

                let cb = runtime.scopes[scope].values.get(&id("cb")).unwrap();

                let Value::FnDef(def) = cb else {
                    return Err(RuntimeError(format!(
                        "cannot use any() w/ cb of type: {}",
                        cb.ty()
                    )));
                };

                let def = def.clone();

                for item in list {
                    let item = runtime.invoke(def.clone(), vec![(None, item)])?;
                    if item.truthy()? {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
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
                    if item.truthy()? {
                        return Ok(item);
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
        [
            FnSig {
                params: vec![
                    DeclarePattern::Id("text".into(), Some(Type::Str)),
                    DeclarePattern::Id("sep".into(), Some(Type::Str)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "split() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let sep = runtime.scopes[scope].values.get(&id("sep")).unwrap();

                    let Value::Str(sep) = sep else {
                        return Err(RuntimeError(format!(
                            "split() sep must be a string, is a: {}",
                            sep.ty()
                        )));
                    };

                    let result = text
                        .split(sep.as_str())
                        .map(|piece| Value::Str(text.substr_from(piece)))
                        .collect::<Vec<_>>();

                    Ok(Value::List(Type::Str, result))
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id("text".into(), Some(Type::Str)),
                    DeclarePattern::Id("sep".into(), Some(Type::Regex)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.scopes[scope].values.get(&id("text")).unwrap();

                    let Value::Str(text) = text else {
                        return Err(RuntimeError(format!(
                            "split() text must be a string, is a: {}",
                            text.ty()
                        )));
                    };

                    let sep = runtime.scopes[scope].values.get(&id("sep")).unwrap();

                    let Value::Regex(sep) = sep else {
                        return Err(RuntimeError(format!(
                            "split() setp must be a regex, is a: {}",
                            sep.ty()
                        )));
                    };

                    let result = sep
                        .0
                        .split(&text)
                        .map(|piece| Value::Str(text.substr_from(piece)))
                        .collect::<Vec<_>>();

                    Ok(Value::List(Type::Str, result))
                }),
            },
        ],
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
                        "replace() def must be a tuple, is a: {}",
                        def.ty()
                    )));
                };

                if def.len() != 2 {
                    return Err(RuntimeError(format!(
                        "replace() def must be a tuple with two elements"
                    )));
                }

                let replace = def.get(1).unwrap();

                let Value::Str(replace) = replace else {
                    return Err(RuntimeError(format!("replace() def[1] must be a string")));
                };

                let find = def.get(0).unwrap();

                match find {
                    Value::Str(find) => Ok(Value::Str(text.replace(find.as_str(), replace).into())),
                    Value::Regex(find) => Ok(Value::Str(
                        find.0.replace_all(&text, replace.to_string()).into(),
                    )),
                    _ => {
                        return Err(RuntimeError(format!(
                            "replace() def[0] must be a string or regex, is a: {}",
                            find.ty()
                        )));
                    }
                }
            }),
        }],
    );

    runtime.builtin(
        "slice",
        [
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
                    DeclarePattern::Id(id("i"), Some(Type::Numeric)),
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
                    DeclarePattern::Id(id("text"), Some(Type::Str)),
                    DeclarePattern::Id(id("i"), Some(Type::Numeric)),
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
                    DeclarePattern::Id(id("text"), Some(Type::Str)),
                    DeclarePattern::Id(id("range"), Some(Type::Tuple)),
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
                        "index() i must be an int, is a: {}",
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

    runtime.builtin(
        "sqrt",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.scopes[scope].values.get(&id("num")).unwrap();

                let Value::Numeric(num) = num else {
                    return Err(RuntimeError(format!(
                        "sqrt() num should be a num, is a: {}",
                        num.ty()
                    )));
                };

                Ok(Value::Numeric(Numeric::Double(num.get_double().sqrt())))
            }),
        }],
    );

    runtime.builtin(
        "ceil",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.scopes[scope].values.get(&id("num")).unwrap();

                let Value::Numeric(num) = num else {
                    return Err(RuntimeError(format!(
                        "ceil() num should be a num, is a: {}",
                        num.ty()
                    )));
                };

                Ok(Value::Numeric(Numeric::Double(num.get_double().ceil())))
            }),
        }],
    );

    runtime.builtin(
        "floor",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.scopes[scope].values.get(&id("num")).unwrap();

                let Value::Numeric(num) = num else {
                    return Err(RuntimeError(format!(
                        "floor() num should be a num, is a: {}",
                        num.ty()
                    )));
                };

                Ok(Value::Numeric(Numeric::Double(num.get_double().floor())))
            }),
        }],
    );

    runtime.builtin(
        "abs",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.scopes[scope].values.get(&id("num")).unwrap();

                let Value::Numeric(num) = num else {
                    return Err(RuntimeError(format!(
                        "abs() num should be a num, is a: {}",
                        num.ty()
                    )));
                };

                match num {
                    Numeric::Int(n) => Ok(Value::Numeric(Numeric::Int(n.abs()))),
                    Numeric::Double(d) => Ok(Value::Numeric(Numeric::Double(d.abs()))),
                }
            }),
        }],
    );

    runtime.builtin(
        "round",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.scopes[scope].values.get(&id("num")).unwrap();

                let Value::Numeric(num) = num else {
                    return Err(RuntimeError(format!(
                        "abs() num should be a num, is a: {}",
                        num.ty()
                    )));
                };

                match num {
                    Numeric::Int(n) => Ok(Value::Numeric(Numeric::Int(*n))),
                    Numeric::Double(d) => Ok(Value::Numeric(Numeric::Int(d.round() as i64))),
                }
            }),
        }],
    );
}
