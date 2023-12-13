use std::cmp::Ordering;

use arcstr::Substr;

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
                let text = runtime.get_scope(scope).get_unchecked("text");

                println!("{}", runtime.display(text, true));
                Ok(runtime.new_value(Value::Nil))
            }),
        }],
    );

    runtime.builtin(
        "run",
        [FnSig {
            params: vec![idpat("f")],
            body: FnBody::Builtin(|runtime, scope| {
                let f = runtime.get_scope(scope).get_unchecked("f");

                Ok(runtime.invoke(f, vec![])?)
            }),
        }],
    );

    runtime.builtin(
        "min",
        [
            FnSig {
                params: vec![idpat_ty("items", Type::List(Type::Any.into()))],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    match runtime.get_value(items) {
                        Value::List(_, list) => {
                            if list.len() == 0 {
                                return Ok(runtime.new_value(Value::Nil));
                            }

                            match list.iter().min_by(|&&a, &&b| runtime.cmp(a, b)) {
                                Some(result) => Ok((*result, false)),
                                None => RuntimeError(
                                    "error getting min: could not compare all elements".into(),
                                )
                                .into(),
                            }
                        }
                        _ => RuntimeError(format!("cannot get min of: {}", runtime.get_ty(items)))
                            .into(),
                    }
                }),
            },
            FnSig {
                params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
                body: FnBody::Builtin(|runtime, scope| {
                    let a = runtime.get_scope(scope).get_unchecked("a");
                    let b = runtime.get_scope(scope).get_unchecked("b");

                    if runtime.get_value(a) == &Value::Nil {
                        return Ok((b, false));
                    } else if runtime.get_value(b) == &Value::Nil {
                        return Ok((a, false));
                    }

                    match runtime.cmp(a, b) {
                        Ordering::Greater => Ok((b, false)),
                        Ordering::Less => Ok((a, false)),
                        _ => Ok((a, false)),
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
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    match runtime.get_value(items) {
                        Value::List(_, list) => {
                            if list.len() == 0 {
                                return Ok(runtime.new_value(Value::Nil));
                            }

                            match list.iter().max_by(|&&a, &&b| runtime.cmp(a, b)) {
                                Some(result) => Ok((*result, false)),
                                None => RuntimeError(
                                    "error getting max: could not compare all elements".into(),
                                )
                                .into(),
                            }
                        }
                        _ => RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                            .into(),
                    }
                }),
            },
            FnSig {
                params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
                body: FnBody::Builtin(|runtime, scope| {
                    let a = runtime.get_scope(scope).get_unchecked("a");
                    let b = runtime.get_scope(scope).get_unchecked("b");

                    if runtime.get_value(a) == &Value::Nil {
                        return Ok((b, false));
                    } else if runtime.get_value(b) == &Value::Nil {
                        return Ok((a, false));
                    }

                    match runtime.cmp(a, b) {
                        Ordering::Greater => Ok((a, false)),
                        Ordering::Less => Ok((b, false)),
                        _ => Ok((a, false)),
                    }
                }),
            },
        ],
    );

    runtime.builtin(
        "add",
        [FnSig {
            params: vec![idpat("a"), idpat("b")],
            body: FnBody::Builtin(|runtime, scope| {
                let a = runtime.get_scope(scope).get_unchecked("a");
                let b = runtime.get_scope(scope).get_unchecked("b");

                match (runtime.get_value(a).clone(), runtime.get_value(b).clone()) {
                    (Value::Str(a), Value::Str(b)) => {
                        let new = Substr::from(a.to_string() + &b);
                        Ok(runtime.new_value(Value::Str(new)))
                    }
                    (Value::Numeric(a), Value::Numeric(b)) => {
                        Ok(runtime.new_value(Value::Numeric(a.add(&b))))
                    }
                    (Value::List(_, a), Value::List(_, b)) => {
                        let elements = a
                            .into_iter()
                            .chain(b.into_iter())
                            .map(|v| runtime.ensure_new((v, false)))
                            .collect();

                        Ok(runtime.new_value(Value::List(Type::Any, elements)))
                    }
                    _ => {
                        return RuntimeError(format!(
                            "can't perform {} + {}",
                            runtime.get_ty(a),
                            runtime.get_ty(b)
                        ))
                        .into()
                    }
                }
            }),
        }],
    );

    runtime.builtin(
        "chunks",
        [FnSig {
            params: vec![
                DeclarePattern::Id("items".into(), Some(Type::List(Type::Any.into()))),
                DeclarePattern::Id("size".into(), Some(Type::Numeric)),
            ],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(t, list) = runtime.get_value(items).clone() else {
                    return RuntimeError(format!(
                        "cannot get chunks of: {}",
                        runtime.get_ty(items)
                    ))
                    .into();
                };

                let size = runtime.get_scope(scope).get_unchecked("size");

                let Value::Numeric(Numeric::Int(size)) = runtime.get_value(size) else {
                    return RuntimeError(format!(
                        "chunks() size must be int >= 1, is a: {}",
                        runtime.get_ty(size)
                    ))
                    .into();
                };

                if size < &1 {
                    return RuntimeError(format!("chunks() size must be int >= 1, is: {}", size))
                        .into();
                }

                let chunks = list
                    .chunks_exact(*size as usize)
                    .map(|chunk| runtime.new_value(Value::List(t.clone(), chunk.to_vec())).0)
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::List(t.clone().into()), chunks)))
            }),
        }],
    );

    // holy fuck so many clones..
    // :|
    // surely I can do better!
    runtime.builtin(
        "sort_by_key",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(t, list) = runtime.get_value(items).clone() else {
                    return RuntimeError(format!(
                        "sort_by_key() items must be a list, is a: {}",
                        runtime.get_ty(items)
                    ))
                    .into();
                };

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                let mut sorting_keys = Vec::with_capacity(list.len());

                for (i, item) in list.clone().into_iter().enumerate() {
                    let key = runtime.invoke(cb, vec![(None, item)])?;
                    sorting_keys.push((i, key.0));
                }

                sorting_keys.sort_by(|a, b| runtime.cmp(a.1, b.1));

                let mut result = list
                    .iter()
                    .map(|_| runtime.new_value(Value::Nil).0)
                    .collect::<Vec<_>>();

                for (dest, (source, _)) in sorting_keys.iter().enumerate() {
                    result[dest] = list[*source].clone();
                }

                Ok(runtime.new_value(Value::List(t.clone(), result)))
            }),
        }],
    );

    runtime.builtin(
        "reverse",
        [FnSig {
            params: vec![idpat("items")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");
                let items = runtime.clone(items);

                match runtime.get_value(items.0).clone() {
                    Value::List(t, mut list) => {
                        list.reverse();
                        Ok(runtime.new_value(Value::List(t, list)))
                    }
                    Value::Tuple(mut list) => {
                        list.reverse();
                        Ok(runtime.new_value(Value::Tuple(list)))
                    }
                    _ => {
                        return RuntimeError(format!(
                            "reverse() items must be a list or tuple, is a: {}",
                            runtime.get_ty(items.0)
                        ))
                        .into();
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
                let xs = runtime.get_scope(scope).get_unchecked("xs");
                let ys = runtime.get_scope(scope).get_unchecked("ys");

                match (runtime.get_value(xs).clone(), runtime.get_value(ys).clone()) {
                    (Value::List(_, x_els), Value::List(_, y_els)) => {
                        let zipped_items = x_els
                            .into_iter()
                            .zip(y_els.into_iter())
                            .map(|(x, y)| runtime.new_value(Value::Tuple(vec![x, y])).0)
                            .collect();

                        Ok(runtime.new_value(Value::List(Type::Tuple, zipped_items)))
                    }
                    (Value::Tuple(x_els), Value::Tuple(y_els)) => {
                        let zipped_items = x_els
                            .into_iter()
                            .zip(y_els.into_iter())
                            .map(|(x, y)| runtime.new_value(Value::Tuple(vec![x, y])).0)
                            .collect();

                        Ok(runtime.new_value(Value::Tuple(zipped_items)))
                    }
                    _ => {
                        return RuntimeError(format!(
                            "cannot apply zip to these types: {}, {}",
                            runtime.get_ty(xs),
                            runtime.get_ty(ys),
                        ))
                        .into()
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
                let items = runtime.get_scope(scope).get_unchecked("items");
                let cb = runtime.get_scope(scope).get_unchecked("cb");
                let init = runtime.get_scope(scope).get_unchecked("init");

                match runtime.get_value(items).clone() {
                    Value::List(_, els) => {
                        Ok(els.clone().into_iter().try_fold((init, false), |acc, el| {
                            runtime.invoke(cb, vec![(None, acc.0), (None, el.clone())])
                        })?)
                    }
                    Value::Tuple(els) => {
                        Ok(els.clone().into_iter().try_fold((init, false), |acc, el| {
                            runtime.invoke(cb, vec![(None, acc.0), (None, el.clone())])
                        })?)
                    }
                    _ => {
                        return RuntimeError(format!(
                            "cannot apply fold to these types: {}, {}, {}",
                            runtime.get_ty(items),
                            runtime.get_ty(cb),
                            runtime.get_ty(init),
                        ))
                        .into()
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
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items) else {
                    return RuntimeError(format!("cannot get map of")).into();
                };

                let list = list.clone();

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                let mut result = vec![];
                for item in list.iter() {
                    result.push(runtime.invoke(cb, vec![(None, item.clone())])?.0);
                }

                Ok(runtime.new_value(Value::List(Type::Any, result)))
            }),
        }],
    );

    runtime.builtin(
        "flat_map",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items).clone() else {
                    return RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                        .into();
                };

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                let mut result = vec![];
                for item in list.into_iter() {
                    let value = runtime.invoke(cb, vec![(None, item.clone())])?;
                    let Value::List(_, items) = runtime.get_value(value.0) else {
                        return RuntimeError(format!(
                            "flat_map cb should return lists, returned: {}",
                            runtime.get_ty(value.0)
                        ))
                        .into();
                    };

                    // TODO type-check

                    result.extend(items);
                }

                // TODO type
                Ok(runtime.new_value(Value::List(Type::Any, result)))
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
                let pairs = runtime.get_scope(scope).get_unchecked("pairs");

                let Value::List(_, pairs) = runtime.get_value(pairs).clone() else {
                    return RuntimeError(format!(
                        "dict() pairs must be list of tuples, is a: {}",
                        runtime.get_ty(pairs)
                    ))
                    .into();
                };

                let mut dict = Dict::new();

                for pair in pairs {
                    let Value::Tuple(elements) = runtime.get_value(pair) else {
                        return RuntimeError(format!(
                            "each dict() pair must be a tuple, is a: {}",
                            runtime.get_ty(pair)
                        ))
                        .into();
                    };

                    let mut elements = elements.into_iter();

                    let Some(key) = elements.next() else {
                        return RuntimeError(format!("dict() pair without key")).into();
                    };

                    let Some(value) = elements.next() else {
                        return RuntimeError(format!("dict() pair without key")).into();
                    };

                    dict.insert(runtime, *key, *value);
                }

                Ok(runtime.new_value(Value::Dict(dict)))
            }),
        }],
    );

    runtime.builtin(
        "in",
        [
            FnSig {
                params: vec![
                    idpat_ty("needle", Type::Str),
                    idpat_ty("haystack", Type::Str),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let needle = runtime.get_scope(scope).get_unchecked("needle");
                    let haystack = runtime.get_scope(scope).get_unchecked("haystack");

                    let Value::Str(needle) = runtime.get_value(needle).clone() else {
                        return RuntimeError(format!(
                            "in() needle must be a str, is a: {}",
                            runtime.get_ty(needle)
                        ))
                        .into();
                    };

                    let Value::Str(haystack) = runtime.get_value(haystack).clone() else {
                        return RuntimeError(format!(
                            "in() haystack must be a str, is a: {}",
                            runtime.get_ty(haystack)
                        ))
                        .into();
                    };

                    Ok(runtime.new_value(Value::Bool(haystack.contains(&needle.as_str()))))
                }),
            },
            FnSig {
                params: vec![
                    idpat("needle"),
                    idpat_ty("haystack", Type::List(Type::Any.into())),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let needle = runtime.get_scope(scope).get_unchecked("needle");
                    let haystack = runtime.get_scope(scope).get_unchecked("haystack");

                    let Value::List(_, haystack) = runtime.get_value(haystack).clone() else {
                        return RuntimeError(format!(
                            "cannot get in() of: {}",
                            runtime.get_ty(haystack)
                        ))
                        .into();
                    };

                    for el in haystack {
                        if runtime.eq(el, needle) {
                            return Ok(runtime.new_value(Value::Bool(true)));
                        }
                    }

                    Ok(runtime.new_value(Value::Bool(false)))
                }),
            },
        ],
    );

    runtime.builtin(
        "filter",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items) else {
                    return RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                        .into();
                };

                let list = list.clone();

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                let mut result = vec![];
                for item in list.iter() {
                    let r = runtime.invoke(cb, vec![(None, item.clone())])?;
                    if runtime.get_value(r.0).truthy()? {
                        result.push(*item);
                    }
                }

                // TODO
                Ok(runtime.new_value(Value::List(Type::Any, result)))
            }),
        }],
    );

    runtime.builtin(
        "filter_map",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items) else {
                    return RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                        .into();
                };

                let list = list.clone();

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                let mut result = vec![];
                for item in list.iter() {
                    let item = runtime.invoke(cb, vec![(None, item.clone())])?;

                    match runtime.get_value(item.0) {
                        // TODO fix the "nil as well as unit" problem
                        Value::Nil => {}
                        _ => {
                            result.push(item.0);
                        }
                    }
                }

                // TODO
                Ok(runtime.new_value(Value::List(Type::Any, result)))
            }),
        }],
    );

    runtime.builtin(
        "any",
        [
            FnSig {
                params: vec![idpat("items"), idpat("cb")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    let Value::List(_, list) = runtime.get_value(items).clone() else {
                        return RuntimeError(format!(
                            "any() items must be a list, is a: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
                    };

                    let cb = runtime.get_scope(scope).get_unchecked("cb");

                    for item in list {
                        let item = runtime.invoke(cb, vec![(None, item)])?;
                        if runtime.get_value(item.0).truthy()? {
                            return Ok(runtime.new_value(Value::Bool(true)));
                        }
                    }

                    Ok(runtime.new_value(Value::Bool(false)))
                }),
            },
            FnSig {
                params: vec![idpat("items")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    let Value::List(_, list) = runtime.get_value(items).clone() else {
                        return RuntimeError(format!(
                            "any() items must be a list, is a: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
                    };

                    for item in list {
                        if runtime.get_value(item).truthy()? {
                            return Ok(runtime.new_value(Value::Bool(true)));
                        }
                    }

                    Ok(runtime.new_value(Value::Bool(false)))
                }),
            },
        ],
    );

    runtime.builtin(
        "all",
        [
            FnSig {
                params: vec![idpat("items"), idpat("cb")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    let Value::List(_, list) = runtime.get_value(items) else {
                        return RuntimeError(format!(
                            "all() items must be a list, is a: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
                    };

                    let list = list.clone();

                    let cb = runtime.get_scope(scope).get_unchecked("cb");

                    for item in list {
                        let item = runtime.invoke(cb, vec![(None, item)])?;
                        if !runtime.get_value(item.0).truthy()? {
                            return Ok(runtime.new_value(Value::Bool(false)));
                        }
                    }

                    Ok(runtime.new_value(Value::Bool(true)))
                }),
            },
            FnSig {
                params: vec![idpat("items")],
                body: FnBody::Builtin(|runtime, scope| {
                    let items = runtime.get_scope(scope).get_unchecked("items");

                    let Value::List(_, list) = runtime.get_value(items) else {
                        return RuntimeError(format!(
                            "all() items must be a list, is a: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
                    };

                    let list = list.clone();

                    for item in list {
                        if !runtime.get_value(item).truthy()? {
                            return Ok(runtime.new_value(Value::Bool(false)));
                        }
                    }

                    Ok(runtime.new_value(Value::Bool(true)))
                }),
            },
        ],
    );

    runtime.builtin(
        "find_map",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items) else {
                    return RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                        .into();
                };

                let list = list.clone();

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                for item in list {
                    let item = runtime.invoke(cb, vec![(None, item)])?;
                    if runtime.get_value(item.0).truthy()? {
                        return Ok(item);
                    }
                }

                Ok(runtime.new_value(Value::Nil))
            }),
        }],
    );

    runtime.builtin(
        "find",
        [FnSig {
            params: vec![idpat("items"), idpat("cb")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, list) = runtime.get_value(items) else {
                    return RuntimeError(format!("cannot get max of: {}", runtime.get_ty(items)))
                        .into();
                };

                let list = list.clone();

                let cb = runtime.get_scope(scope).get_unchecked("cb");

                for item in list {
                    let check = runtime.invoke(cb, vec![(None, item.clone())])?;
                    if runtime.get_value(check.0).truthy()? {
                        return Ok((item, false));
                    }
                }

                Ok(runtime.new_value(Value::Nil))
            }),
        }],
    );

    runtime.builtin(
        "range",
        [FnSig {
            params: vec![idpat("start"), idpat("end")],
            body: FnBody::Builtin(|runtime, scope| {
                let start = runtime.get_scope(scope).get_unchecked("start");

                let Value::Numeric(start) = runtime.get_value(start) else {
                    return RuntimeError(format!(
                        "range() start must be int, is: {}",
                        runtime.get_ty(start)
                    ))
                    .into();
                };

                let start = start.get_int()?;

                let end = runtime.get_scope(scope).get_unchecked("end");

                let Value::Numeric(end) = runtime.get_value(end) else {
                    return RuntimeError(format!(
                        "range() end must be int, is: {}",
                        runtime.get_ty(end)
                    ))
                    .into();
                };

                let end = end.get_int()?;

                if end >= start {
                    let items = (start..end)
                        .map(|n| runtime.new_value(Value::Numeric(Numeric::Int(n))).0)
                        .collect();

                    Ok(runtime.new_value(Value::List(Type::Numeric, items)))
                } else {
                    // TODO allow this conditionally or something?
                    // Ok(Value::List(
                    //     Type::Numeric,
                    //     (0..(start - end))
                    //         .map(|n| Value::Numeric(Numeric::Int(start - n)))
                    //         .collect(),
                    // ))

                    Ok(runtime.new_value(Value::List(Type::Numeric, vec![])))
                }
            }),
        }],
    );

    runtime.builtin(
        "enumerate",
        [FnSig {
            params: vec![idpat("items")],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                Ok(match runtime.get_value(items).clone() {
                    Value::List(_, list) => {
                        let new_items = list
                            .iter()
                            .cloned()
                            .enumerate()
                            .map(|(i, item)| {
                                let index =
                                    runtime.new_value(Value::Numeric(Numeric::Int(i as i64)));

                                runtime.new_value(Value::Tuple(vec![index.0, item])).0
                            })
                            .collect::<Vec<_>>();

                        runtime.new_value(Value::List(
                            Type::Tuple, // TODO generic tuple types
                            new_items,
                        ))
                    }
                    Value::Tuple(list) => {
                        let new_items = list
                            .iter()
                            .cloned()
                            .enumerate()
                            .map(|(i, item)| {
                                let index =
                                    runtime.new_value(Value::Numeric(Numeric::Int(i as i64)));

                                runtime.new_value(Value::Tuple(vec![index.0, item])).0
                            })
                            .collect::<Vec<_>>();

                        runtime.new_value(Value::Tuple(new_items))
                    }
                    _ => {
                        return RuntimeError(format!(
                            "cannot get max of: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
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
                let items = runtime.get_scope(scope).get_unchecked("items");

                let list = match runtime.get_value(items) {
                    Value::List(_, list) => list,
                    Value::Tuple(list) => list,
                    _ => {
                        return RuntimeError(format!(
                            "cannot get max of: {}",
                            runtime.get_ty(items)
                        ))
                        .into();
                    }
                };

                let mut result = Value::Numeric(Numeric::Int(0));
                for item in list.iter() {
                    result = result.add(runtime.get_value(*item))?;
                }

                Ok(runtime.new_value(result))
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
                    let text = runtime.get_scope(scope).get_unchecked("text");

                    let Value::Str(text) = runtime.get_value(text).clone() else {
                        return RuntimeError(format!(
                            "split() text must be a string, is a: {}",
                            runtime.get_ty(text)
                        ))
                        .into();
                    };

                    let sep = runtime.get_scope(scope).get_unchecked("sep");

                    let Value::Str(sep) = runtime.get_value(sep).clone() else {
                        return RuntimeError(format!(
                            "split() sep must be a string, is a: {}",
                            runtime.get_ty(sep)
                        ))
                        .into();
                    };

                    let result = text
                        .split(sep.as_str())
                        .map(|piece| runtime.new_value(Value::Str(text.substr_from(piece))).0)
                        .collect::<Vec<_>>();

                    Ok(runtime.new_value(Value::List(Type::Str, result)))
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id("text".into(), Some(Type::Str)),
                    DeclarePattern::Id("sep".into(), Some(Type::Regex)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.get_scope(scope).get_unchecked("text");

                    let Value::Str(text) = runtime.get_value(text).clone() else {
                        return RuntimeError(format!(
                            "split() text must be a string, is a: {}",
                            runtime.get_ty(text)
                        ))
                        .into();
                    };

                    let sep = runtime.get_scope(scope).get_unchecked("sep");

                    let Value::Regex(sep) = runtime.get_value(sep).clone() else {
                        return RuntimeError(format!(
                            "split() setp must be a regex, is a: {}",
                            runtime.get_ty(sep)
                        ))
                        .into();
                    };

                    let result = sep
                        .0
                        .split(&text)
                        .map(|piece| runtime.new_value(Value::Str(text.substr_from(piece))).0)
                        .collect::<Vec<_>>();

                    Ok(runtime.new_value(Value::List(Type::Str, result)))
                }),
            },
        ],
    );

    runtime.builtin(
        "join",
        [FnSig {
            params: vec![
                idpat_ty("items", Type::List(Type::Any.into())),
                idpat_ty("glue", Type::Str),
            ],
            body: FnBody::Builtin(|runtime, scope| {
                let items = runtime.get_scope(scope).get_unchecked("items");

                let Value::List(_, items) = runtime.get_value(items).clone() else {
                    return RuntimeError(format!(
                        "join() items must be a list, is a: {}",
                        runtime.get_ty(items)
                    ))
                    .into();
                };

                let glue = runtime.get_scope(scope).get_unchecked("glue");

                let Value::Str(glue) = runtime.get_value(glue).clone() else {
                    return RuntimeError(format!(
                        "join() glue must be a string, is a: {}",
                        runtime.get_ty(glue)
                    ))
                    .into();
                };

                let result = items
                    .into_iter()
                    .map(|v| runtime.get_value(v).auto_coerce_str())
                    .collect::<Vec<_>>()
                    .join(glue.as_str());

                Ok(runtime.new_value(Value::Str(result.into())))
            }),
        }],
    );

    runtime.builtin(
        "lines",
        [FnSig {
            params: vec![idpat("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text).clone() else {
                    return RuntimeError(format!(
                        "lines() text must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let result = text
                    .clone()
                    .lines()
                    .map(|line| runtime.new_value(Value::Str(text.substr_from(line))).0)
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::Str, result)))
            }),
        }],
    );

    runtime.builtin(
        "match",
        [FnSig {
            params: vec![idpat("text"), idpat("regex")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text).clone() else {
                    return RuntimeError(format!(
                        "match() text must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let regex = runtime.get_scope(scope).get_unchecked("regex");

                let Value::Regex(regex) = runtime.get_value(regex).clone() else {
                    return RuntimeError(format!(
                        "match() regex must be a regex, is a: {}",
                        runtime.get_ty(regex)
                    ))
                    .into();
                };

                match regex.0.captures(&text) {
                    Some(cap) => {
                        let m = cap.get(0).unwrap();
                        let matched_part = runtime.new_value(Value::Str(m.as_str().into()));
                        let offset =
                            runtime.new_value(Value::Numeric(Numeric::Int(m.start() as i64)));
                        Ok(runtime.new_value(Value::Tuple(vec![matched_part.0, offset.0])))
                    }
                    None => Ok(runtime.new_value(Value::Nil)),
                }
            }),
        }],
    );

    runtime.builtin(
        "match_all",
        [FnSig {
            params: vec![idpat("text"), idpat("regex")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text).clone() else {
                    return RuntimeError(format!(
                        "match() text must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let regex = runtime.get_scope(scope).get_unchecked("regex");

                let Value::Regex(regex) = runtime.get_value(regex).clone() else {
                    return RuntimeError(format!(
                        "match() regex must be a regex, is a: {}",
                        runtime.get_ty(regex)
                    ))
                    .into();
                };

                let items = regex
                    .0
                    .captures_iter(&text)
                    .map(|cap| {
                        let m = cap.get(0).unwrap();
                        let matched = runtime.new_value(Value::Str(m.as_str().into()));
                        let offset =
                            runtime.new_value(Value::Numeric(Numeric::Int(m.start() as i64)));

                        runtime.new_value(Value::Tuple(vec![matched.0, offset.0])).0
                    })
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::Tuple, items)))
            }),
        }],
    );

    runtime.builtin(
        "starts_with",
        [FnSig {
            params: vec![idpat("text"), idpat("substr")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text) else {
                    return RuntimeError(format!(
                        "starts_with() text must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let substr = runtime.get_scope(scope).get_unchecked("substr");

                let Value::Str(substr) = runtime.get_value(substr) else {
                    return RuntimeError(format!(
                        "starts_with() substr must be a string, is a: {}",
                        runtime.get_ty(substr)
                    ))
                    .into();
                };

                Ok(runtime.new_value(Value::Bool(text.starts_with(substr.as_str()))))
            }),
        }],
    );

    runtime.builtin(
        "replace",
        [FnSig {
            params: vec![idpat("text"), idpat("def")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text) else {
                    return RuntimeError(format!(
                        "replace() text must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let def = runtime.get_scope(scope).get_unchecked("def");

                let Value::Tuple(def) = runtime.get_value(def) else {
                    return RuntimeError(format!(
                        "replace() def must be a tuple, is a: {}",
                        runtime.get_ty(def)
                    ))
                    .into();
                };

                if def.len() != 2 {
                    return RuntimeError(format!(
                        "replace() def must be a tuple with two elements"
                    ))
                    .into();
                }

                let replace = *def.get(1).unwrap();

                let Value::Str(replace) = runtime.get_value(replace) else {
                    return RuntimeError(format!("replace() def[1] must be a string")).into();
                };

                let find = *def.get(0).unwrap();

                match runtime.get_value(find) {
                    Value::Str(find) => {
                        Ok(runtime
                            .new_value(Value::Str(text.replace(find.as_str(), replace).into())))
                    }
                    Value::Regex(find) => Ok(runtime.new_value(Value::Str(
                        find.0.replace_all(&text, replace.to_string()).into(),
                    ))),
                    _ => {
                        return RuntimeError(format!(
                            "replace() def[0] must be a string or regex, is a: {}",
                            runtime.get_ty(find)
                        ))
                        .into();
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
                    let list = runtime.get_scope(scope).get_unchecked("list");

                    let Value::List(t, list) = runtime.get_value(list).clone() else {
                        return RuntimeError(format!(
                            "slice() list must be a list, is a: {}",
                            runtime.get_ty(list)
                        ))
                        .into();
                    };

                    let i = runtime.get_scope(scope).get_unchecked("i");

                    let Value::Numeric(i) = runtime.get_value(i) else {
                        return RuntimeError(format!(
                            "slice() i must be an int, is a: {}",
                            runtime.get_ty(i)
                        ))
                        .into();
                    };

                    let i = i.get_int()?;

                    if i < 0 {
                        return RuntimeError(format!("slice() i must be a positive int")).into();
                    }

                    Ok(runtime.new_value(Value::List(
                        t.clone(),
                        list.clone().split_off((i as usize).min(list.len())),
                    )))
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("text"), Some(Type::Str)),
                    DeclarePattern::Id(id("i"), Some(Type::Numeric)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.get_scope(scope).get_unchecked("text");

                    let Value::Str(text) = runtime.get_value(text) else {
                        return RuntimeError(format!(
                            "slice() text must be a string, is a: {}",
                            runtime.get_ty(text)
                        ))
                        .into();
                    };

                    let i = runtime.get_scope(scope).get_unchecked("i");

                    let Value::Numeric(i) = runtime.get_value(i) else {
                        return RuntimeError(format!(
                            "slice() i must be an int, is a: {}",
                            runtime.get_ty(i)
                        ))
                        .into();
                    };

                    let mut i = i.get_int()?;

                    if i < 0 && text.len() as i64 + i >= 0 {
                        i = text.len() as i64 + i;
                    }

                    if i < 0 {
                        return RuntimeError(format!("slice() i must be a positive int")).into();
                    }

                    // println!("text :slice i -- {}, {} -- {}", text.len(), i, text.len());

                    Ok(runtime.new_value(Value::Str(text.substr((i as usize)..text.len()))))
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("text"), Some(Type::Str)),
                    DeclarePattern::Id(id("range"), Some(Type::Tuple)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.get_scope(scope).get_unchecked("text");

                    let Value::Str(text) = runtime.get_value(text).clone() else {
                        return RuntimeError(format!(
                            "slice() text must be a string, is a: {}",
                            runtime.get_ty(text)
                        ))
                        .into();
                    };

                    let range = runtime.get_scope(scope).get_unchecked("range");

                    let Value::Tuple(range) = runtime.get_value(range).clone() else {
                        return RuntimeError(format!(
                            "slice() range must be an (int, int) range, is a: {}",
                            runtime.get_ty(range)
                        ))
                        .into();
                    };

                    let Some(start) = range.get(0) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let Value::Numeric(start) = runtime.get_value(*start) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let start = start.get_int()?;

                    if start < 0 {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    }

                    let Some(end) = range.get(1) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let Value::Numeric(end) = runtime.get_value(*end) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let end = end.get_int()?;

                    if end < 0 {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    }

                    Ok(runtime.new_value(Value::Str(
                        text.substr((start as usize)..(end as usize).min(text.len())),
                    )))
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
                    DeclarePattern::Id(id("range"), Some(Type::Tuple)),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.get_scope(scope).get_unchecked("list");

                    let Value::List(el_type, list) = runtime.get_value(list).clone() else {
                        return RuntimeError(format!(
                            "slice() list must be a list, is a: {}",
                            runtime.get_ty(list)
                        ))
                        .into();
                    };

                    let range = runtime.get_scope(scope).get_unchecked("range");

                    let Value::Tuple(range) = runtime.get_value(range).clone() else {
                        return RuntimeError(format!(
                            "slice() range must be an (int, int) range, is a: {}",
                            runtime.get_ty(range)
                        ))
                        .into();
                    };

                    let Some(start) = range.get(0) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let Value::Numeric(start) = runtime.get_value(*start) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let start = start.get_int()?;

                    if start < 0 {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    }

                    let Some(end) = range.get(1) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let Value::Numeric(end) = runtime.get_value(*end) else {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    };

                    let end = end.get_int()?;

                    if end < 0 {
                        return RuntimeError(format!("slice() range must be an (int, int) range"))
                            .into();
                    }

                    let slice_els = list[(start as usize)..(end as usize).min(list.len())]
                        .into_iter()
                        .map(|v| runtime.clone(*v).0)
                        .collect::<Vec<_>>();

                    Ok(runtime.new_value(Value::List(el_type, slice_els)))
                }),
            },
        ],
    );

    runtime.builtin(
        "index",
        [
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("dict"), Some(Type::Dict)),
                    idpat("key"),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let dict = runtime.get_scope(scope).get_unchecked("dict");

                    let Value::Dict(dict) = runtime.get_value(dict).clone() else {
                        return RuntimeError(format!(
                            "index() dict must be a dict, is a: {}",
                            runtime.get_ty(dict)
                        ))
                        .into();
                    };

                    let key = runtime.get_scope(scope).get_unchecked("key");

                    let result = dict
                        .get(runtime, key)
                        .map(|(key, value)| (value, false))
                        .unwrap_or(runtime.new_value(Value::Nil));

                    Ok(result)
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
                    idpat("i"),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.get_scope(scope).get_unchecked("list");

                    let Value::List(_, items) = runtime.get_value(list).clone() else {
                        return RuntimeError(format!(
                            "index() list must be a list, is a: {}",
                            runtime.get_ty(list)
                        ))
                        .into();
                    };

                    let i = runtime.get_scope(scope).get_unchecked("i");

                    let Value::Numeric(i) = runtime.get_value(i) else {
                        return RuntimeError(format!(
                            "index() i must be an int, is a: {}",
                            runtime.get_ty(i)
                        ))
                        .into();
                    };

                    let i = i.get_int()?;

                    let el = (match i {
                        i if i >= 0 => items.get(i as usize).cloned(),
                        i if items.len() as i64 + i >= 0 => {
                            items.get((items.len() as i64 + i) as usize).cloned()
                        }
                        _ => None,
                    })
                    .map(|v| (v, false))
                    .unwrap_or(runtime.new_value(Value::Nil));

                    Ok(el)
                }),
            },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("list"), Some(Type::Tuple)),
                    idpat("i"),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.get_scope(scope).get_unchecked("list");

                    let Value::Tuple(list) = runtime.get_value(list) else {
                        return RuntimeError(format!(
                            "index() list must be a tuple, is a: {}",
                            runtime.get_ty(list)
                        ))
                        .into();
                    };

                    let i = runtime.get_scope(scope).get_unchecked("i");

                    let Value::Numeric(i) = runtime.get_value(i) else {
                        return RuntimeError(format!(
                            "index() i must be an int, is a: {}",
                            runtime.get_ty(i)
                        ))
                        .into();
                    };

                    let i = i.get_int()?;

                    let el = (match i {
                        i if i >= 0 => list.get(i as usize).cloned(),
                        i if list.len() as i64 + i >= 0 => {
                            list.get((list.len() as i64 + i) as usize).cloned()
                        }
                        _ => None,
                    })
                    .map(|v| (v, false))
                    .unwrap_or(runtime.new_value(Value::Nil));

                    Ok(el)
                }),
            },
            FnSig {
                params: vec![DeclarePattern::Id(id("text"), Some(Type::Str)), idpat("i")],
                body: FnBody::Builtin(|runtime, scope| {
                    let text = runtime.get_scope(scope).get_unchecked("text");

                    let Value::Str(text) = runtime.get_value(text) else {
                        return RuntimeError(format!(
                            "index() list must be a str, is a: {}",
                            runtime.get_value(text)
                        ))
                        .into();
                    };

                    let i = runtime.get_scope(scope).get_unchecked("i");

                    let Value::Numeric(i) = runtime.get_value(i) else {
                        return RuntimeError(format!(
                            "index() i must be an int, is a: {}",
                            runtime.get_ty(i)
                        ))
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

                    Ok(runtime.new_value(result))
                }),
            },
        ],
    );

    runtime.builtin(
        "clone",
        [FnSig {
            params: vec![idpat("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.get_scope(scope).get_unchecked("data");

                Ok(runtime.clone(data))
            }),
        }],
    );

    runtime.builtin(
        "trim",
        [FnSig {
            params: vec![idpat("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text) else {
                    return RuntimeError(format!(
                        "trim[#1] must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                Ok(runtime.new_value(Value::Str(text.substr_from(text.trim()))))
            }),
        }],
    );

    runtime.builtin(
        "len",
        [FnSig {
            params: vec![idpat("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.get_scope(scope).get_unchecked("data");

                let len = match runtime.get_value(data) {
                    Value::Str(text) => text.len(),
                    Value::List(_, list) => list.len(),
                    Value::Tuple(tuple) => tuple.len(),
                    _ => {
                        return RuntimeError(format!(
                            "cannot get len of: {}",
                            runtime.get_ty(data)
                        ))
                        .into();
                    }
                };

                Ok(runtime.new_value(Value::Numeric(Numeric::Int(len as i64))))
            }),
        }],
    );

    runtime.builtin(
        "chars",
        [FnSig {
            params: vec![idpat("text")],
            body: FnBody::Builtin(|runtime, scope| {
                let text = runtime.get_scope(scope).get_unchecked("text");

                let Value::Str(text) = runtime.get_value(text).clone() else {
                    return RuntimeError(format!(
                        "trim[#1] must be a string, is a: {}",
                        runtime.get_ty(text)
                    ))
                    .into();
                };

                let items = text
                    .chars()
                    .map(|c| runtime.new_value(Value::Str(c.to_string().into())).0)
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::Str, items)))
            }),
        }],
    );

    runtime.builtin(
        "assert",
        [FnSig {
            params: vec![idpat("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.get_scope(scope).get_unchecked("data");

                if !runtime.get_value(data).truthy()? {
                    return RuntimeError(format!("assertion failed")).into();
                }

                Ok(runtime.new_value(Value::Nil))
            }),
        }],
    );

    runtime.builtin(
        "int",
        [FnSig {
            params: vec![idpat("data")],
            body: FnBody::Builtin(|runtime, scope| {
                let data = runtime.get_scope(scope).get_unchecked("data");

                let result = runtime.get_value(data).auto_coerce_int()?;

                Ok(runtime.new_value(Value::Numeric(Numeric::Int(result))))
            }),
        }],
    );

    runtime.builtin(
        "sqrt",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "sqrt() num should be a num, is a: {}",
                        runtime.get_ty(num)
                    ))
                    .into();
                };

                Ok(runtime.new_value(Value::Numeric(Numeric::Double(num.get_double().sqrt()))))
            }),
        }],
    );

    runtime.builtin(
        "ceil",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "ceil() num should be a num, is a: {}",
                        runtime.get_ty(num)
                    ))
                    .into();
                };

                Ok(runtime.new_value(Value::Numeric(Numeric::Double(num.get_double().ceil()))))
            }),
        }],
    );

    runtime.builtin(
        "floor",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "floor() num should be a num, is a: {}",
                        runtime.get_ty(num)
                    ))
                    .into();
                };

                Ok(runtime.new_value(Value::Numeric(Numeric::Double(num.get_double().floor()))))
            }),
        }],
    );

    runtime.builtin(
        "abs",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "abs() num should be a num, is a: {}",
                        runtime.get_ty(num)
                    ))
                    .into();
                };

                match num {
                    Numeric::Int(n) => Ok(runtime.new_value(Value::Numeric(Numeric::Int(n.abs())))),
                    Numeric::Double(d) => {
                        Ok(runtime.new_value(Value::Numeric(Numeric::Double(d.abs()))))
                    }
                }
            }),
        }],
    );

    runtime.builtin(
        "round",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "abs() num should be a num, is a: {}",
                        runtime.get_ty(num)
                    ))
                    .into();
                };

                match num {
                    Numeric::Int(n) => Ok(runtime.new_value(Value::Numeric(Numeric::Int(*n)))),
                    Numeric::Double(d) => {
                        Ok(runtime.new_value(Value::Numeric(Numeric::Int(d.round() as i64))))
                    }
                }
            }),
        }],
    );
}
