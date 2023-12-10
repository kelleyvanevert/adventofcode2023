use crate::{
    ast::{DeclarePattern, Identifier, Type},
    runtime::{bool, nil, FnBody, FnSig, Runtime, Value},
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

                Ok(nil())
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

    // runtime.builtin(
    //     "min",
    //     [
    //         FnSig {
    //             params: vec![idpat_ty("items", Type::List(Type::Any.into()))],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let items = runtime.get_scope(scope).get_unchecked("items");

    //                 match items {
    //                     Value::List(_, list) => {
    //                         if list.len() == 0 {
    //                             return Ok(Value::Nil);
    //                         }

    //                         match list.iter().min().cloned() {
    //                             Some(result) => Ok(result),
    //                             None => RuntimeError(
    //                                 "error getting min: could not compare all elements".into(),
    //                             )
    //                             .into(),
    //                         }
    //                     }
    //                     _ => RuntimeError(format!("cannot get min of: {}", items.ty())).into(),
    //                 }
    //             }),
    //         },
    //         FnSig {
    //             params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let a = runtime.get_scope(scope).get_unchecked("a");
    //                 let b = runtime.get_scope(scope).get_unchecked("b");

    //                 match (a, b) {
    //                     (&Value::Nil, b) => Ok(b.clone()),
    //                     (a, &Value::Nil) => Ok(a.clone()),
    //                     _ => Ok(a.min(b).clone()),
    //                 }
    //             }),
    //         },
    //     ],
    // );

    // runtime.builtin(
    //     "max",
    //     [
    //         FnSig {
    //             params: vec![idpat_ty("items", Type::List(Type::Any.into()))],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let items = runtime.get_scope(scope).get_unchecked("items");

    //                 match items {
    //                     Value::List(_, list) => {
    //                         if list.len() == 0 {
    //                             return Ok(Value::Nil);
    //                         }

    //                         match list.iter().max().cloned() {
    //                             Some(result) => Ok(result),
    //                             None => RuntimeError(
    //                                 "error getting max: could not compare all elements".into(),
    //                             )
    //                             .into(),
    //                         }
    //                     }
    //                     _ => RuntimeError(format!("cannot get max of: {}", items.ty())).into(),
    //                 }
    //             }),
    //         },
    //         FnSig {
    //             params: vec![idpat_ty("a", Type::Any), idpat_ty("b", Type::Any)],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let a = runtime.get_scope(scope).get_unchecked("a");
    //                 let b = runtime.get_scope(scope).get_unchecked("b");

    //                 match (a, b) {
    //                     (&Value::Nil, b) => Ok(b.clone()),
    //                     (a, &Value::Nil) => Ok(a.clone()),
    //                     _ => Ok(a.max(b).clone()),
    //                 }
    //             }),
    //         },
    //     ],
    // );

    // runtime.builtin(
    //     "chunks",
    //     [FnSig {
    //         params: vec![
    //             DeclarePattern::Id("items".into(), Some(Type::List(Type::Any.into()))),
    //             DeclarePattern::Id("size".into(), Some(Type::Numeric)),
    //         ],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             let Value::List(t, list) = items else {
    //                 return RuntimeError(format!("cannot get chunks of: {}", items.ty())).into();
    //             };

    //             let size = runtime.get_scope(scope).get_unchecked("size");

    //             let Value::Numeric(Numeric::Int(size)) = size else {
    //                 return RuntimeError(format!(
    //                     "chunks() size must be int >= 1, is a: {}",
    //                     size.ty()
    //                 ))
    //                 .into();
    //             };

    //             if size < &1 {
    //                 return RuntimeError(format!("chunks() size must be int >= 1, is: {}", size))
    //                     .into();
    //             }

    //             Ok(Value::List(
    //                 Type::List(t.clone().into()),
    //                 list.chunks_exact(*size as usize)
    //                     .map(|chunk| Value::List(t.clone(), chunk.to_vec()))
    //                     .collect::<Vec<_>>(),
    //             ))
    //         }),
    //     }],
    // );

    // // holy fuck so many clones..
    // // :|
    // // I can do better!
    // runtime.builtin(
    //     "sort_by_key",
    //     [FnSig {
    //         params: vec![idpat("items"), idpat("cb")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             let Value::List(t, list) = items.clone() else {
    //                 return RuntimeError(format!(
    //                     "sort_by_key() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let cb = runtime.get_scope(scope).get_unchecked("cb");

    //             let Value::FnDef(def) = cb else {
    //                 return RuntimeError(format!(
    //                     "sort_by_key() cb must be a fn, is a: {}",
    //                     cb.ty()
    //                 ))
    //                 .into();
    //             };

    //             let def = def.clone();

    //             let mut sorting_keys = Vec::with_capacity(list.len());

    //             for (i, item) in list.clone().into_iter().enumerate() {
    //                 let key = runtime.invoke(def.clone(), vec![(None, item)])?;
    //                 sorting_keys.push((i, key));
    //             }

    //             sorting_keys.sort_by_cached_key(|t| t.1.clone());

    //             let mut result = vec![Value::Nil; list.len()];

    //             for (dest, (source, _)) in sorting_keys.iter().enumerate() {
    //                 result[dest] = list[*source].clone();
    //             }

    //             Ok(Value::List(t.clone(), result))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "reverse",
    //     [FnSig {
    //         params: vec![idpat("items")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             match items.clone() {
    //                 Value::List(t, mut list) => {
    //                     list.reverse();
    //                     Ok(Value::List(t, list))
    //                 }
    //                 Value::Tuple(mut list) => {
    //                     list.reverse();
    //                     Ok(Value::Tuple(list))
    //                 }
    //                 _ => {
    //                     return RuntimeError(format!(
    //                         "reverse() items must be a list or tuple, is a: {}",
    //                         items.ty()
    //                     ))
    //                     .into();
    //                 }
    //             }
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "zip",
    //     [FnSig {
    //         params: vec![idpat("xs"), idpat("ys")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let xs = runtime.get_scope(scope).get_unchecked("xs");
    //             let ys = runtime.get_scope(scope).get_unchecked("ys");

    //             match (&xs, &ys) {
    //                 (Value::List(_, x_els), Value::List(_, y_els)) => Ok(Value::List(
    //                     Type::Tuple,
    //                     x_els
    //                         .into_iter()
    //                         .zip(y_els.into_iter())
    //                         .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
    //                         .collect(),
    //                 )),
    //                 (Value::Tuple(x_els), Value::Tuple(y_els)) => Ok(Value::Tuple(
    //                     x_els
    //                         .into_iter()
    //                         .zip(y_els.into_iter())
    //                         .map(|(x, y)| Value::Tuple(vec![x.clone(), y.clone()]))
    //                         .collect(),
    //                 )),
    //                 _ => {
    //                     return RuntimeError(format!(
    //                         "cannot apply zip to these types: {}, {}",
    //                         xs.ty(),
    //                         ys.ty()
    //                     ))
    //                     .into()
    //                 }
    //             }
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "fold",
    //     [FnSig {
    //         params: vec![idpat("items"), idpat("init"), idpat("cb")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");
    //             let cb = runtime.get_scope(scope).get_unchecked("cb");
    //             let init = runtime.get_scope(scope).get_unchecked("init");

    //             let Value::FnDef(def) = cb else {
    //                 return RuntimeError(format!("fold() cb must be a fn, is a: {}", cb.ty()))
    //                     .into();
    //             };

    //             let def = def.clone();

    //             match items {
    //                 Value::List(_, els) => {
    //                     Ok(els.clone().into_iter().try_fold(init.clone(), |acc, el| {
    //                         runtime.invoke(def.clone(), vec![(None, acc), (None, el.clone())])
    //                     })?)
    //                 }
    //                 Value::Tuple(els) => {
    //                     Ok(els.clone().into_iter().try_fold(init.clone(), |acc, el| {
    //                         runtime.invoke(def.clone(), vec![(None, acc), (None, el.clone())])
    //                     })?)
    //                 }
    //                 _ => {
    //                     return RuntimeError(format!(
    //                         "cannot apply fold to these types: {}, {}, {}",
    //                         items.ty(),
    //                         cb.ty(),
    //                         init.ty()
    //                     ))
    //                     .into()
    //                 }
    //             }
    //         }),
    //     }],
    // );

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
                    result.push(runtime.invoke(cb, vec![(None, item.clone())])?);
                }

                Ok(runtime.new_value(Value::List(Type::Any, result)))
            }),
        }],
    );

    // runtime.builtin(
    //     "flat_map",
    //     [FnSig {
    //         params: vec![idpat("items"), idpat("cb")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             let Value::List(_, list) = items else {
    //                 return RuntimeError(format!("cannot get max of: {}", items.ty())).into();
    //             };

    //             let list = list.clone();

    //             let cb = runtime.get_scope(scope).get_unchecked("cb");

    //             let Value::FnDef(def) = cb else {
    //                 return RuntimeError(format!("cannot use map w/ cb of type: {}", cb.ty()))
    //                     .into();
    //             };

    //             let def = def.clone();

    //             let mut result = vec![];
    //             for item in list.iter() {
    //                 let value = runtime.invoke(def.clone(), vec![(None, item.clone())])?;
    //                 let Value::List(_, items) = value else {
    //                     return RuntimeError(format!(
    //                         "flat_map cb should return lists, returned: {}",
    //                         value.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 // TODO type-check

    //                 result.extend(items);
    //             }

    //             // TODO type
    //             Ok(Value::List(Type::Any, result))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "dict",
    //     [FnSig {
    //         params: vec![DeclarePattern::Id(
    //             id("pairs"),
    //             Some(Type::List(Type::Any.into())),
    //         )],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let pairs = runtime.get_scope(scope).get_unchecked("pairs");

    //             let Value::List(_, pairs) = pairs.clone() else {
    //                 return RuntimeError(format!(
    //                     "dict() pairs must be list of tuples, is a: {}",
    //                     pairs.ty()
    //                 ))
    //                 .into();
    //             };

    //             let mut dict = Dict::new();

    //             for pair in pairs {
    //                 let Value::Tuple(elements) = pair else {
    //                     return RuntimeError(format!(
    //                         "each dict() pair must be a tuple, is a: {}",
    //                         pair.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let mut elements = elements.into_iter();

    //                 let Some(key) = elements.next() else {
    //                     return RuntimeError(format!("dict() pair without key")).into();
    //                 };

    //                 let Some(value) = elements.next() else {
    //                     return RuntimeError(format!("dict() pair without key")).into();
    //                 };

    //                 dict.0.insert(key, value);
    //             }

    //             Ok(Value::Dict(dict))
    //         }),
    //     }],
    // );

    runtime.builtin(
        "in",
        [FnSig {
            params: vec![idpat("needle"), idpat("haystack")],
            body: FnBody::Builtin(|runtime, scope| {
                let needle = runtime.get_scope(scope).get_unchecked("needle");
                let haystack = runtime.get_scope(scope).get_unchecked("haystack");

                let Value::List(_, haystack) = runtime.get_value(haystack).clone() else {
                    return RuntimeError(format!(
                        "cannot get max of: {}",
                        runtime.get_ty(haystack)
                    ))
                    .into();
                };

                for el in haystack {
                    if runtime.get_value(el) == runtime.get_value(needle) {
                        return Ok(bool(true));
                    }
                }

                Ok(bool(false))
            }),
        }],
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
                    if runtime.get_value(r).truthy()? {
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

                    match runtime.get_value(item) {
                        // TODO fix the "nil as well as unit" problem
                        Value::Nil => {}
                        _ => {
                            result.push(item);
                        }
                    }
                }

                // TODO
                Ok(runtime.new_value(Value::List(Type::Any, result)))
            }),
        }],
    );

    // runtime.builtin(
    //     "any",
    //     [FnSig {
    //         params: vec![idpat("items"), idpat("cb")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             let Value::List(_, list) = items else {
    //                 return RuntimeError(format!(
    //                     "any() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let list = list.clone();

    //             let cb = runtime.get_scope(scope).get_unchecked("cb");

    //             let Value::FnDef(def) = cb else {
    //                 return RuntimeError(format!("cannot use any() w/ cb of type: {}", cb.ty()))
    //                     .into();
    //             };

    //             let def = def.clone();

    //             for item in list {
    //                 let item = runtime.invoke(def.clone(), vec![(None, item)])?;
    //                 if item.truthy()? {
    //                     return Ok(Value::Bool(true));
    //                 }
    //             }

    //             Ok(Value::Bool(false))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "all",
    //     [
    //         FnSig {
    //             params: vec![idpat("items"), idpat("cb")],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let items = runtime.get_scope(scope).get_unchecked("items");

    //                 let Value::List(_, list) = items else {
    //                     return RuntimeError(format!(
    //                         "all() items must be a list, is a: {}",
    //                         items.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let list = list.clone();

    //                 let cb = runtime.get_scope(scope).get_unchecked("cb");

    //                 let Value::FnDef(def) = cb else {
    //                     return RuntimeError(format!(
    //                         "cannot use all() w/ cb of type: {}",
    //                         cb.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let def = def.clone();

    //                 for item in list {
    //                     let item = runtime.invoke(def.clone(), vec![(None, item)])?;
    //                     if !item.truthy()? {
    //                         return Ok(Value::Bool(false));
    //                     }
    //                 }

    //                 Ok(Value::Bool(true))
    //             }),
    //         },
    //         FnSig {
    //             params: vec![idpat("items")],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let items = runtime.get_scope(scope).get_unchecked("items");

    //                 let Value::List(_, list) = items else {
    //                     return RuntimeError(format!(
    //                         "all() items must be a list, is a: {}",
    //                         items.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let list = list.clone();

    //                 for item in list {
    //                     if !item.truthy()? {
    //                         return Ok(Value::Bool(false));
    //                     }
    //                 }

    //                 Ok(Value::Bool(true))
    //             }),
    //         },
    //     ],
    // );

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
                    if runtime.get_value(item).truthy()? {
                        return Ok(item);
                    }
                }

                Ok(nil())
            }),
        }],
    );

    // runtime.builtin(
    //     "find",
    //     [FnSig {
    //         params: vec![idpat("items"), idpat("cb")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             let Value::List(_, list) = items else {
    //                 return RuntimeError(format!("cannot get max of: {}", items.ty())).into();
    //             };

    //             let list = list.clone();

    //             let cb = runtime.get_scope(scope).get_unchecked("cb");

    //             let Value::FnDef(def) = cb else {
    //                 return RuntimeError(format!("cannot use filter w/ cb of type: {}", cb.ty()))
    //                     .into();
    //             };

    //             let def = def.clone();

    //             for item in list {
    //                 let check = runtime.invoke(def.clone(), vec![(None, item.clone())])?;
    //                 if check.truthy()? {
    //                     return Ok(item);
    //                 }
    //             }

    //             Ok(Value::Nil)
    //         }),
    //     }],
    // );

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
                        .map(|n| runtime.new_value(Value::Numeric(Numeric::Int(n))))
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

    // runtime.builtin(
    //     "enumerate",
    //     [FnSig {
    //         params: vec![idpat("items")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let items = runtime.get_scope(scope).get_unchecked("items");

    //             Ok(match items {
    //                 Value::List(_, list) => Value::List(
    //                     Type::Tuple, // TODO generic tuple types
    //                     list.iter()
    //                         .cloned()
    //                         .enumerate()
    //                         .map(|(i, item)| {
    //                             Value::Tuple(vec![Value::Numeric(Numeric::Int(i as i64)), item])
    //                         })
    //                         .collect::<Vec<_>>(),
    //                 ),
    //                 Value::Tuple(list) => Value::Tuple(
    //                     list.iter()
    //                         .cloned()
    //                         .enumerate()
    //                         .map(|(i, item)| {
    //                             Value::Tuple(vec![Value::Numeric(Numeric::Int(i as i64)), item])
    //                         })
    //                         .collect::<Vec<_>>(),
    //                 ),
    //                 _ => {
    //                     return RuntimeError(format!("cannot get max of: {}", items.ty())).into();
    //                 }
    //             })
    //         }),
    //     }],
    // );

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

    // runtime.builtin(
    //     "split",
    //     [
    //         FnSig {
    //             params: vec![
    //                 DeclarePattern::Id("text".into(), Some(Type::Str)),
    //                 DeclarePattern::Id("sep".into(), Some(Type::Str)),
    //             ],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let text = runtime.get_scope(scope).get_unchecked("text");

    //                 let Value::Str(text) = text else {
    //                     return RuntimeError(format!(
    //                         "split() text must be a string, is a: {}",
    //                         text.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let sep = runtime.get_scope(scope).get_unchecked("sep");

    //                 let Value::Str(sep) = sep else {
    //                     return RuntimeError(format!(
    //                         "split() sep must be a string, is a: {}",
    //                         sep.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let result = text
    //                     .split(sep.as_str())
    //                     .map(|piece| Value::Str(text.substr_from(piece)))
    //                     .collect::<Vec<_>>();

    //                 Ok(Value::List(Type::Str, result))
    //             }),
    //         },
    //         FnSig {
    //             params: vec![
    //                 DeclarePattern::Id("text".into(), Some(Type::Str)),
    //                 DeclarePattern::Id("sep".into(), Some(Type::Regex)),
    //             ],
    //             body: FnBody::Builtin(|runtime, scope| {
    //                 let text = runtime.get_scope(scope).get_unchecked("text");

    //                 let Value::Str(text) = text else {
    //                     return RuntimeError(format!(
    //                         "split() text must be a string, is a: {}",
    //                         text.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let sep = runtime.get_scope(scope).get_unchecked("sep");

    //                 let Value::Regex(sep) = sep else {
    //                     return RuntimeError(format!(
    //                         "split() setp must be a regex, is a: {}",
    //                         sep.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let result = sep
    //                     .0
    //                     .split(&text)
    //                     .map(|piece| Value::Str(text.substr_from(piece)))
    //                     .collect::<Vec<_>>();

    //                 Ok(Value::List(Type::Str, result))
    //             }),
    //         },
    //     ],
    // );

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
                    .map(|line| runtime.new_value(Value::Str(text.substr_from(line))))
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::Str, result)))
            }),
        }],
    );

    // runtime.builtin(
    //     "match",
    //     [FnSig {
    //         params: vec![idpat("text"), idpat("regex")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let text = runtime.get_scope(scope).get_unchecked("text");

    //             let Value::Str(text) = text else {
    //                 return RuntimeError(format!(
    //                     "match() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let regex = runtime.get_scope(scope).get_unchecked("regex");

    //             let Value::Regex(regex) = regex else {
    //                 return RuntimeError(format!(
    //                     "match() regex must be a regex, is a: {}",
    //                     regex.ty()
    //                 ))
    //                 .into();
    //             };

    //             match regex.0.captures(text) {
    //                 Some(cap) => {
    //                     let m = cap.get(0).unwrap();
    //                     Ok(Value::Tuple(vec![
    //                         Value::Str(m.as_str().into()),
    //                         Value::Numeric(Numeric::Int(m.start() as i64)),
    //                     ]))
    //                 }
    //                 None => Ok(Value::Nil),
    //             }
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "match_all",
    //     [FnSig {
    //         params: vec![idpat("text"), idpat("regex")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let text = runtime.get_scope(scope).get_unchecked("text");

    //             let Value::Str(text) = text else {
    //                 return RuntimeError(format!(
    //                     "match() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let regex = runtime.get_scope(scope).get_unchecked("regex");

    //             let Value::Regex(regex) = regex else {
    //                 return RuntimeError(format!(
    //                     "match() regex must be a regex, is a: {}",
    //                     regex.ty()
    //                 ))
    //                 .into();
    //             };

    //             Ok(Value::List(
    //                 Type::Tuple,
    //                 regex
    //                     .0
    //                     .captures_iter(text)
    //                     .map(|cap| {
    //                         let m = cap.get(0).unwrap();
    //                         Value::Tuple(vec![
    //                             Value::Str(m.as_str().into()),
    //                             Value::Numeric(Numeric::Int(m.start() as i64)),
    //                         ])
    //                     })
    //                     .collect(),
    //             ))
    //         }),
    //     }],
    // );

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

                Ok(bool(text.starts_with(substr.as_str())))
            }),
        }],
    );

    // runtime.builtin(
    //     "replace",
    //     [FnSig {
    //         params: vec![idpat("text"), idpat("def")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let text = runtime.get_scope(scope).get_unchecked("text");

    //             let Value::Str(text) = text else {
    //                 return RuntimeError(format!(
    //                     "replace() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let def = runtime.get_scope(scope).get_unchecked("def");

    //             let Value::Tuple(def) = def else {
    //                 return RuntimeError(format!(
    //                     "replace() def must be a tuple, is a: {}",
    //                     def.ty()
    //                 ))
    //                 .into();
    //             };

    //             if def.len() != 2 {
    //                 return RuntimeError(format!(
    //                     "replace() def must be a tuple with two elements"
    //                 ))
    //                 .into();
    //             }

    //             let replace = def.get(1).unwrap();

    //             let Value::Str(replace) = replace else {
    //                 return RuntimeError(format!("replace() def[1] must be a string")).into();
    //             };

    //             let find = def.get(0).unwrap();

    //             match find {
    //                 Value::Str(find) => Ok(Value::Str(text.replace(find.as_str(), replace).into())),
    //                 Value::Regex(find) => Ok(Value::Str(
    //                     find.0.replace_all(&text, replace.to_string()).into(),
    //                 )),
    //                 _ => {
    //                     return RuntimeError(format!(
    //                         "replace() def[0] must be a string or regex, is a: {}",
    //                         find.ty()
    //                     ))
    //                     .into();
    //                 }
    //             }
    //         }),
    //     }],
    // );

    runtime.builtin(
        "slice",
        [
            // FnSig {
            //     params: vec![
            //         DeclarePattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
            //         DeclarePattern::Id(id("i"), Some(Type::Numeric)),
            //     ],
            //     body: FnBody::Builtin(|runtime, scope| {
            //         let list = runtime.get_scope(scope).get_unchecked("list");

            //         let Value::List(t, list) = list else {
            //             return RuntimeError(format!(
            //                 "slice() list must be a list, is a: {}",
            //                 list.ty()
            //             ))
            //             .into();
            //         };

            //         let i = runtime.get_scope(scope).get_unchecked("i");

            //         let Value::Numeric(i) = i else {
            //             return RuntimeError(format!("slice() i must be an int, is a: {}", i.ty()))
            //                 .into();
            //         };

            //         let i = i.get_int()?;

            //         if i < 0 {
            //             return RuntimeError(format!("slice() i must be a positive int")).into();
            //         }

            //         Ok(Value::List(
            //             t.clone(),
            //             list.clone().split_off((i as usize).min(list.len())),
            //         ))
            //     }),
            // },
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

                    let i = i.get_int()?;

                    if i < 0 {
                        return RuntimeError(format!("slice() i must be a positive int")).into();
                    }

                    Ok(runtime.new_value(Value::Str(text.substr((i as usize)..))))
                }),
            },
            // FnSig {
            //     params: vec![
            //         DeclarePattern::Id(id("text"), Some(Type::Str)),
            //         DeclarePattern::Id(id("range"), Some(Type::Tuple)),
            //     ],
            //     body: FnBody::Builtin(|runtime, scope| {
            //         let text = runtime.get_scope(scope).get_unchecked("text");

            //         let Value::Str(text) = text else {
            //             return RuntimeError(format!(
            //                 "slice() text must be a string, is a: {}",
            //                 text.ty()
            //             ))
            //             .into();
            //         };

            //         let range = runtime.get_scope(scope).get_unchecked("range");

            //         let Value::Tuple(range) = range else {
            //             return RuntimeError(format!(
            //                 "slice() range must be an (int, int) range, is a: {}",
            //                 range.ty()
            //             ))
            //             .into();
            //         };

            //         let Some(Value::Numeric(start)) = range.get(0) else {
            //             return RuntimeError(format!("slice() range must be an (int, int) range"))
            //                 .into();
            //         };

            //         let start = start.get_int()?;

            //         if start < 0 {
            //             return RuntimeError(format!("slice() range must be an (int, int) range"))
            //                 .into();
            //         }

            //         let Some(Value::Numeric(end)) = range.get(1) else {
            //             return RuntimeError(format!("slice() range must be an (int, int) range"))
            //                 .into();
            //         };

            //         let end = end.get_int()?;

            //         if end < 0 {
            //             return RuntimeError(format!("slice() range must be an (int, int) range"))
            //                 .into();
            //         }

            //         Ok(Value::Str(
            //             text.substr((start as usize)..(end as usize).min(text.len())),
            //         ))
            //     }),
            // },
        ],
    );

    runtime.builtin(
        "index",
        [
            // FnSig {
            //     params: vec![
            //         DeclarePattern::Id(id("dict"), Some(Type::Dict)),
            //         idpat("key"),
            //     ],
            //     body: FnBody::Builtin(|runtime, scope| {
            //         let dict = runtime.get_scope(scope).get_unchecked("dict");

            //         let Value::Dict(dict) = dict else {
            //             return RuntimeError(format!(
            //                 "index() dict must be a dict, is a: {}",
            //                 dict.ty()
            //             ))
            //             .into();
            //         };

            //         let key = runtime.get_scope(scope).get_unchecked("key");

            //         let result = dict.0.get(key).cloned().unwrap_or(Value::Nil);

            //         Ok(result)
            //     }),
            // },
            FnSig {
                params: vec![
                    DeclarePattern::Id(id("list"), Some(Type::List(Type::Any.into()))),
                    idpat("i"),
                ],
                body: FnBody::Builtin(|runtime, scope| {
                    let list = runtime.get_scope(scope).get_unchecked("list");

                    let Value::List(_, list) = runtime.get_value(list).clone() else {
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

                    let el = match i {
                        i if i >= 0 => list.get(i as usize).cloned(),
                        i if list.len() as i64 + i >= 0 => {
                            list.get((list.len() as i64 + i) as usize).cloned()
                        }
                        _ => None,
                    };

                    Ok(el.unwrap_or(nil()))
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

                    let el = match i {
                        i if i >= 0 => list.get(i as usize).cloned(),
                        i if list.len() as i64 + i >= 0 => {
                            list.get((list.len() as i64 + i) as usize).cloned()
                        }
                        _ => None,
                    };

                    Ok(el.unwrap_or(nil()))
                }),
            },
            // FnSig {
            //     params: vec![DeclarePattern::Id(id("text"), Some(Type::Str)), idpat("i")],
            //     body: FnBody::Builtin(|runtime, scope| {
            //         let text = runtime.get_scope(scope).get_unchecked("text");

            //         let Value::Str(text) = text else {
            //             return RuntimeError(format!(
            //                 "index() list must be a str, is a: {}",
            //                 text.ty()
            //             ))
            //             .into();
            //         };

            //         let i = runtime.get_scope(scope).get_unchecked("i");

            //         let Value::Numeric(i) = i else {
            //             return RuntimeError(format!("index() i must be an int, is a: {}", i.ty()))
            //                 .into();
            //         };

            //         let i = i.get_int()?;

            //         let result = match i {
            //             i if i >= 0 => {
            //                 let i = i as usize;
            //                 text.get(i..(i + 1))
            //             }
            //             i if text.len() as i64 + i >= 0 => {
            //                 let i = (text.len() as i64 + i) as usize;
            //                 text.get(i..(i + 1))
            //             }
            //             _ => None,
            //         }
            //         .map(|substr| Value::Str(text.substr_from(substr)))
            //         .unwrap_or(Value::Nil);

            //         Ok(result)
            //     }),
            // },
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

    // runtime.builtin(
    //     "trim",
    //     [FnSig {
    //         params: vec![idpat("text")],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let text = runtime.get_scope(scope).get_unchecked("text");

    //             let Value::Str(text) = text else {
    //                 return RuntimeError(format!("trim[#1] must be a string, is a: {}", text.ty()))
    //                     .into();
    //             };

    //             // TODO // substr_from
    //             Ok(Value::Str(text.trim().to_string().into()))
    //         }),
    //     }],
    // );

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
                    .map(|c| runtime.new_value(Value::Str(c.to_string().into())))
                    .collect::<Vec<_>>();

                Ok(runtime.new_value(Value::List(Type::Str, items)))
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

    // runtime.builtin(
    //     "sqrt",
    //     [FnSig {
    //         params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let num = runtime.get_scope(scope).get_unchecked("num");

    //             let Value::Numeric(num) = num else {
    //                 return RuntimeError(format!("sqrt() num should be a num, is a: {}", num.ty()))
    //                     .into();
    //             };

    //             Ok(Value::Numeric(Numeric::Double(num.get_double().sqrt())))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "ceil",
    //     [FnSig {
    //         params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let num = runtime.get_scope(scope).get_unchecked("num");

    //             let Value::Numeric(num) = num else {
    //                 return RuntimeError(format!("ceil() num should be a num, is a: {}", num.ty()))
    //                     .into();
    //             };

    //             Ok(Value::Numeric(Numeric::Double(num.get_double().ceil())))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "floor",
    //     [FnSig {
    //         params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let num = runtime.get_scope(scope).get_unchecked("num");

    //             let Value::Numeric(num) = num else {
    //                 return RuntimeError(format!(
    //                     "floor() num should be a num, is a: {}",
    //                     num.ty()
    //                 ))
    //                 .into();
    //             };

    //             Ok(Value::Numeric(Numeric::Double(num.get_double().floor())))
    //         }),
    //     }],
    // );

    // runtime.builtin(
    //     "abs",
    //     [FnSig {
    //         params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
    //         body: FnBody::Builtin(|runtime, scope| {
    //             let num = runtime.get_scope(scope).get_unchecked("num");

    //             let Value::Numeric(num) = num else {
    //                 return RuntimeError(format!("abs() num should be a num, is a: {}", num.ty()))
    //                     .into();
    //             };

    //             match num {
    //                 Numeric::Int(n) => Ok(Value::Numeric(Numeric::Int(n.abs()))),
    //                 Numeric::Double(d) => Ok(Value::Numeric(Numeric::Double(d.abs()))),
    //             }
    //         }),
    //     }],
    // );

    runtime.builtin(
        "round",
        [FnSig {
            params: vec![DeclarePattern::Id("num".into(), Some(Type::Numeric))],
            body: FnBody::Builtin(|runtime, scope| {
                let num = runtime.get_scope(scope).get_unchecked("num");

                let Value::Numeric(num) = runtime.get_value(num) else {
                    return RuntimeError(format!(
                        "abs() num should be a num, is a: {}",
                        runtime.get_value(num).ty()
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
