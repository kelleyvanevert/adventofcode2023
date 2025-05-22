use std::cmp::Ordering;

use arcstr::Substr;

use crate::{
    ast::Type,
    gc::Gc,
    parse::{parse_declarable, parse_type},
    runtime::{Dict, FnBody, FnSig, List, Runtime, Scope, Tuple, Value},
    runtime_builtins::RuntimeLike,
    value::{EvaluationResult, Numeric, RuntimeError},
};

fn signature(
    params: impl IntoIterator<Item = &'static str>,
    result: &'static str,
    body: fn(&mut Runtime, Gc<Scope>) -> EvaluationResult<Value>,
) -> FnSig {
    FnSig {
        params: params.into_iter().map(parse_declarable).collect(),
        body: FnBody::Builtin(body),
        result: parse_type(result),
    }
}

pub fn implement_stdlib<R: RuntimeLike>(runtime: &mut R) {
    runtime.builtin(
        "print",
        [signature(["text"], "nil", |runtime, scope| {
            let text = runtime.get_unchecked(scope, "text");

            println!("{}", runtime.display(text, true));
            Ok(Value::Nil)
        })],
    );

    runtime.builtin(
        "run",
        [signature(["f"], "any", |runtime, scope| {
            let f = runtime.get_unchecked(scope, "f").clone();

            Ok(runtime.invoke(&f, vec![])?)
        })],
    );

    runtime.builtin(
        "min",
        [
            signature(["items: [any]"], "num", |runtime, scope| {
                let items = runtime.get_unchecked(scope, "items");

                match items {
                    Value::List(list) => {
                        let elements = &runtime.heap[*list].elements;

                        if elements.len() == 0 {
                            return Ok(Value::Nil);
                        }

                        match elements.into_iter().min_by(|&a, &b| runtime.cmp(a, b)) {
                            Some(result) => Ok(result.clone()),
                            None => RuntimeError(
                                "error getting min: could not compare all elements".into(),
                            )
                            .into(),
                        }
                    }
                    _ => RuntimeError(format!("cannot get min of: {}", items.ty())).into(),
                }
            }),
            signature(["a", "b"], "num", |runtime, scope| {
                let a = runtime.get_unchecked(scope, "a");
                let b = runtime.get_unchecked(scope, "b");

                if a == &Value::Nil {
                    return Ok(b.clone());
                } else if b == &Value::Nil {
                    return Ok(a.clone());
                }

                match runtime.cmp(a, b) {
                    Ordering::Greater => Ok(b.clone()),
                    Ordering::Less => Ok(a.clone()),
                    _ => Ok(a.clone()),
                }
            }),
        ],
    );

    // runtime.builtin(
    //     "max",
    //     [
    //         signature(["items: [any]"], "any", |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             match items {
    //                 Value::List(_, list) => {
    //                     if list.len() == 0 {
    //                         return Ok(runtime.new_value(Value::Nil));
    //                     }

    //                     match list.iter().max_by(|&&a, &&b| runtime.cmp(a, b)) {
    //                         Some(result) => Ok((*result, false)),
    //                         None => RuntimeError(
    //                             "error getting max: could not compare all elements".into(),
    //                         )
    //                         .into(),
    //                     }
    //                 }
    //                 _ => {
    //                     RuntimeError(format!("cannot get max of: {}", items.ty())).into()
    //                 }
    //             }
    //         }),
    //         signature(["a", "b"], "any", |runtime, scope| {
    //             let a = runtime.get_unchecked(scope, "a");
    //             let b = runtime.get_unchecked(scope, "b");

    //             if a == &Value::Nil {
    //                 return Ok((b, false));
    //             } else if b == &Value::Nil {
    //                 return Ok((a, false));
    //             }

    //             match runtime.cmp(a, b) {
    //                 Ordering::Greater => Ok((a, false)),
    //                 Ordering::Less => Ok((b, false)),
    //                 _ => Ok((a, false)),
    //             }
    //         }),
    //     ],
    // );

    runtime.builtin(
        "add",
        [signature(["a", "b"], "any", |runtime, scope| {
            let a = runtime.get_unchecked(scope, "a");
            let b = runtime.get_unchecked(scope, "b");

            match (a.clone(), b.clone()) {
                (Value::Str(a), Value::Str(b)) => {
                    let new = Substr::from(a.to_string() + &b);
                    Ok(Value::Str(new))
                }
                (Value::Numeric(a), Value::Numeric(b)) => Ok(Value::Numeric(a.add(&b))),
                (Value::List(a), Value::List(b)) => {
                    let list = runtime
                        .heap
                        .alloc(List {
                            ty: Type::Any,
                            elements: runtime.heap[a]
                                .elements
                                .clone()
                                .into_iter()
                                .chain(runtime.heap[b].elements.clone())
                                .collect::<Vec<_>>(),
                        })
                        .unrooted();

                    Ok(Value::List(list))
                }
                _ => return RuntimeError(format!("can't perform {} + {}", a.ty(), b.ty())).into(),
            }
        })],
    );

    // runtime.builtin(
    //     "chunks",
    //     [signature(
    //         ["items: [any]", "size: int"],
    //         "any",
    //         |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(t, list) = items.clone() else {
    //                 return RuntimeError(format!(
    //                     "cannot get chunks of: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let size = runtime.get_unchecked(scope, "size");

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

    //             let chunks = list
    //                 .chunks_exact(*size as usize)
    //                 .map(|chunk| runtime.new_value(Value::List(t.clone(), chunk.to_vec())).0)
    //                 .collect::<Vec<_>>();

    //             Ok(runtime.new_value(Value::List(Type::List(t.clone().into()), chunks)))
    //         },
    //     )],
    // );

    // runtime.builtin(
    //     "windows",
    //     [signature(
    //         ["items: [any]", "size: int"],
    //         "any",
    //         |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(t, list) = items.clone() else {
    //                 return RuntimeError(format!(
    //                     "cannot get windows of: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let size = runtime.get_unchecked(scope, "size");

    //             let Value::Numeric(Numeric::Int(size)) = size else {
    //                 return RuntimeError(format!(
    //                     "windows() size must be int >= 1, is a: {}",
    //                     size.ty()
    //                 ))
    //                 .into();
    //             };

    //             if size < &1 {
    //                 return RuntimeError(format!("windows() size must be int >= 1, is: {}", size))
    //                     .into();
    //             }

    //             let windows = list
    //                 .windows(*size as usize)
    //                 .map(|window| runtime.new_value(Value::List(t.clone(), window.to_vec())).0)
    //                 .collect::<Vec<_>>();

    //             Ok(runtime.new_value(Value::List(Type::List(t.clone().into()), windows)))
    //         },
    //     )],
    // );

    // // holy fuck so many clones..
    // // :|
    // // surely I can do better!
    // runtime.builtin(
    //     "sort",
    //     [signature(["items"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(t, list) = items.clone() else {
    //             return RuntimeError(format!(
    //                 "sort_by_key() items must be a list, is a: {}",
    //                 items.ty()
    //             ))
    //             .into();
    //         };

    //         let mut sorted = list.clone().into_iter().enumerate().collect::<Vec<_>>();

    //         sorted.sort_by(|a, b| runtime.cmp(a.1, b.1));

    //         let mut result = list
    //             .iter()
    //             .map(|_| runtime.new_value(Value::Nil).0)
    //             .collect::<Vec<_>>();

    //         for (dest, (source, _)) in sorted.iter().enumerate() {
    //             result[dest] = list[*source].clone();
    //         }

    //         Ok(runtime.new_value(Value::List(t.clone(), result)))
    //     })],
    // );

    // // holy fuck so many clones..
    // // :|
    // // surely I can do better!
    // runtime.builtin(
    //     "sort_by_key",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(t, list) = items.clone() else {
    //             return RuntimeError(format!(
    //                 "sort_by_key() items must be a list, is a: {}",
    //                 items.ty()
    //             ))
    //             .into();
    //         };

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         let mut sorting_keys = Vec::with_capacity(list.len());

    //         for (i, item) in list.clone().into_iter().enumerate() {
    //             let key = runtime.invoke(cb, vec![(None, item)])?;
    //             sorting_keys.push((i, key.0));
    //         }

    //         sorting_keys.sort_by(|a, b| runtime.cmp(a.1, b.1));

    //         let mut result = list
    //             .iter()
    //             .map(|_| runtime.new_value(Value::Nil).0)
    //             .collect::<Vec<_>>();

    //         for (dest, (source, _)) in sorting_keys.iter().enumerate() {
    //             result[dest] = list[*source].clone();
    //         }

    //         Ok(runtime.new_value(Value::List(t.clone(), result)))
    //     })],
    // );

    // runtime.builtin(
    //     "reverse",
    //     [signature(["items"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");
    //         let items = runtime.clone(items);

    //         match items0).clone() {
    //             Value::List(t, mut list) => {
    //                 list.reverse();
    //                 Ok(runtime.new_value(Value::List(t, list)))
    //             }
    //             Value::Tuple(ts, mut list) => {
    //                 list.reverse();
    //                 Ok(runtime.new_value(Value::Tuple(ts, list)))
    //             }
    //             _ => {
    //                 return RuntimeError(format!(
    //                     "reverse() items must be a list or tuple, is a: {}",
    //                     items.ty()0)
    //                 ))
    //                 .into();
    //             }
    //         }
    //     })],
    // );

    // runtime.builtin(
    //     "remove_at",
    //     [signature(["items", "i: int"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");
    //         let i = runtime.get_unchecked(scope, "i");

    //         let Value::Numeric(i) = i else {
    //             return RuntimeError(format!(
    //                 "remove_at() i must be an int, is: {}",
    //                 i.ty()
    //             ))
    //             .into();
    //         };

    //         let i = i.get_int()?;

    //         if i < 0 {
    //             return RuntimeError(format!("remove_at() i must be a positive int, is: {}", i))
    //                 .into();
    //         }

    //         let i = i as usize;

    //         match items.clone() {
    //             Value::List(t, mut list) => {
    //                 if i >= list.len() {
    //                     return RuntimeError(format!(
    //                         "remove_at() i out of bounds, is: {}, len: {}",
    //                         i,
    //                         list.len(),
    //                     ))
    //                     .into();
    //                 }

    //                 let el = list.remove(i);
    //                 runtime.replace_value(items, Value::List(t, list));
    //                 Ok((el, false))
    //             }
    //             Value::Tuple(ts, mut list) => {
    //                 if i >= list.len() {
    //                     return RuntimeError(format!(
    //                         "remove_at() i out of bounds, is: {}, len: {}",
    //                         i,
    //                         list.len(),
    //                     ))
    //                     .into();
    //                 }

    //                 let el = list.remove(i);
    //                 runtime.replace_value(items, Value::Tuple(ts, list));
    //                 Ok((el, false))
    //             }
    //             _ => {
    //                 return RuntimeError(format!(
    //                     "remove_at() items must be a list or tuple, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             }
    //         }
    //     })],
    // );

    // runtime.builtin(
    //     "pop",
    //     [signature(["items"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         match items.clone() {
    //             Value::List(t, mut list) => match list.pop() {
    //                 Some(el) => {
    //                     runtime.replace_value(items, Value::List(t, list));
    //                     Ok((el, false))
    //                 }
    //                 None => Ok(runtime.new_value(Value::Nil)),
    //             },
    //             Value::Tuple(ts, mut list) => match list.pop() {
    //                 Some(el) => {
    //                     runtime.replace_value(items, Value::Tuple(ts, list));
    //                     Ok((el, false))
    //                 }
    //                 None => Ok(runtime.new_value(Value::Nil)),
    //             },
    //             _ => {
    //                 return RuntimeError(format!(
    //                     "pop() items must be a list or tuple, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             }
    //         }
    //     })],
    // );

    runtime.builtin(
        "zip",
        [signature(["xs", "ys"], "any", |runtime, scope| {
            let xs = runtime.get_unchecked(scope, "xs").clone();
            let ys = runtime.get_unchecked(scope, "ys").clone();

            match (&xs, &ys) {
                (Value::List(x), Value::List(y)) => {
                    let pair_ty = Some(vec![
                        runtime.heap[*x].ty.clone(),
                        runtime.heap[*y].ty.clone(),
                    ]);

                    let zipped_items = runtime.heap[*x]
                        .elements
                        .clone()
                        .into_iter()
                        .zip(runtime.heap[*y].elements.clone())
                        .map(|(x, y)| {
                            Value::Tuple(
                                runtime
                                    .heap
                                    .alloc(Tuple {
                                        ty: pair_ty.clone(),
                                        elements: vec![x, y],
                                    })
                                    .unrooted(),
                            )
                        })
                        .collect();

                    Ok(Value::List(
                        runtime
                            .heap
                            .alloc(List {
                                ty: Type::Tuple(pair_ty),
                                elements: zipped_items,
                            })
                            .unrooted(),
                    ))
                }
                (Value::Tuple(x), Value::Tuple(y)) => {
                    let zipped_items = runtime.heap[*x]
                        .elements
                        .clone()
                        .into_iter()
                        .zip(runtime.heap[*y].elements.clone())
                        .map(|(x, y)| {
                            Value::Tuple(
                                runtime
                                    .heap
                                    .alloc(Tuple {
                                        ty: None,
                                        elements: vec![x, y],
                                    })
                                    .unrooted(),
                            )
                        })
                        .collect();

                    Ok(Value::List(
                        runtime
                            .heap
                            .alloc(List {
                                ty: Type::Tuple(None),
                                elements: zipped_items,
                            })
                            .unrooted(),
                    ))
                }
                _ => {
                    return RuntimeError(format!(
                        "cannot apply zip to these types: {}, {}",
                        xs.ty(),
                        ys.ty(),
                    ))
                    .into()
                }
            }
        })],
    );

    runtime.builtin(
        "fold",
        [signature(
            ["items", "init", "cb"],
            "any",
            |runtime, scope| {
                let items = runtime.get_unchecked(scope, "items");
                let cb = runtime.get_unchecked(scope, "cb").clone();
                let init = runtime.get_unchecked(scope, "init").clone();

                match items.clone() {
                    Value::List(coll) => Ok(runtime.heap[coll]
                        .elements
                        .clone()
                        .into_iter()
                        .try_fold(init, |acc, el| {
                            runtime.invoke(&cb, vec![(None, acc), (None, el.clone())])
                        })?),
                    Value::Tuple(coll) => Ok(runtime.heap[coll]
                        .elements
                        .clone()
                        .into_iter()
                        .try_fold(init, |acc, el| {
                            runtime.invoke(&cb, vec![(None, acc), (None, el.clone())])
                        })?),
                    _ => {
                        return RuntimeError(format!(
                            "cannot apply fold to these types: {}, {}, {}",
                            items.ty(),
                            cb.ty(),
                            init.ty(),
                        ))
                        .into()
                    }
                }
            },
        )],
    );

    runtime.builtin(
        "map",
        [signature(["items", "cb"], "any", |runtime, scope| {
            let items = runtime.get_unchecked(scope, "items");

            let Value::List(list) = items else {
                return RuntimeError(format!("cannot get map of")).into();
            };

            let cb = runtime.get_unchecked(scope, "cb").clone();

            let mut result = vec![];
            for item in runtime.heap[*list].elements.clone().iter() {
                let el = runtime.invoke(&cb, vec![(None, item.clone())])?;
                result.push(el);
            }

            let new_list = runtime
                .heap
                .alloc(List {
                    ty: Type::Any,
                    elements: result,
                })
                .unrooted();

            Ok(Value::List(new_list))
        })],
    );

    // runtime.builtin(
    //     "flat_map",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items.clone() else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         let mut result = vec![];
    //         for item in list.into_iter() {
    //             let value = runtime.invoke(cb, vec![(None, item.clone())])?;
    //             let Value::List(_, items) = value0) else {
    //                 return RuntimeError(format!(
    //                     "flat_map cb should return lists, returned: {}",
    //                     value.ty()0)
    //                 ))
    //                 .into();
    //             };

    //             // TODO type-check

    //             result.extend(items);
    //         }

    //         // TODO type
    //         Ok(runtime.new_value(Value::List(Type::Any, result)))
    //     })],
    // );

    // runtime.builtin(
    //     "dict",
    //     [signature(["pairs: [any]"], "any", |runtime, scope| {
    //         let pairs = runtime.get_unchecked(scope, "pairs");

    //         let Value::List(_, pairs) = pairs.clone() else {
    //             return RuntimeError(format!(
    //                 "dict() pairs must be list of tuples, is a: {}",
    //                 pairs.ty()
    //             ))
    //             .into();
    //         };

    //         let mut dict = Dict::new();

    //         for pair in pairs {
    //             let Value::Tuple(_, elements) = pair else {
    //                 return RuntimeError(format!(
    //                     "each dict() pair must be a tuple, is a: {}",
    //                     pair.ty()
    //                 ))
    //                 .into();
    //             };

    //             let mut elements = elements.into_iter();

    //             let Some(key) = elements.next() else {
    //                 return RuntimeError(format!("dict() pair without key")).into();
    //             };

    //             let Some(value) = elements.next() else {
    //                 return RuntimeError(format!("dict() pair without key")).into();
    //             };

    //             dict.insert(runtime, *key, *value);
    //         }

    //         Ok(runtime.new_value(Value::Dict(None, dict)))
    //     })],
    // );

    // runtime.builtin(
    //     "in",
    //     [
    //         signature(["needle: str", "haystack: str"], "any", |runtime, scope| {
    //             let needle = runtime.get_unchecked(scope, "needle");
    //             let haystack = runtime.get_unchecked(scope, "haystack");

    //             let Value::Str(needle) = needle.clone() else {
    //                 return RuntimeError(format!(
    //                     "in() needle must be a str, is a: {}",
    //                     needle.ty()
    //                 ))
    //                 .into();
    //             };

    //             let Value::Str(haystack) = haystack.clone() else {
    //                 return RuntimeError(format!(
    //                     "in() haystack must be a str, is a: {}",
    //                     haystack.ty()
    //                 ))
    //                 .into();
    //             };

    //             Ok(runtime.new_value(Value::Bool(haystack.contains(&needle.as_str()))))
    //         }),
    //         signature(["needle", "haystack: [any]"], "any", |runtime, scope| {
    //             let needle = runtime.get_unchecked(scope, "needle");
    //             let haystack = runtime.get_unchecked(scope, "haystack");

    //             let Value::List(_, haystack) = haystack.clone() else {
    //                 return RuntimeError(format!(
    //                     "cannot get in() of: {}",
    //                     haystack.ty()
    //                 ))
    //                 .into();
    //             };

    //             for el in haystack {
    //                 if runtime.eq(el, needle) {
    //                     return Ok(runtime.new_value(Value::Bool(true)));
    //                 }
    //             }

    //             Ok(runtime.new_value(Value::Bool(false)))
    //         }),
    //     ],
    // );

    // runtime.builtin(
    //     "filter",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let list = list.clone();

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         let mut result = vec![];
    //         for item in list.iter() {
    //             let r = runtime.invoke(cb, vec![(None, item.clone())])?;
    //             if r0).truthy() {
    //                 result.push(*item);
    //             }
    //         }

    //         // TODO
    //         Ok(runtime.new_value(Value::List(Type::Any, result)))
    //     })],
    // );

    // runtime.builtin(
    //     "filter_map",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let list = list.clone();

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         let mut result = vec![];
    //         for item in list.iter() {
    //             let item = runtime.invoke(cb, vec![(None, item.clone())])?;

    //             match item0) {
    //                 // TODO fix the "nil as well as unit" problem
    //                 Value::Nil => {}
    //                 _ => {
    //                     result.push(item.0);
    //                 }
    //             }
    //         }

    //         // TODO
    //         Ok(runtime.new_value(Value::List(Type::Any, result)))
    //     })],
    // );

    runtime.builtin(
        "any",
        [
            signature(["items", "cb"], "any", |runtime, scope| {
                let items = runtime.get_unchecked(scope, "items");

                let Value::List(list) = items.clone() else {
                    return RuntimeError(format!(
                        "any() items must be a list, is a: {}",
                        items.ty()
                    ))
                    .into();
                };

                let cb = runtime.get_unchecked(scope, "cb").clone();

                for item in runtime.heap[list].elements.clone() {
                    let item = runtime.invoke(&cb, vec![(None, item)])?;
                    if item.truthy() {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
            }),
            signature(["items"], "any", |runtime, scope| {
                let items = runtime.get_unchecked(scope, "items");

                let Value::List(list) = items.clone() else {
                    return RuntimeError(format!(
                        "any() items must be a list, is a: {}",
                        items.ty()
                    ))
                    .into();
                };

                for item in &runtime.heap[list].elements {
                    if item.truthy() {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
            }),
        ],
    );

    // runtime.builtin(
    //     "all",
    //     [
    //         signature(["items", "cb"], "any", |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(_, list) = items else {
    //                 return RuntimeError(format!(
    //                     "all() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let list = list.clone();

    //             let cb = runtime.get_unchecked(scope, "cb");

    //             for item in list {
    //                 let item = runtime.invoke(cb, vec![(None, item)])?;
    //                 if !item0).truthy() {
    //                     return Ok(runtime.new_value(Value::Bool(false)));
    //                 }
    //             }

    //             Ok(runtime.new_value(Value::Bool(true)))
    //         }),
    //         signature(["items"], "any", |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(_, list) = items else {
    //                 return RuntimeError(format!(
    //                     "all() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let list = list.clone();

    //             for item in list {
    //                 if !item.truthy() {
    //                     return Ok(runtime.new_value(Value::Bool(false)));
    //                 }
    //             }

    //             Ok(runtime.new_value(Value::Bool(true)))
    //         }),
    //     ],
    // );

    // runtime.builtin(
    //     "find_map",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let list = list.clone();

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         for item in list {
    //             let item = runtime.invoke(cb, vec![(None, item)])?;
    //             if item0).truthy() {
    //                 return Ok(item);
    //             }
    //         }

    //         Ok(runtime.new_value(Value::Nil))
    //     })],
    // );

    // runtime.builtin(
    //     "first",
    //     [signature(["items"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get first of: {}", items.ty()))
    //                 .into();
    //         };

    //         match list.first() {
    //             Some(n) => Ok((*n, false)),
    //             None => Ok(runtime.new_value(Value::Nil)),
    //         }
    //     })],
    // );

    // runtime.builtin(
    //     "last",
    //     [signature(["items"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get first of: {}", items.ty()))
    //                 .into();
    //         };

    //         match list.last() {
    //             Some(n) => Ok((*n, false)),
    //             None => Ok(runtime.new_value(Value::Nil)),
    //         }
    //     })],
    // );

    // runtime.builtin(
    //     "find",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let list = list.clone();

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         for item in list {
    //             let check = runtime.invoke(cb, vec![(None, item.clone())])?;
    //             if check0).truthy() {
    //                 return Ok((item, false));
    //             }
    //         }

    //         Ok(runtime.new_value(Value::Nil))
    //     })],
    // );

    // runtime.builtin(
    //     "find_index",
    //     [signature(["items", "cb"], "any", |runtime, scope| {
    //         let items = runtime.get_unchecked(scope, "items");

    //         let Value::List(_, list) = items else {
    //             return RuntimeError(format!("cannot get max of: {}", items.ty()))
    //                 .into();
    //         };

    //         let list = list.clone();

    //         let cb = runtime.get_unchecked(scope, "cb");

    //         for (i, item) in list.into_iter().enumerate() {
    //             let check = runtime.invoke(cb, vec![(None, item.clone())])?;
    //             if check0).truthy() {
    //                 return Ok(runtime.new_value(Value::Numeric(Numeric::Int(i as i64))));
    //             }
    //         }

    //         Ok(runtime.new_value(Value::Nil))
    //     })],
    // );

    runtime.builtin(
        "range",
        [signature(["start", "end"], "any", |runtime, scope| {
            let start = runtime.get_unchecked(scope, "start");

            let Value::Numeric(start) = start else {
                return RuntimeError(format!("range() start must be int, is: {}", start.ty()))
                    .into();
            };

            let start = start.get_int()?;

            let end = runtime.get_unchecked(scope, "end");

            let Value::Numeric(end) = end else {
                return RuntimeError(format!("range() end must be int, is: {}", end.ty())).into();
            };

            let end = end.get_int()?;

            if end >= start {
                let elements = (start..end)
                    .map(|n| Value::Numeric(Numeric::Int(n)))
                    .collect();

                Ok(Value::List(
                    runtime
                        .heap
                        .alloc(List {
                            ty: Type::Numeric,
                            elements,
                        })
                        .unrooted(),
                ))
            } else {
                // TODO allow this conditionally or something?
                // Ok(Value::List(
                //     Type::Numeric,
                //     (0..(start - end))
                //         .map(|n| Value::Numeric(Numeric::Int(start - n)))
                //         .collect(),
                // ))

                Ok(Value::List(
                    runtime
                        .heap
                        .alloc(List {
                            ty: Type::Numeric,
                            elements: vec![],
                        })
                        .unrooted(),
                ))
            }
        })],
    );

    runtime.builtin(
        "enumerate",
        [signature(["items"], "any", |runtime, scope| {
            let items = runtime.get_unchecked(scope, "items");

            Ok(match items.clone() {
                Value::List(list) => {
                    let t = runtime.heap[list].ty.clone();

                    let new_items = runtime.heap[list]
                        .elements
                        .clone()
                        .into_iter()
                        .enumerate()
                        .map(|(i, item)| {
                            let index = Value::Numeric(Numeric::Int(i as i64));

                            Value::Tuple(
                                runtime
                                    .heap
                                    .alloc(Tuple {
                                        ty: Some(vec![Type::Numeric, t.clone()]),
                                        elements: vec![index, item],
                                    })
                                    .unrooted(),
                            )
                        })
                        .collect::<Vec<_>>();

                    Value::List(
                        runtime
                            .heap
                            .alloc(List {
                                ty: Type::Tuple(Some(vec![Type::Numeric, t.clone()])),
                                elements: new_items,
                            })
                            .unrooted(),
                    )
                }
                Value::Tuple(tuple) => {
                    let t = runtime.heap[tuple].ty.clone();

                    let new_items = runtime.heap[tuple]
                        .elements
                        .clone()
                        .into_iter()
                        .enumerate()
                        .map(|(i, item)| {
                            let index = Value::Numeric(Numeric::Int(i as i64));

                            Value::Tuple(
                                runtime
                                    .heap
                                    .alloc(Tuple {
                                        ty: Some(vec![Type::Numeric, Type::Tuple(t.clone())]),
                                        elements: vec![index, item],
                                    })
                                    .unrooted(),
                            )
                        })
                        .collect::<Vec<_>>();

                    Value::Tuple(
                        runtime
                            .heap
                            .alloc(Tuple {
                                ty: None,
                                elements: new_items,
                            })
                            .unrooted(),
                    )
                }
                _ => {
                    return RuntimeError(format!("cannot get max of: {}", items.ty())).into();
                }
            })
        })],
    );

    runtime.builtin(
        "sum",
        [signature(["items"], "any", |runtime, scope| {
            let items = runtime.get_unchecked(scope, "items");

            let elements = match items {
                Value::List(list) => &runtime.heap[*list].elements,
                Value::Tuple(tuple) => &runtime.heap[*tuple].elements,
                _ => {
                    return RuntimeError(format!("cannot get max of: {}", items.ty())).into();
                }
            };

            let mut result = Value::Numeric(Numeric::Int(0));
            for item in elements {
                result = result.add(item)?;
            }

            Ok(result)
        })],
    );

    runtime.builtin(
        "split",
        [
            signature(["text: str", "sep: str"], "any", |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text.clone() else {
                    return RuntimeError(format!(
                        "split() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let sep = runtime.get_unchecked(scope, "sep");

                let Value::Str(sep) = sep.clone() else {
                    return RuntimeError(format!(
                        "split() sep must be a string, is a: {}",
                        sep.ty()
                    ))
                    .into();
                };

                let result = text
                    .split(sep.as_str())
                    .map(|piece| Value::Str(text.substr_from(piece)))
                    .collect::<Vec<_>>();

                Ok(Value::List(
                    runtime
                        .heap
                        .alloc(List {
                            ty: Type::Str,
                            elements: result,
                        })
                        .unrooted(),
                ))
            }),
            signature(["text: str", ("sep: regex")], "any", |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text.clone() else {
                    return RuntimeError(format!(
                        "split() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let sep = runtime.get_unchecked(scope, "sep");

                let Value::Regex(sep) = sep.clone() else {
                    return RuntimeError(format!(
                        "split() setp must be a regex, is a: {}",
                        sep.ty()
                    ))
                    .into();
                };

                let result = sep
                    .0
                    .split(&text)
                    .map(|piece| Value::Str(text.substr_from(piece)))
                    .collect::<Vec<_>>();

                Ok(Value::List(
                    runtime
                        .heap
                        .alloc(List {
                            ty: Type::Str,
                            elements: result,
                        })
                        .unrooted(),
                ))
            }),
        ],
    );

    // runtime.builtin(
    //     "join",
    //     [signature(
    //         ["items: [any]", "glue: str"],
    //         "any",
    //         |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(_, items) = items.clone() else {
    //                 return RuntimeError(format!(
    //                     "join() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let glue = runtime.get_unchecked(scope, "glue");

    //             let Value::Str(glue) = glue.clone() else {
    //                 return RuntimeError(format!(
    //                     "join() glue must be a string, is a: {}",
    //                     glue.ty()
    //                 ))
    //                 .into();
    //             };

    //             let result = items
    //                 .into_iter()
    //                 .map(|v| v.auto_coerce_str())
    //                 .collect::<Vec<_>>()
    //                 .join(glue.as_str());

    //             Ok(runtime.new_value(Value::Str(result.into())))
    //         },
    //     )],
    // );

    // runtime.builtin(
    //     "insert",
    //     [signature(
    //         ["items: [any]", "index: num", "item"],
    //         "[any]",
    //         |runtime, scope| {
    //             let items = runtime.get_unchecked(scope, "items");

    //             let Value::List(_, mut items) = items.clone() else {
    //                 return RuntimeError(format!(
    //                     "insert() items must be a list, is a: {}",
    //                     items.ty()
    //                 ))
    //                 .into();
    //             };

    //             let index = runtime.get_unchecked(scope, "index");

    //             let Value::Numeric(Numeric::Int(index)) = index.clone() else {
    //                 return RuntimeError(format!(
    //                     "insert() index must be an int, is a: {}",
    //                     index.ty()
    //                 ))
    //                 .into();
    //             };

    //             if index < 0 {
    //                 return RuntimeError(format!(
    //                     "insert() index must be a positive int, is: {}",
    //                     index
    //                 ))
    //                 .into();
    //             }

    //             let item = runtime.get_unchecked(scope, "item");

    //             items.insert(index as usize, runtime.clone(item).0);

    //             Ok(runtime.new_value(Value::List(Type::Any, items)))
    //         },
    //     )],
    // );

    runtime.builtin(
        "lines",
        [signature(["text: str"], "any", |runtime, scope| {
            let text = runtime.get_unchecked(scope, "text");

            let Value::Str(text) = text.clone() else {
                return RuntimeError(format!(
                    "lines() text must be a string, is a: {}",
                    text.ty()
                ))
                .into();
            };

            let result = text
                .clone()
                .lines()
                .map(|line| Value::Str(text.substr_from(line)))
                .collect::<Vec<_>>();

            Ok(Value::List(
                runtime
                    .heap
                    .alloc(List {
                        ty: Type::Str,
                        elements: result,
                    })
                    .unrooted(),
            ))
        })],
    );

    runtime.builtin(
        "match",
        [signature(
            ["text: str", "regex: regex"],
            "any",
            |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text.clone() else {
                    return RuntimeError(format!(
                        "match() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let regex = runtime.get_unchecked(scope, "regex");

                let Value::Regex(regex) = regex.clone() else {
                    return RuntimeError(format!(
                        "match() regex must be a regex, is a: {}",
                        regex.ty()
                    ))
                    .into();
                };

                match regex.0.captures(&text) {
                    Some(cap) => {
                        let m = cap.get(0).unwrap();
                        let matched_part = Value::Str(m.as_str().into());
                        let offset = Value::Numeric(Numeric::Int(m.start() as i64));

                        Ok(Value::Tuple(
                            runtime
                                .heap
                                .alloc(Tuple {
                                    ty: Some(vec![Type::Str, Type::Numeric]),
                                    elements: vec![matched_part, offset],
                                })
                                .unrooted(),
                        ))
                    }
                    None => Ok(Value::Nil),
                }
            },
        )],
    );

    // runtime.builtin(
    //     "matches",
    //     [signature(
    //         ["text: str", "regex: regex"],
    //         "any",
    //         |runtime, scope| {
    //             let text = runtime.get_unchecked(scope, "text");

    //             let Value::Str(text) = text.clone() else {
    //                 return RuntimeError(format!(
    //                     "matches() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let regex = runtime.get_unchecked(scope, "regex");

    //             let Value::Regex(regex) = regex.clone() else {
    //                 return RuntimeError(format!(
    //                     "matches() regex must be a regex, is a: {}",
    //                     regex.ty()
    //                 ))
    //                 .into();
    //             };

    //             match regex.0.captures(&text) {
    //                 Some(cap) => {
    //                     let mut groups = vec![];

    //                     for m in cap.iter() {
    //                         groups.push(
    //                             match m {
    //                                 Some(m) => {
    //                                     let matched_part =
    //                                         runtime.new_value(Value::Str(m.as_str().into()));
    //                                     let offset = runtime.new_value(Value::Numeric(
    //                                         Numeric::Int(m.start() as i64),
    //                                     ));

    //                                     runtime.new_value(Value::Tuple(
    //                                         Some(vec![Type::Str, Type::Numeric]),
    //                                         vec![matched_part.0, offset.0],
    //                                     ))
    //                                 }
    //                                 None => runtime.new_value(Value::Nil),
    //                             }
    //                             .0,
    //                         );
    //                     }

    //                     Ok(runtime
    //                         .new_value(Value::List(Type::List(Type::Tuple(None).into()), groups)))
    //                 }
    //                 None => Ok(runtime.new_value(Value::Nil)),
    //             }
    //         },
    //     )],
    // );

    runtime.builtin(
        "match_all",
        [signature(
            ["text: str", "regex: regex"],
            "any",
            |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text.clone() else {
                    return RuntimeError(format!(
                        "match() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let regex = runtime.get_unchecked(scope, "regex");

                let Value::Regex(regex) = regex.clone() else {
                    return RuntimeError(format!(
                        "match() regex must be a regex, is a: {}",
                        regex.ty()
                    ))
                    .into();
                };

                let items = regex
                    .0
                    .captures_iter(&text)
                    .map(|cap| {
                        let m = cap.get(0).unwrap();
                        let matched = Value::Str(m.as_str().into());
                        let offset = Value::Numeric(Numeric::Int(m.start() as i64));

                        let tuple = runtime
                            .heap
                            .alloc(Tuple {
                                ty: Some(vec![Type::Str, Type::Numeric]),
                                elements: vec![matched, offset],
                            })
                            .unrooted();

                        Value::Tuple(tuple)
                    })
                    .collect::<Vec<_>>();

                let list = runtime
                    .heap
                    .alloc(List {
                        ty: Type::Tuple(Some(vec![Type::Str, Type::Numeric])),
                        elements: items,
                    })
                    .unrooted();

                Ok(Value::List(list))
            },
        )],
    );

    runtime.builtin(
        "starts_with",
        [signature(
            ["text: str", "substr: str"],
            "any",
            |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text else {
                    return RuntimeError(format!(
                        "starts_with() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let substr = runtime.get_unchecked(scope, "substr");

                let Value::Str(substr) = substr else {
                    return RuntimeError(format!(
                        "starts_with() substr must be a string, is a: {}",
                        substr.ty()
                    ))
                    .into();
                };

                Ok(Value::Bool(text.starts_with(substr.as_str())))
            },
        )],
    );

    runtime.builtin(
        "ascii",
        [signature(["c: str"], "any", |runtime, scope| {
            let c = runtime.get_unchecked(scope, "c");

            let Value::Str(c) = c else {
                return RuntimeError(format!("ascii() c must be a string, is a: {}", c.ty()))
                    .into();
            };

            if c.len() != 1 {
                return RuntimeError(format!(
                    "ascii() c must be a string of length 1, given: {}",
                    c
                ))
                .into();
            }

            Ok(Value::Numeric(Numeric::Int(
                c.chars().next().unwrap() as i64
            )))
        })],
    );

    runtime.builtin(
        "replace",
        [signature(
            ["text: str", ("def: tuple")],
            "any",
            |runtime, scope| {
                let text = runtime.get_unchecked(scope, "text");

                let Value::Str(text) = text else {
                    return RuntimeError(format!(
                        "replace() text must be a string, is a: {}",
                        text.ty()
                    ))
                    .into();
                };

                let def = runtime.get_unchecked(scope, "def");

                let Value::Tuple(def) = def else {
                    return RuntimeError(format!(
                        "replace() def must be a tuple, is a: {}",
                        def.ty()
                    ))
                    .into();
                };

                let def = runtime.heap[*def].clone();

                if def.elements.len() != 2 {
                    return RuntimeError(format!(
                        "replace() def must be a tuple with two elements"
                    ))
                    .into();
                }

                let replace = def.elements.get(1).unwrap();

                let Value::Str(replace) = replace else {
                    return RuntimeError(format!("replace() def[1] must be a string")).into();
                };

                let find = def.elements.get(0).unwrap();

                match find {
                    Value::Str(find) => Ok(Value::Str(
                        text.replace(find.as_str(), replace.as_str()).into(),
                    )),
                    Value::Regex(find) => Ok(Value::Str(
                        find.0.replace_all(&text, replace.to_string()).into(),
                    )),
                    _ => {
                        return RuntimeError(format!(
                            "replace() def[0] must be a string or regex, is a: {}",
                            find.ty()
                        ))
                        .into();
                    }
                }
            },
        )],
    );

    // runtime.builtin(
    //     "slice",
    //     [
    //         signature(["list: [any]", "i: int"], "any", |runtime, scope| {
    //             let list = runtime.get_unchecked(scope, "list");

    //             let Value::List(t, list) = list.clone() else {
    //                 return RuntimeError(format!(
    //                     "slice() list must be a list, is a: {}",
    //                     list.ty()
    //                 ))
    //                 .into();
    //             };

    //             let i = runtime.get_unchecked(scope, "i");

    //             let Value::Numeric(i) = i else {
    //                 return RuntimeError(format!(
    //                     "slice() i must be an int, is a: {}",
    //                     i.ty()
    //                 ))
    //                 .into();
    //             };

    //             let i = i.get_int()?;

    //             if i < 0 {
    //                 return RuntimeError(format!("slice() i must be a positive int")).into();
    //             }

    //             Ok(runtime.new_value(Value::List(
    //                 t.clone(),
    //                 list.clone().split_off((i as usize).min(list.len())),
    //             )))
    //         }),
    //         signature(["text: str", "i: int"], "any", |runtime, scope| {
    //             let text = runtime.get_unchecked(scope, "text");

    //             let Value::Str(text) = text else {
    //                 return RuntimeError(format!(
    //                     "slice() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let i = runtime.get_unchecked(scope, "i");

    //             let Value::Numeric(i) = i else {
    //                 return RuntimeError(format!(
    //                     "slice() i must be an int, is a: {}",
    //                     i.ty()
    //                 ))
    //                 .into();
    //             };

    //             let mut i = i.get_int()?;

    //             if i < 0 && text.len() as i64 + i >= 0 {
    //                 i = text.len() as i64 + i;
    //             }

    //             if i < 0 {
    //                 return RuntimeError(format!("slice() i must be a positive int")).into();
    //             }

    //             // println!("text :slice i -- {}, {} -- {}", text.len(), i, text.len());

    //             Ok(runtime.new_value(Value::Str(text.substr((i as usize)..text.len()))))
    //         }),
    //         signature(["text: str", ("range: tuple")], "any", |runtime, scope| {
    //             let text = runtime.get_unchecked(scope, "text");

    //             let Value::Str(text) = text.clone() else {
    //                 return RuntimeError(format!(
    //                     "slice() text must be a string, is a: {}",
    //                     text.ty()
    //                 ))
    //                 .into();
    //             };

    //             let range = runtime.get_unchecked(scope, "range");

    //             let Value::Tuple(_, range) = range.clone() else {
    //                 return RuntimeError(format!(
    //                     "slice() range must be an (int, int) range, is a: {}",
    //                     range.ty()
    //                 ))
    //                 .into();
    //             };

    //             let Some(start) = range.get(0) else {
    //                 return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                     .into();
    //             };

    //             let Value::Numeric(start) = *start else {
    //                 return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                     .into();
    //             };

    //             let start = start.get_int()?;

    //             if start < 0 {
    //                 return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                     .into();
    //             }

    //             let Some(end) = range.get(1) else {
    //                 return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                     .into();
    //             };

    //             let Value::Numeric(end) = *end else {
    //                 return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                     .into();
    //             };

    //             let mut end = end.get_int()?;

    //             if end < 0 {
    //                 end = (text.len() as i64) + end;
    //             }

    //             Ok(runtime.new_value(Value::Str(
    //                 text.substr((start as usize)..(end as usize).min(text.len())),
    //             )))
    //         }),
    //         signature(
    //             ["list: [any]", ("range: tuple")],
    //             "any",
    //             |runtime, scope| {
    //                 let list = runtime.get_unchecked(scope, "list");

    //                 let Value::List(el_type, list) = list.clone() else {
    //                     return RuntimeError(format!(
    //                         "slice() list must be a list, is a: {}",
    //                         list.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let range = runtime.get_unchecked(scope, "range");

    //                 let Value::Tuple(_, range) = range.clone() else {
    //                     return RuntimeError(format!(
    //                         "slice() range must be an (int, int) range, is a: {}",
    //                         range.ty()
    //                     ))
    //                     .into();
    //                 };

    //                 let Some(start) = range.get(0) else {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 };

    //                 let Value::Numeric(start) = *start else {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 };

    //                 let start = start.get_int()?;

    //                 if start < 0 {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 }

    //                 let Some(end) = range.get(1) else {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 };

    //                 let Value::Numeric(end) = *end else {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 };

    //                 let end = end.get_int()?;

    //                 if end < 0 {
    //                     return RuntimeError(format!("slice() range must be an (int, int) range"))
    //                         .into();
    //                 }

    //                 let slice_els = list[(start as usize)..(end as usize).min(list.len())]
    //                     .into_iter()
    //                     .map(|v| runtime.clone(*v).0)
    //                     .collect::<Vec<_>>();

    //                 Ok(runtime.new_value(Value::List(el_type, slice_els)))
    //             },
    //         ),
    //     ],
    // );

    runtime.builtin(
        "clone",
        [signature(["data"], "any", |runtime, scope| {
            let data = runtime.get_unchecked(scope, "data").clone();
            Ok(runtime.clone(data))
        })],
    );

    runtime.builtin(
        "hash",
        [signature(["data"], "any", |runtime, scope| {
            let data = runtime.get_unchecked(scope, "data");

            Ok(Value::Numeric(Numeric::Int(runtime.hash(data) as i64)))
        })],
    );

    runtime.builtin(
        "trim",
        [signature(["text: str"], "any", |runtime, scope| {
            let text = runtime.get_unchecked(scope, "text");

            let Value::Str(text) = text else {
                return RuntimeError(format!("trim[#1] must be a string, is a: {}", text.ty()))
                    .into();
            };

            Ok(Value::Str(text.substr_from(text.trim())))
        })],
    );

    runtime.builtin(
        "len",
        [signature(
            ["data: list | tuple | str"],
            "any",
            |runtime, scope| {
                let data = runtime.get_unchecked(scope, "data");

                let len = match data {
                    Value::Str(text) => text.len(),
                    Value::List(list) => runtime.heap[*list].elements.len(),
                    Value::Tuple(tuple) => runtime.heap[*tuple].elements.len(),
                    _ => {
                        return RuntimeError(format!("cannot get len of: {}", data.ty())).into();
                    }
                };

                Ok(Value::Numeric(Numeric::Int(len as i64)))
            },
        )],
    );

    runtime.builtin(
        "chars",
        [signature(["text: str"], "any", |runtime, scope| {
            let text = runtime.get_unchecked(scope, "text");

            let Value::Str(text) = text.clone() else {
                return RuntimeError(format!("trim[#1] must be a string, is a: {}", text.ty()))
                    .into();
            };

            let items = text
                .chars()
                .map(|c| Value::Str(c.to_string().into()))
                .collect::<Vec<_>>();

            let list = runtime
                .heap
                .alloc(List {
                    ty: Type::Str,
                    elements: items,
                })
                .unrooted();

            Ok(Value::List(list))
        })],
    );

    runtime.builtin(
        "assert",
        [signature(["data"], "any", |runtime, scope| {
            let data = runtime.get_unchecked(scope, "data");

            if !data.truthy() {
                return RuntimeError(format!("assertion failed")).into();
            }

            Ok(Value::Nil)
        })],
    );

    runtime.builtin(
        "int",
        [
            signature(["data"], "any", |runtime, scope| {
                let data = runtime.get_unchecked(scope, "data");
                let result = data.auto_coerce_int()?;
                Ok(Value::Numeric(Numeric::Int(result)))
            }),
            signature(["data: str", "radix: int"], "any", |runtime, scope| {
                let data = runtime.get_unchecked(scope, "data");

                let Value::Str(s) = data else {
                    return RuntimeError(format!("int() data expected a str, is a: {}", data.ty()))
                        .into();
                };

                let radix = runtime.get_unchecked(scope, "radix");

                let Value::Numeric(Numeric::Int(radix)) = radix else {
                    return RuntimeError(format!(
                        "int() radix expected an int, is a: {}",
                        radix.ty()
                    ))
                    .into();
                };

                if *radix < 1 {
                    return RuntimeError(format!(
                        "int() radix expected a positive int >= 1, is: {}",
                        radix
                    ))
                    .into();
                }

                let result = i64::from_str_radix(&s, *radix as u32)
                    .map(|n| Value::Numeric(Numeric::Int(n)))
                    .unwrap_or(Value::Nil);

                Ok(result)
            }),
        ],
    );

    runtime.builtin(
        "str",
        [signature(["data"], "any", |runtime, scope| {
            let data = runtime.get_unchecked(scope, "data");

            let result = data.auto_coerce_str();

            Ok(Value::Str(Substr::from(result)))
        })],
    );

    runtime.builtin(
        "sqrt",
        [signature(["num: num"], "any", |runtime, scope| {
            let num = runtime.get_unchecked(scope, "num");

            let Value::Numeric(num) = num else {
                return RuntimeError(format!("sqrt() num should be a num, is a: {}", num.ty()))
                    .into();
            };

            Ok(Value::Numeric(Numeric::Double(num.get_double().sqrt())))
        })],
    );

    runtime.builtin(
        "ceil",
        [signature(["num: num"], "any", |runtime, scope| {
            let num = runtime.get_unchecked(scope, "num");

            let Value::Numeric(num) = num else {
                return RuntimeError(format!("ceil() num should be a num, is a: {}", num.ty()))
                    .into();
            };

            Ok(Value::Numeric(Numeric::Double(num.get_double().ceil())))
        })],
    );

    runtime.builtin(
        "floor",
        [signature(["num: num"], "any", |runtime, scope| {
            let num = runtime.get_unchecked(scope, "num");

            let Value::Numeric(num) = num else {
                return RuntimeError(format!("floor() num should be a num, is a: {}", num.ty()))
                    .into();
            };

            Ok(Value::Numeric(Numeric::Double(num.get_double().floor())))
        })],
    );

    runtime.builtin(
        "abs",
        [signature(["num: num"], "any", |runtime, scope| {
            let num = runtime.get_unchecked(scope, "num");

            let Value::Numeric(num) = num else {
                return RuntimeError(format!("abs() num should be a num, is a: {}", num.ty()))
                    .into();
            };

            match num {
                Numeric::Int(n) => Ok(Value::Numeric(Numeric::Int(n.abs()))),
                Numeric::Double(d) => Ok(Value::Numeric(Numeric::Double(d.abs()))),
            }
        })],
    );

    runtime.builtin(
        "round",
        [signature(["num: num"], "any", |runtime, scope| {
            let num = runtime.get_unchecked(scope, "num");

            let Value::Numeric(num) = num else {
                return RuntimeError(format!("abs() num should be a num, is a: {}", num.ty()))
                    .into();
            };

            match num {
                Numeric::Int(n) => Ok(Value::Numeric(Numeric::Int(*n))),
                Numeric::Double(d) => Ok(Value::Numeric(Numeric::Int(d.round() as i64))),
            }
        })],
    );
}
