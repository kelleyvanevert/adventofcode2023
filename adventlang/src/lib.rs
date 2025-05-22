#![feature(let_chains)]
#![feature(iterator_try_reduce)]
#![feature(iterator_try_collect)]
#![feature(box_patterns)]

pub mod ast;
pub mod external;
pub mod fmt;
mod gc;
pub mod parse;
pub mod parser_combinators;
pub mod repl;
pub mod runtime;
pub mod runtime_builtins;
pub mod stdlib;
pub mod types;
pub mod value;
