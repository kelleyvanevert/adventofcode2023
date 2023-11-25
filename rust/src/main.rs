#![feature(let_chains)]
use std::{env, fs, process::exit};

use parse::document;
use parser_combinators::Parser;

pub mod ast;
pub mod parse;
pub mod parser_combinators;

fn main() {
    let Some(filename) = env::args().nth(1) else {
        println!("Pass filename to run");
        exit(1);
    };

    let Ok(contents) = fs::read_to_string(filename) else {
        println!("Could not read file");
        exit(2);
    };

    let Some(ast) = document.parse(&contents) else {
        println!("Could not parse");
        exit(3);
    };

    println!("{ast:?}");
}
