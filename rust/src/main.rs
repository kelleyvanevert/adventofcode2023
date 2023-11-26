#![feature(let_chains)]
use std::{env, fs, io, process::exit};

pub mod ast;
pub mod parse;
pub mod parser_combinators;
pub mod runtime;

use parse::parse_document;
use runtime::execute;

fn main() {
    let Some(filename) = env::args().nth(1) else {
        eprintln!("Pass name of file to execute");
        exit(1);
    };

    let Ok(contents) = fs::read_to_string(filename) else {
        eprintln!("Could not read file");
        exit(2);
    };

    let Some(doc) = parse_document(&contents) else {
        eprintln!("Could not parse");
        exit(3);
    };

    let stdin = io::stdin()
        .lines()
        .map(|line| line.unwrap())
        .collect::<Vec<_>>()
        .join("\n");

    match execute(&doc, stdin) {
        Err(runtime_err) => {
            eprintln!("Runtime error: {}", runtime_err.0);
            exit(4);
        }
        Ok(value) => {
            println!("{value}");
            exit(0);
        }
    }
}
