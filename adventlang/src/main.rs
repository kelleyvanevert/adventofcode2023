#![feature(let_chains)]
#![feature(iterator_try_reduce)]
#![feature(iterator_try_collect)]
#![feature(box_patterns)]
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    process::exit,
    time::Instant,
};

mod ast;
mod parse;
mod parser_combinators;
mod runtime;
mod stdlib;

use clap::{Parser, Subcommand};
use parse::parse_document;
use runtime::execute;

#[derive(Parser)]
#[command()]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Execute a script
    Run {
        #[arg(short, long)]
        /// Print out how long parsing and executing took
        timings: bool,

        /// File to run
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { timings, file } => {
            let Ok(contents) = fs::read_to_string(&file) else {
                eprintln!("Could not read file: {}", file.display());
                exit(1);
            };

            let t0 = Instant::now();
            let Some(doc) = parse_document(&contents) else {
                eprintln!("Could not parse document");
                exit(2);
            };
            // println!("Parsed: {doc:?}");
            if timings {
                eprintln!("Parsed in {:?}", t0.elapsed());
            }

            let stdin = if atty::is(atty::Stream::Stdin) {
                "".to_string()
            } else {
                let mut str = String::new();
                io::stdin().read_to_string(&mut str).unwrap();
                str
            };

            let t0 = Instant::now();
            let result = execute(&doc, stdin);
            if timings {
                eprintln!("Executed in {:?}", t0.elapsed());
            }
            if let Err(runtime_err) = result {
                eprintln!("Runtime error: {}", runtime_err.0);
                exit(3);
            }
        }
    }
}
