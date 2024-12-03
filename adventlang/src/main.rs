use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    process::exit,
    time::Instant,
};

use adventlang::{
    parse::parse_document, repl::repl, runtime::Runtime, runtime_builtins::RuntimeBuiltinDefs,
    stdlib::implement_stdlib, value::EvalOther,
};
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command()]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parses a script
    Parse {
        /// File to parse
        file: PathBuf,
    },

    /// Execute a script
    Run {
        #[arg(short, long)]
        /// Print out how long parsing and executing took
        timings: bool,

        #[arg(short, long)]
        /// Do a garbage collection run at the end
        gc: bool,

        /// File to run
        file: PathBuf,
    },

    /// Prints the standard library function signatures to stdout
    Stdlib {},

    /// Run a REPL
    Repl {},
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse { file } => {
            let Ok(contents) = fs::read_to_string(&file) else {
                eprintln!("Could not read file: {}", file.display());
                exit(1);
            };

            let t0 = Instant::now();
            let Some(_) = parse_document(&contents) else {
                eprintln!("Could not parse document");
                exit(2);
            };

            eprintln!("Parsed in {:?}", t0.elapsed());
        }

        Commands::Run { timings, file, gc } => {
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
            let mut runtime = Runtime::new();
            if timings {
                eprintln!("Initialized runtime in {:?}", t0.elapsed());
            }

            let t0 = Instant::now();
            let result = runtime.execute_document(&doc, Some(stdin));
            if timings {
                eprintln!("Executed in {:?}", t0.elapsed());
            }
            if let Err(EvalOther::RuntimeError(runtime_err)) = result {
                eprintln!("Runtime error: {}", runtime_err.0);
                exit(3);
            }

            if gc {
                // (mostly just a proof-of-concept atm)
                let t0 = Instant::now();
                runtime.gc([]);
                eprintln!("GC'd in {:?}", t0.elapsed());
            }
        }

        Commands::Stdlib {} => {
            let mut builtins = RuntimeBuiltinDefs::new();

            implement_stdlib(&mut builtins);

            builtins.builtins.sort_by_key(|t| t.0.clone());

            for (name, sigs) in builtins.builtins {
                for sig in sigs {
                    println!("{name}: {sig}");
                }
            }
        }

        Commands::Repl {} => repl(),
    }
}
