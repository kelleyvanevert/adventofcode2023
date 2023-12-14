use std::process::exit;

use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter, hint::HistoryHinter,
    validate::MatchingBracketValidator, Cmd, Completer, Editor, EventHandler, Helper, Highlighter,
    Hinter, KeyCode, KeyEvent, Modifiers, Validator,
};

use crate::{parse::parse_document, runtime::Runtime};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

pub fn repl() {
    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter::new(),
    };
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(h));
    rl.bind_sequence(
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );

    let mut runtime = Runtime::new();

    loop {
        match rl.readline("> ") {
            Ok(code) if code == ":quit" || code == ":exit" => {
                exit(0);
            }
            Ok(code) if code == ":help" => {
                println!("Hey!")
            }
            Ok(code) => {
                rl.add_history_entry(&code).unwrap();

                let Some(doc) = parse_document(&code) else {
                    eprintln!("could not parse");
                    continue;
                };

                match runtime.execute_document(&doc, None) {
                    Ok(res) => {
                        println!("{}", res);
                    }
                    Err(err) => {
                        eprintln!("some error occurred: {err:?}");
                        continue;
                    }
                }

                // runtime.gc([]);
            }
            Err(ReadlineError::Eof) => {
                // Ctrl+D
                exit(0);
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C
                exit(0);
            }
            Err(err) => {
                eprintln!("Some error occurred: {err:?}");
                exit(5);
            }
        }
    }
}
