use mast::lex::Lexer;
use owo_colors::OwoColorize;
use std::io::{self, Write};

fn main() -> Result<(), io::Error> {
    let mut stdout = io::stdout().lock();

    loop {
        write!(stdout, "{} ", ">".green())?;
        stdout.flush()?;

        let Some(Ok(input)) = io::stdin().lines().next() else {
            writeln!(stdout, "{}", "exiting...".yellow())?;
            std::process::exit(0);
        };
        for token in Lexer::new(&input) {
            print!("{token:?} ");
            stdout.flush()?;
        }

        writeln!(stdout)?;
    }
}
