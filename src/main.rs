use rmonkey::repl::Repl;
use std::io::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let repl = Repl::new(">> ".into());
    repl.start(io::stdin(), io::stdout())
}
