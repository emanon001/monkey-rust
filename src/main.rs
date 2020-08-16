use monkey::repl::Repl;
use std::io::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let prompt = ">> ";
    let repl = Repl::new(prompt);
    repl.start(io::stdin(), io::stdout())
}
