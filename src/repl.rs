use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::parser::{self, Parser};
use std::io::prelude::*;
use std::io::{self, BufRead};

pub struct Repl {
    prompt: String,
}

impl Repl {
    pub fn new(prompt: String) -> Self {
        Self { prompt }
    }

    pub fn start<R, W>(&self, reader: R, writer: W) -> Result<(), Box<dyn std::error::Error>>
    where
        R: io::Read,
        W: io::Write,
    {
        let reader = io::BufReader::new(reader);
        let mut writer = io::BufWriter::new(writer);
        writer.write_fmt(format_args!("{}", self.prompt))?;
        writer.flush()?;
        for l in reader.lines() {
            let l = l?;
            let lexer = Lexer::new(l);
            let mut parser = Parser::new(lexer);
            match parser.parse() {
                Ok(program) => {
                    let evaluated = eval(program);
                    writer.write_fmt(format_args!("{}\n", evaluated))?;
                }
                Err(parser::Errors(e)) => {
                    Self::print_parse_error(&mut writer, e)?;
                }
            }
            writer.write_fmt(format_args!("{}", self.prompt))?;
            writer.flush()?;
        }
        Ok(())
    }

    fn print_parse_error<W: io::Write>(
        writer: &mut io::BufWriter<W>,
        errors: Vec<String>,
    ) -> io::Result<()> {
        writer.write_fmt(format_args!("parser errors:\n"))?;
        for e in errors {
            writer.write_fmt(format_args!("\t{}\n", e))?;
        }
        Ok(())
    }
}
