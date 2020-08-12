use crate::evaluator::{define_macros, eval, expand_macros};
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::object::{self};
use crate::parser::{self, parse};
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
        write!(writer, "{}", self.prompt)?;
        writer.flush()?;
        let mut env = Environment::new();
        let mut macro_env = Environment::new();
        for l in reader.lines() {
            let l = l?;
            let lexer = Lexer::new(l);
            match parse(lexer) {
                Ok(prog) => {
                    let prog = define_macros(prog, &mut macro_env);
                    match expand_macros(prog, &macro_env) {
                        Ok(expanded) => {
                            let evaluated = eval(expanded.into(), &mut env);
                            if object::Object::Let != evaluated {
                                write!(writer, "{}\n", evaluated)?;
                            }
                        }
                        Err(e) => write!(writer, "{}\n", e)?,
                    }
                }
                Err(parser::Errors(e)) => {
                    Self::print_parse_error(&mut writer, e)?;
                }
            }
            write!(writer, "{}", self.prompt)?;
            writer.flush()?;
        }
        Ok(())
    }

    fn print_parse_error<W: io::Write>(
        writer: &mut io::BufWriter<W>,
        errors: Vec<String>,
    ) -> io::Result<()> {
        write!(writer, "parser errors:\n")?;
        for e in errors {
            write!(writer, "\t{}\n", e)?;
        }
        Ok(())
    }
}
