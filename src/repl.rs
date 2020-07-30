use crate::lexer::Lexer;
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
            for t in lexer {
                writer.write_fmt(format_args!("{:?}\n", t))?;
            }
            writer.write_fmt(format_args!("{}", self.prompt))?;
            writer.flush()?;
        }
        Ok(())
    }
}
