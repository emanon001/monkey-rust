use crate::ast::{self};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut iter = lexer.into_iter();
        let current_token = iter.next();
        let peek_token = iter.next();
        Self {
            lexer: iter,
            current_token,
            peek_token,
        }
    }

    pub fn next(&mut self) {
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program() -> ast::Program {
        panic!();
    }
}
