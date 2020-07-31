use crate::token::Token;

// traits

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {}

pub trait Expression: Node {}

// Program

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or("".into())
    }
}

// LetStatement

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl Expression for LetStatement {}

// Identifier

pub struct Identifier {
    token: Token,
    value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl Expression for Identifier {}
