use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

// Program

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or("".into())
    }
}

// Statement

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let {
        token: Token,
        ident: Identifier,
        expr: Box<Expression>,
    },
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. } => token.literal(),
        }
    }
}

// Expression

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token.literal(),
        }
    }
}

// Identifier

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
