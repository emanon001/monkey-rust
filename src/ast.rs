// Program

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    statements: Vec<Statement>,
}

// Statement

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { ident: Expression, expr: Expression },
}

// Expression

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
}
