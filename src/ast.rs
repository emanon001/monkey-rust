// Program

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// Statement

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let { ident: Identifier, expr: Expression },
    Return(Expression),
}

// Expression

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier(Identifier),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier(pub String);
