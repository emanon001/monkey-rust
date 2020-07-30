#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Assign,
    Comma,
    EOF,
    Function,
    Ident(String),
    Illegal(String),
    Int(String),
    LBrace,
    LParen,
    Let,
    Plus,
    RBrace,
    RParen,
    Semicolon,
}
