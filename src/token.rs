#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Illegal(String),
    EOF,

    Ident(String),
    Int(String),

    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
}
