pub enum Token {
    Illegal(String),
    Eof,

    Ident(String),
    Int(String),

    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
}
