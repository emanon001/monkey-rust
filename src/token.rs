#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Int(String),        // 123
    True,               // true
    False,              // false
    String(String),     // "foobar"
    Identifier(String), // foo, foobar
    Assign,             // =
    Bang,               // !
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Eq,                 // ==
    NotEq,              // !=
    Function,           // fn
    GT,                 // >=
    LT,                 // <=
    If,                 // if
    Else,               // else
    Let,                // let
    Return,             // return
    LParen,             // (
    RParen,             // )
    LBrace,             // {
    RBrace,             // }
    Comma,              // ,
    Semicolon,          // ;
    Illegal(String),
}
