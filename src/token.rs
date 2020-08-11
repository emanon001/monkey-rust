#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    /// 123
    Int(String),
    /// true
    True,
    /// false
    False,
    /// "foobar"
    String(String),
    /// foo, foobar
    Identifier(String),
    /// =
    Assign,
    /// !
    Bang,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// ==
    Eq,
    /// !=
    NotEq,
    /// fn
    Function,
    /// >=
    GT,
    /// <=
    LT,
    /// if
    If,
    /// else
    Else,
    /// let
    Let,
    /// return
    Return,
    /// (
    LParen,
    /// )
    RParen,
    /// {
    LBrace,
    /// }
    RBrace,
    /// [
    LBracket,
    /// [
    RBracket,
    /// ,
    Comma,
    /// :
    Colon,
    /// ;
    Semicolon,
    /// quote
    Quote,
    /// unquote
    Unquote,
    Illegal(String),
}
