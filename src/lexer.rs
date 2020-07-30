use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = input.chars().collect::<Vec<char>>();
        let mut lexer = Self {
            input,
            pos: 0,
            read_pos: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            Some(';') => Token::Semicolon,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::NotEq
                }
                _ => Token::Bang,
            },
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(ch) => {
                if Self::is_letter(ch) {
                    // already read next char
                    return Self::new_indent_token(self.read_identifier());
                } else if Self::is_digit(ch) {
                    // already read next char
                    return Token::Int(self.read_number());
                } else {
                    Token::Illegal(format!("{}", ch))
                }
            }
            None => Token::EOF,
        };
        self.read_char();
        token
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_pos >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_pos])
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.ch = self.peek_char();
        if self.ch.is_some() {
            self.pos = self.read_pos;
            self.read_pos += 1;
        }
        self.ch
    }

    fn read_identifier(&mut self) -> String {
        assert!(self.ch.is_some());
        let l = self.pos;
        while self.ch.filter(|&ch| Self::is_letter(ch)).is_some() {
            self.read_char();
        }
        let r = self.pos;
        // [l, r)
        self.input[l..r].into_iter().collect::<String>()
    }

    fn read_number(&mut self) -> String {
        assert!(self.ch.is_some());
        let l = self.pos;
        while self.ch.filter(|&ch| Self::is_digit(ch)).is_some() {
            self.read_char();
        }
        let r = self.pos;
        // [l, r)
        self.input[l..r].into_iter().collect::<String>()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.filter(|&ch| ch.is_ascii_whitespace()).is_some() {
            self.read_char();
        }
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
    }

    fn new_indent_token(ident: String) -> Token {
        let s: &str = &ident;
        match s {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(ident),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::EOF => None,
            tok => Some(tok),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn parse() {
        let input = r#"
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Ident("five".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Ident("ten".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Ident("add".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Function));
        assert_eq!(iter.next(), Some(Token::LParen));
        assert_eq!(iter.next(), Some(Token::Ident("x".into())));
        assert_eq!(iter.next(), Some(Token::Comma));
        assert_eq!(iter.next(), Some(Token::Ident("y".into())));
        assert_eq!(iter.next(), Some(Token::RParen));
        assert_eq!(iter.next(), Some(Token::LBrace));
        assert_eq!(iter.next(), Some(Token::Ident("x".into())));
        assert_eq!(iter.next(), Some(Token::Plus));
        assert_eq!(iter.next(), Some(Token::Ident("y".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::RBrace));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Ident("result".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Ident("add".into())));
        assert_eq!(iter.next(), Some(Token::LParen));
        assert_eq!(iter.next(), Some(Token::Ident("five".into())));
        assert_eq!(iter.next(), Some(Token::Comma));
        assert_eq!(iter.next(), Some(Token::Ident("ten".into())));
        assert_eq!(iter.next(), Some(Token::RParen));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), None);

        let input = r#"
!-/*5;
5 < 10 > 5;
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Bang));
        assert_eq!(iter.next(), Some(Token::Minus));
        assert_eq!(iter.next(), Some(Token::Slash));
        assert_eq!(iter.next(), Some(Token::Asterisk));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::LT));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::GT));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), None);

        let input = r#"
if (5 < 10) {
    return true;
} else {
    return false;
}
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::If));
        assert_eq!(iter.next(), Some(Token::LParen));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::LT));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::RParen));
        assert_eq!(iter.next(), Some(Token::LBrace));
        assert_eq!(iter.next(), Some(Token::Return));
        assert_eq!(iter.next(), Some(Token::True));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::RBrace));
        assert_eq!(iter.next(), Some(Token::Else));
        assert_eq!(iter.next(), Some(Token::LBrace));
        assert_eq!(iter.next(), Some(Token::Return));
        assert_eq!(iter.next(), Some(Token::False));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::RBrace));
        assert_eq!(iter.next(), None);

        let input = r#"
10 == 10;
10 != 9;
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::Eq));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::NotEq));
        assert_eq!(iter.next(), Some(Token::Int("9".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), None);
    }
}
