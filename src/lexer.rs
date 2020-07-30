use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = input.chars().collect::<Vec<char>>();
        Self { input, pos: 0 }
    }

    fn current_char(&self) -> Option<&char> {
        self.input.get(self.pos)
    }

    fn is_digit(ch: &char) -> bool {
        ch.is_ascii_digit()
    }

    fn is_letter(ch: &char) -> bool {
        ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == &'_'
    }

    fn new_identifier_token(ident: String) -> Token {
        let s: &str = &ident;
        match s {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(ident),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token = match self.current_char() {
            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.advance();
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
                    self.advance();
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
                    Self::new_identifier_token(self.read_identifier())
                } else if Self::is_digit(ch) {
                    Token::Int(self.read_number())
                } else {
                    Token::Illegal(format!("{}", ch))
                }
            }
            None => return None,
        };
        self.advance();
        Some(token)
    }

    // `next` is used in an Iterator
    fn advance(&mut self) {
        self.pos = std::cmp::min(self.pos + 1, self.input.len());
    }

    fn peek_char(&self) -> Option<&char> {
        self.input.get(self.pos + 1)
    }

    fn read_identifier(&mut self) -> String {
        let is_latter =
            |ch: Option<&char>| -> bool { ch.filter(|&ch| Self::is_letter(ch)).is_some() };
        assert!(is_latter(self.current_char()));
        let l = self.pos;
        while is_latter(self.peek_char()) {
            self.advance();
        }
        let r = self.pos + 1;
        self.input[l..r].into_iter().collect::<String>()
    }

    fn read_number(&mut self) -> String {
        let is_digit =
            |ch: Option<&char>| -> bool { ch.filter(|&ch| Self::is_digit(ch)).is_some() };
        assert!(is_digit(self.current_char()));
        let l = self.pos;
        while is_digit(self.peek_char()) {
            self.advance();
        }
        let r = self.pos + 1;
        self.input[l..r].into_iter().collect::<String>()
    }

    fn skip_whitespace(&mut self) {
        while self
            .current_char()
            .filter(|&ch| ch.is_ascii_whitespace())
            .is_some()
        {
            self.advance();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn iter() {
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
        assert_eq!(iter.next(), Some(Token::Identifier("five".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Int("5".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Identifier("ten".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Int("10".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Identifier("add".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Function));
        assert_eq!(iter.next(), Some(Token::LParen));
        assert_eq!(iter.next(), Some(Token::Identifier("x".into())));
        assert_eq!(iter.next(), Some(Token::Comma));
        assert_eq!(iter.next(), Some(Token::Identifier("y".into())));
        assert_eq!(iter.next(), Some(Token::RParen));
        assert_eq!(iter.next(), Some(Token::LBrace));
        assert_eq!(iter.next(), Some(Token::Identifier("x".into())));
        assert_eq!(iter.next(), Some(Token::Plus));
        assert_eq!(iter.next(), Some(Token::Identifier("y".into())));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::RBrace));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), Some(Token::Let));
        assert_eq!(iter.next(), Some(Token::Identifier("result".into())));
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Identifier("add".into())));
        assert_eq!(iter.next(), Some(Token::LParen));
        assert_eq!(iter.next(), Some(Token::Identifier("five".into())));
        assert_eq!(iter.next(), Some(Token::Comma));
        assert_eq!(iter.next(), Some(Token::Identifier("ten".into())));
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

    #[test]
    fn ident_eof() {
        let input = r#"
        foo + bar
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Identifier("foo".into())));
        assert_eq!(iter.next(), Some(Token::Plus));
        assert_eq!(iter.next(), Some(Token::Identifier("bar".into())));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn int_eof() {
        let input = r#"
        1 + 2
"#;
        let lexer = Lexer::new(input.into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Int("1".into())));
        assert_eq!(iter.next(), Some(Token::Plus));
        assert_eq!(iter.next(), Some(Token::Int("2".into())));
        assert_eq!(iter.next(), None);
    }
}
