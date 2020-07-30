use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    read_position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = input.chars().collect::<Vec<char>>();
        Self {
            input,
            read_position: 0,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let ch = if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        };
        if ch.is_some() {
            self.read_position += 1;
        }
        ch
    }

    fn next_token(&mut self) -> Token {
        let ch = self.read_char();
        match ch {
            Some('=') => Token::Assign,
            Some(';') => Token::Semicolon,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            None => Token::Eof,
            _ => panic!(),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
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
        let lexer = Lexer::new("=+(){},;".into());
        let mut iter = lexer.into_iter();
        assert_eq!(iter.next(), Some(Token::Assign));
        assert_eq!(iter.next(), Some(Token::Plus));
        assert_eq!(iter.next(), Some(Token::Lparen));
        assert_eq!(iter.next(), Some(Token::Rparen));
        assert_eq!(iter.next(), Some(Token::Lbrace));
        assert_eq!(iter.next(), Some(Token::Rbrace));
        assert_eq!(iter.next(), Some(Token::Comma));
        assert_eq!(iter.next(), Some(Token::Semicolon));
        assert_eq!(iter.next(), None);
    }
}
