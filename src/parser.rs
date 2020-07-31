use crate::ast::{self};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut iter = lexer.into_iter();
        let current_token = iter.next();
        let peek_token = iter.next();
        Self {
            lexer: iter,
            current_token,
            peek_token,
        }
    }

    pub fn next(&mut self) {
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = Vec::new();
        while self.current_token.is_some() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next();
        }
        ast::Program { statements }
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(_) => None,
            None => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        // let <identifier> = <expr>
        assert!(self.current_token == Some(Token::Let));
        let name = match &self.peek_token {
            Some(Token::Identifier(name)) => name.clone(),
            _ => return None,
        };

        self.next();
        match &self.peek_token {
            Some(Token::Assign) => {}
            _ => return None,
        };

        // TODO: parse expr

        let stmt = ast::Statement::Let {
            ident: ast::Expression::Identifier(name),
            expr: ast::Expression::Identifier("dummy".into()), // TODO: use parsed expr
        };

        Some(stmt)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::{self};
    use crate::lexer::Lexer;

    #[test]
    fn let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program(); // panic
        let expected_names = vec!["x", "y", "foobar"];
        for i in 0..expected_names.len() {
            let s = &program.statements[i];
            test_let_statement(s, expected_names[i]);
        }
    }

    fn test_let_statement(s: &ast::Statement, name: &str) {
        match s {
            ast::Statement::Let { ident, .. } => match ident {
                ast::Expression::Identifier(s) => {
                    assert_eq!(s, name, "identifier not {}. got={}", name, s);
                }
            },
        };
    }
}
