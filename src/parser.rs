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

    pub fn parse_program(&mut self) -> Result<ast::Program, Vec<String>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while self.current_token.is_some() {
            match self.parse_statement() {
                Ok(Some(stmt)) => statements.push(stmt),
                Ok(_) => {}
                Err(mut e) => errors.append(&mut e),
            }
            self.next();
        }
        if errors.is_empty() {
            Ok(ast::Program { statements })
        } else {
            Err(errors)
        }
    }

    fn next(&mut self) {
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    fn parse_statement(&mut self) -> Result<Option<ast::Statement>, Vec<String>> {
        match &self.current_token {
            Some(Token::Let) => self.parse_let_statement().map(|s| Some(s)),
            Some(Token::Return) => self.parse_return_statement().map(|s| Some(s)),
            _ => Ok(None),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, Vec<String>> {
        // let <identifier> = <expr>;
        assert!(self.current_token == Some(Token::Let));

        let name = match &self.peek_token {
            Some(Token::Identifier(name)) => name.clone(),
            t => return Err(vec![Self::new_token_error_message("Identifier", t)]),
        };

        self.next();
        match &self.peek_token {
            Some(Token::Assign) => {}
            t => return Err(vec![Self::new_token_error_message("Assign", t)]),
        };

        // TODO: parse expr
        while self
            .peek_token
            .as_ref()
            .filter(|&t| t != &Token::Semicolon)
            .is_some()
        {
            self.next();
        }

        let stmt = ast::Statement::Let {
            ident: ast::Identifier(name),
            expr: ast::Expression::Identifier(ast::Identifier("dummy".into())), // TODO: use parsed expr
        };

        Ok(stmt)
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, Vec<String>> {
        // return <expression>;
        assert!(self.current_token == Some(Token::Return));

        self.next();

        // TODO: parse expr
        while self
            .peek_token
            .as_ref()
            .filter(|&t| t != &Token::Semicolon)
            .is_some()
        {
            self.next();
        }

        let stmt = ast::Statement::Return(
            ast::Expression::Identifier(ast::Identifier("dummy".into())), // TODO: use parsed expr
        );
        Ok(stmt)
    }

    fn new_token_error_message(expected: &str, actual: &Option<Token>) -> String {
        let actual = match actual {
            Some(t) => format!("{:?}", t),
            _ => "EOF".into(),
        };
        format!("expected token to be {}, got {} instead", expected, actual)
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
        match parser.parse_program() {
            Ok(program) => {
                assert_eq!(program.statements.len(), 3);
                let expected_names = vec!["x", "y", "foobar"];
                for i in 0..expected_names.len() {
                    let s = &program.statements[i];
                    test_let_statement(s, expected_names[i]);
                }
            }
            Err(errors) => {
                panic!(errors.join("\n"));
            }
        }
    }

    #[test]
    fn return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => {
                assert_eq!(program.statements.len(), 3);
                for s in &program.statements {
                    test_return_statement(s);
                }
            }
            Err(errors) => {
                panic!(errors.join("\n"));
            }
        }
    }

    fn test_let_statement(s: &ast::Statement, name: &str) {
        match s {
            ast::Statement::Let { ident, .. } => {
                let s = &ident.0;
                assert_eq!(s, name, "identifier not {}. got={}", name, s);
            }
            _ => panic!("statement not `let`. got={:?}", s),
        };
    }

    fn test_return_statement(s: &ast::Statement) {
        match s {
            ast::Statement::Return(_) => {}
            _ => panic!("statement not `return`. got={:?}", s),
        };
    }
}
