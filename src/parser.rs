use crate::ast::{self};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    lexer: std::iter::Peekable<Lexer>,
    current_token: Option<Token>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut iter = lexer.into_iter().peekable();
        let current_token = iter.next();
        Self {
            lexer: iter,
            current_token,
        }
    }

    pub fn parse_program(&mut self) -> Result<ast::Program> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while self.current_token.is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => errors.push(format!("{}", e)),
            }
            self.next();
        }
        if errors.is_empty() {
            Ok(ast::Program { statements })
        } else {
            Err(errors.join("\n").into())
        }
    }

    fn next(&mut self) {
        self.current_token = self.lexer.next();
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        assert!(self.current_token.is_some());

        match &self.current_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(_) => self.parse_expression_statement(),
            _ => unreachable!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        // let <identifier> = <expr>;
        assert!(self.current_token == Some(Token::Let));

        let name = match self.peek_token() {
            Some(Token::Identifier(name)) => name.clone(),
            t => return Err(Self::new_token_error_message("Identifier", t).into()),
        };

        self.next();
        match self.peek_token() {
            Some(Token::Assign) => {}
            t => return Err(Self::new_token_error_message("Assign", t).into()),
        };

        self.next();

        // TODO: parse expr
        while self
            .current_token
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

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        // return <expression>;
        assert!(self.current_token == Some(Token::Return));

        self.next();

        // TODO: parse expr
        while self
            .current_token
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

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        // `<expression>` | `<expression>;`
        assert!(self.current_token.is_some());

        let expr = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token() == Some(&Token::Semicolon) {
            self.next();
        }
        let stmt = ast::Statement::Expression(expr);
        Ok(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression> {
        match &self.current_token {
            Some(Token::Identifier(id)) => {
                Ok(ast::Expression::Identifier(ast::Identifier(id.clone())))
            }
            Some(Token::Int(s)) => {
                let n = s.parse::<i64>()?;
                Ok(ast::Expression::Integer(n))
            }
            _ => Err("not supported".into()),
        }
    }

    fn new_token_error_message(expected: &str, actual: Option<&Token>) -> String {
        let actual = match actual {
            Some(t) => format!("{:?}", t),
            _ => "EOF".into(),
        };
        format!("expected token to be {}, got {} instead", expected, actual)
    }
}

#[cfg(test)]
mod tests {
    use super::{Parser, Result};
    use crate::ast::{self};
    use crate::lexer::Lexer;

    #[test]
    fn parse_let_statements() -> Result<()> {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 3);
        let expected_names = vec!["x", "y", "foobar"];
        for i in 0..expected_names.len() {
            let s = &program.statements[i];
            test_let_statement(s, expected_names[i]);
        }
        Ok(())
    }

    #[test]
    fn parse_return_statements() -> Result<()> {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 3);
        for s in &program.statements {
            test_return_statement(s);
        }
        Ok(())
    }

    #[test]
    fn parse_identifier_expression() -> Result<()> {
        let input = r#"
        foobar;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        match s {
            ast::Statement::Expression(expr) => match expr {
                ast::Expression::Identifier(ast::Identifier(id)) => {
                    let expected = "foobar";
                    assert_eq!(id, expected, "identifier not {}. got={}", expected, id);
                }
                _ => panic!("expression not Identifier. got={:?}", expr),
            },
            _ => panic!("statement not `<expr>`. got={:?}", s),
        };
        Ok(())
    }

    #[test]
    fn parse_integer_expression() -> Result<()> {
        let input = r#"
        let 5;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        match s {
            ast::Statement::Expression(expr) => match expr {
                ast::Expression::Integer(n) => {
                    let expected = 5;
                    assert_eq!(*n, expected, "integer not {}. got={}", expected, n);
                }
                _ => panic!("expression not Integer. got={:?}", expr),
            },
            _ => panic!("statement not `<expr>`. got={:?}", s),
        };
        Ok(())
    }

    #[test]
    fn display() {
        let program = ast::Program {
            statements: vec![ast::Statement::Let {
                ident: ast::Identifier("my_var".into()),
                expr: ast::Expression::Identifier(ast::Identifier("another_var".into())),
            }],
        };
        let source = format!("{}", program);
        assert_eq!(source, "let my_var = another_var;\n".to_string());
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
