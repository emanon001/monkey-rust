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
    Sum,     // + or -
    Product, // * or /
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

    pub fn parse(&mut self) -> Result<ast::Program> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while self.current_token().is_some() {
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

    fn current_token(&self) -> Option<&Token> {
        self.current_token.as_ref()
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        assert!(self.current_token.is_some());

        match self.current_token() {
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
            t => return Err(Self::new_token_error("Identifier", t).into()),
        };

        self.next();
        match self.peek_token() {
            Some(Token::Assign) => {}
            t => return Err(Self::new_token_error("Assign", t).into()),
        };

        self.next();

        // TODO: parse expr
        while self
            .current_token()
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
            .current_token()
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
        assert!(self.current_token().is_some());

        let expr = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token() == Some(&Token::Semicolon) {
            self.next();
        }
        let stmt = ast::Statement::Expression(expr);
        Ok(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression> {
        let token = self
            .current_token()
            .ok_or(Self::new_parse_error("expression", None))?;
        // <identifier> | <integer> | <prefix>
        let mut left = match token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Int(_) => self.parse_integer_expression(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            t => Err(Self::new_parse_error("expression", Some(t)).into()),
        }?;

        while self
            .peek_token()
            .filter(|&t| t != &Token::Semicolon)
            .is_some()
            && precedence < self.peek_prececence()
        {
            left = match self.peek_token().unwrap() {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::LT
                | Token::GT
                | Token::Eq
                | Token::NotEq => {
                    self.next();
                    self.parse_infix_expression(left)?
                }
                _ => return Ok(left),
            }
        }
        Ok(left)
    }

    fn parse_identifier_expression(&mut self) -> Result<ast::Expression> {
        let token = self
            .current_token()
            .ok_or(Self::new_parse_error("identifier", None))?;
        match token {
            Token::Identifier(id) => Ok(ast::Expression::Identifier(ast::Identifier(id.clone()))),
            t => Err(Self::new_parse_error("identifier", Some(t)).into()),
        }
    }

    fn parse_integer_expression(&mut self) -> Result<ast::Expression> {
        let token = self
            .current_token()
            .ok_or(Self::new_parse_error("integer", None))?;
        match token {
            Token::Int(s) => match s.parse::<i64>() {
                Ok(n) => Ok(ast::Expression::Integer(n)),
                Err(_) => Err(Self::new_parse_error("integer", Some(token)).into()),
            },
            t => Err(Self::new_parse_error("integer", Some(t)).into()),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let operator = match self.current_token() {
            Some(Token::Bang) => ast::PrefixOperator::Bang,
            Some(Token::Minus) => ast::PrefixOperator::Minus,
            t => return Err(Self::new_parse_error("prefix operator", t).into()),
        };
        self.next();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(ast::Expression::Prefix {
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Result<ast::Expression> {
        let operator = match self.current_token() {
            Some(Token::Plus) => ast::InfixOperator::Add,
            Some(Token::Minus) => ast::InfixOperator::Sub,
            Some(Token::Asterisk) => ast::InfixOperator::Mul,
            Some(Token::Slash) => ast::InfixOperator::Div,
            Some(Token::LT) => ast::InfixOperator::LT,
            Some(Token::GT) => ast::InfixOperator::GT,
            Some(Token::Eq) => ast::InfixOperator::Eq,
            Some(Token::NotEq) => ast::InfixOperator::NotEq,
            t => return Err(Self::new_parse_error("infix operator", t).into()),
        };
        let precedence = self.current_prececence();
        self.next();
        let right = self.parse_expression(precedence)?;
        Ok(ast::Expression::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn current_prececence(&self) -> Precedence {
        Self::token_precedence(self.current_token())
    }

    fn peek_prececence(&mut self) -> Precedence {
        Self::token_precedence(self.peek_token())
    }

    fn token_precedence(token: Option<&Token>) -> Precedence {
        match token {
            Some(Token::Plus) | Some(Token::Minus) => Precedence::Sum,
            Some(Token::Asterisk) | Some(Token::Slash) => Precedence::Product,
            Some(Token::LT) | Some(Token::GT) => Precedence::LessGreater,
            Some(Token::Eq) | Some(Token::NotEq) => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }

    fn new_token_error(expected: &str, actual: Option<&Token>) -> String {
        let actual = match actual {
            Some(t) => format!("{:?}", t),
            _ => "EOF".into(),
        };
        format!("expected token to be {}, got {} instead", expected, actual)
    }

    fn new_parse_error(expected: &str, actual: Option<&Token>) -> String {
        let actual = match actual {
            Some(t) => format!("{:?}", t),
            _ => "EOF".into(),
        };
        format!("could not parse {} as {}", actual, expected)
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
        let program = parser.parse()?;
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
        let program = parser.parse()?;
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
        let program = parser.parse()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        match s {
            ast::Statement::Expression(expr) => test_identifier_expression(expr, "foobar"),
            _ => panic!("statement not `<expr>`. got={:?}", s),
        };
        Ok(())
    }

    #[test]
    fn parse_integer_expression() -> Result<()> {
        let input = r#"
        5;
        "#
        .into();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        match s {
            ast::Statement::Expression(expr) => test_integer_expression(expr, 5),
            _ => panic!("statement not `<expr>`. got={:?}", s),
        };
        Ok(())
    }

    #[test]
    fn parse_prefix_expressions() -> Result<()> {
        // (input, operator, integer)
        let cases = vec![
            ("!5;", ast::PrefixOperator::Bang, 5),
            ("-15;", ast::PrefixOperator::Minus, 15),
        ];
        for (input, op, int) in cases {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse()?;
            assert_eq!(program.statements.len(), 1);
            let s = &program.statements[0];
            match s {
                ast::Statement::Expression(expr) => match expr {
                    ast::Expression::Prefix { operator, right } => {
                        assert_eq!(operator, &op);
                        assert_eq!(right, &Box::new(ast::Expression::Integer(int)));
                    }
                    _ => panic!("expression not prefix. got={:?}", expr),
                },
                _ => panic!("statement not `<expr>`. got={:?}", s),
            };
        }
        Ok(())
    }

    #[test]
    fn parse_infix_expressions() -> Result<()> {
        // (input, left, operator, right)
        let cases = vec![
            ("5 + 5;", 5, ast::InfixOperator::Add, 5),
            ("5 - 5;", 5, ast::InfixOperator::Sub, 5),
            ("5 * 5;", 5, ast::InfixOperator::Mul, 5),
            ("5 / 5;", 5, ast::InfixOperator::Div, 5),
            ("5 > 5;", 5, ast::InfixOperator::GT, 5),
            ("5 < 5;", 5, ast::InfixOperator::LT, 5),
            ("5 == 5;", 5, ast::InfixOperator::Eq, 5),
            ("5 != 5;", 5, ast::InfixOperator::NotEq, 5),
        ];
        for (input, l, op, r) in cases {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse()?;
            assert_eq!(program.statements.len(), 1);
            let s = &program.statements[0];
            match s {
                ast::Statement::Expression(expr) => match expr {
                    ast::Expression::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        assert_eq!(left, &Box::new(ast::Expression::Integer(l)));
                        assert_eq!(operator, &op);
                        assert_eq!(right, &Box::new(ast::Expression::Integer(r)));
                    }
                    _ => panic!("expression not infix. got={:?}", expr),
                },
                _ => panic!("statement not `<expr>`. got={:?}", s),
            };
        }
        Ok(())
    }

    #[test]
    fn parse_precedence() -> Result<()> {
        // (input, expected)
        let cases = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];
        for (input, expected) in cases {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse()?;
            let s = format!("{}", program);
            assert_eq!(s, expected);
        }
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
        assert_eq!(source, "let my_var = another_var;".to_string());
    }

    // heplers

    fn test_let_statement(s: &ast::Statement, name: &str) {
        match s {
            ast::Statement::Let { ident, .. } => test_identifier_raw(ident, name),
            _ => panic!("statement not `let`. got={:?}", s),
        };
    }

    fn test_return_statement(s: &ast::Statement) {
        match s {
            ast::Statement::Return(_) => {}
            _ => panic!("statement not `return`. got={:?}", s),
        };
    }

    fn test_identifier_expression(expr: &ast::Expression, name: &str) {
        match expr {
            ast::Expression::Identifier(id) => {
                test_identifier_raw(id, name);
            }
            _ => panic!("expression not identifier. got={:?}", expr),
        };
    }

    fn test_identifier_raw(id: &ast::Identifier, name: &str) {
        let s = &id.0;
        assert_eq!(s, name, "identifier not {}. got={}", s, name);
    }

    fn test_integer_expression(expr: &ast::Expression, num: i64) {
        match expr {
            ast::Expression::Integer(n) => {
                assert_eq!(*n, num, "integer not {}. got={}", num, n);
            }
            _ => panic!("expression not Integer. got={:?}", expr),
        }
    }
}
