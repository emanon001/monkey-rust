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
        // let <identifier> = <expression>;
        assert!(self.current_token == Some(Token::Let));

        // <identifier>
        let identifier = Self::parse_identifier(self.peek_token())?;
        self.next();

        // =
        self.expect_peek_token_and_next(Token::Assign)?;

        // TODO: parse expr
        while self
            .current_token()
            .filter(|&t| t != &Token::Semicolon)
            .is_some()
        {
            self.next();
        }

        let stmt = ast::Statement::Let {
            identifier,
            expression: ast::Expression::Identifier(ast::Identifier("dummy".into())), // TODO: use parsed expr
        };

        Ok(stmt)
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        // return <expression>;
        self.expect_current_token(Token::Return)?;

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

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement> {
        self.expect_current_token(Token::LBrace)?;
        self.next();
        let mut statements = Vec::new();
        while self
            .current_token()
            .filter(|&t| t != &Token::RBrace)
            .is_some()
        {
            let s = self.parse_statement()?;
            statements.push(s);
            self.next();
        }
        Ok(ast::BlockStatement { statements })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression> {
        // <expression>

        // parse prefix expression
        let mut left = match self.current_token() {
            Some(Token::Identifier(_)) => self.parse_identifier_expression(),
            Some(Token::Int(_)) => self.parse_integer_expression(),
            Some(Token::Bang) | Some(Token::Minus) => self.parse_prefix_expression(),
            Some(Token::True) | Some(Token::False) => self.parse_boolean_expression(),
            Some(Token::LParen) => self.parse_grouped_expression(),
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Function) => self.parse_function_expression(),
            t => Err(Self::new_parse_error("prefix expression", t).into()),
        }?;

        // parse infix expression
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
        let ident = Self::parse_identifier(self.current_token())?;
        Ok(ast::Expression::Identifier(ident))
    }

    fn parse_integer_expression(&mut self) -> Result<ast::Expression> {
        match self.current_token() {
            Some(Token::Int(s)) => match s.parse::<i64>() {
                Ok(n) => Ok(ast::Expression::Integer(n)),
                Err(_) => Err(Self::new_parse_error("integer", self.current_token()).into()),
            },
            t => Err(Self::new_parse_error("integer", t).into()),
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<ast::Expression> {
        match self.current_token() {
            Some(Token::True) => Ok(ast::Expression::Boolean(true)),
            Some(Token::False) => Ok(ast::Expression::Boolean(false)),
            t => Err(Self::new_parse_error("boolean", t).into()),
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

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        // ( <expression> )
        self.expect_current_token(Token::LParen)?;
        self.next();
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_token_and_next(Token::RParen)?;
        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        // if <condition> { <consequence> } [ else { <alternative> } ]

        // if
        self.expect_current_token(Token::If)?;
        // <condition>
        self.expect_peek_token_and_next(Token::LParen)?;
        self.next();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_token_and_next(Token::RParen)?;
        // { <consequence> }
        self.expect_peek_token_and_next(Token::LBrace)?;
        let consequence = self.parse_block_statement()?;
        // [ else { <alternative> } ]
        let alternative = if self.peek_token() == Some(&Token::Else) {
            self.next();
            self.expect_peek_token_and_next(Token::LBrace)?;
            self.parse_block_statement()?
        } else {
            ast::BlockStatement {
                statements: Vec::new(),
            }
        };
        Ok(ast::Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_function_expression(&mut self) -> Result<ast::Expression> {
        // fn(<arguments>) { <body> }

        // fn(<arguments>)
        self.expect_current_token(Token::Function)?;
        self.expect_peek_token_and_next(Token::LParen)?;
        let parameters = self.parse_function_parameters()?;

        // { <body> }
        self.expect_peek_token_and_next(Token::LBrace)?;
        let body = self.parse_block_statement()?;

        Ok(ast::Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<ast::Identifier>> {
        // ([<arg>, ...])
        self.expect_current_token(Token::LParen)?;
        if self.peek_token() == Some(&Token::RParen) {
            self.next();
            return Ok(Vec::new());
        }
        self.next();
        let mut identifiers = Vec::new();
        let ident = Self::parse_identifier(self.current_token())?;
        identifiers.push(ident);
        while self.peek_token().filter(|&t| t == &Token::Comma).is_some() {
            self.next();
            self.next();
            let ident = Self::parse_identifier(self.current_token())?;
            identifiers.push(ident);
        }
        self.expect_peek_token_and_next(Token::RParen)?;
        Ok(identifiers)
    }

    fn current_prececence(&self) -> Precedence {
        Self::token_precedence(self.current_token())
    }

    fn peek_prececence(&mut self) -> Precedence {
        Self::token_precedence(self.peek_token())
    }

    fn expect_current_token(&mut self, expected: Token) -> Result<()> {
        if self.current_token() == Some(&expected) {
            Ok(())
        } else {
            let s = format!("{:?}", expected);
            Err(Self::new_token_error(&s, self.peek_token()).into())
        }
    }

    fn expect_peek_token(&mut self, expected: Token) -> Result<()> {
        if self.peek_token() == Some(&expected) {
            Ok(())
        } else {
            let s = format!("{:?}", expected);
            Err(Self::new_token_error(&s, self.peek_token()).into())
        }
    }

    fn expect_peek_token_and_next(&mut self, expected: Token) -> Result<()> {
        self.expect_peek_token(expected)?;
        self.next();
        Ok(())
    }

    fn parse_identifier(token: Option<&Token>) -> Result<ast::Identifier> {
        match token {
            Some(Token::Identifier(id)) => Ok(ast::Identifier(id.clone())),
            t => Err(Self::new_parse_error("identifier", t).into()),
        }
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
        parse_expression_statement(s, |expr| test_identifier_expression(expr, "foobar"));
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
        parse_expression_statement(s, |expr| test_integer_expression(expr, 5));
        Ok(())
    }

    #[test]
    fn parse_prefix_expressions() -> Result<()> {
        // (input, operator, right)
        let cases = vec![
            (
                "!5;",
                ast::PrefixOperator::Bang,
                ast::Expression::Integer(5),
            ),
            (
                "-15;",
                ast::PrefixOperator::Minus,
                ast::Expression::Integer(15),
            ),
            (
                "!true;",
                ast::PrefixOperator::Bang,
                ast::Expression::Boolean(true),
            ),
            (
                "!false;",
                ast::PrefixOperator::Bang,
                ast::Expression::Boolean(false),
            ),
        ];
        for (input, op, r) in cases {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse()?;
            assert_eq!(program.statements.len(), 1);
            let s = &program.statements[0];
            parse_expression_statement(s, |expr| match expr {
                ast::Expression::Prefix { .. } => {
                    test_prefix_expression(expr, op, r);
                }
                _ => panic!("expression not prefix. got={:?}", expr),
            });
        }
        Ok(())
    }

    #[test]
    fn parse_infix_expressions() -> Result<()> {
        // (input, left, operator, right)
        let cases = vec![
            (
                "5 + 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::Add,
                ast::Expression::Integer(5),
            ),
            (
                "5 - 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::Sub,
                ast::Expression::Integer(5),
            ),
            (
                "5 * 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::Mul,
                ast::Expression::Integer(5),
            ),
            (
                "5 / 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::Div,
                ast::Expression::Integer(5),
            ),
            (
                "5 > 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::GT,
                ast::Expression::Integer(5),
            ),
            (
                "5 < 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::LT,
                ast::Expression::Integer(5),
            ),
            (
                "5 == 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::Eq,
                ast::Expression::Integer(5),
            ),
            (
                "5 != 5;",
                ast::Expression::Integer(5),
                ast::InfixOperator::NotEq,
                ast::Expression::Integer(5),
            ),
            (
                "true == true;",
                ast::Expression::Boolean(true),
                ast::InfixOperator::Eq,
                ast::Expression::Boolean(true),
            ),
            (
                "true != false;",
                ast::Expression::Boolean(true),
                ast::InfixOperator::NotEq,
                ast::Expression::Boolean(false),
            ),
            (
                "false == false;",
                ast::Expression::Boolean(false),
                ast::InfixOperator::Eq,
                ast::Expression::Boolean(false),
            ),
        ];
        for (input, l, op, r) in cases {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse()?;
            assert_eq!(program.statements.len(), 1);
            let s = &program.statements[0];
            parse_expression_statement(s, |expr| match expr {
                ast::Expression::Infix { .. } => {
                    test_infix_expression(expr, l, op, r);
                }
                _ => panic!("expression not infix. got={:?}", expr),
            });
        }
        Ok(())
    }

    #[test]
    fn parse_if_expression() -> Result<()> {
        let input = r#"
        if (x < y) { x }
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        parse_expression_statement(s, |expr| match expr {
            ast::Expression::If {
                condition,
                consequence,
                ..
            } => {
                // condition
                test_infix_expression(
                    condition,
                    ast::Expression::Identifier(ast::Identifier("x".into())),
                    ast::InfixOperator::LT,
                    ast::Expression::Identifier(ast::Identifier("y".into())),
                );
                // consequence
                assert_eq!(consequence.statements.len(), 1);
                let s = &consequence.statements[0];
                match s {
                    ast::Statement::Expression(expr) => test_identifier_expression(expr, "x"),
                    _ => panic!("statement not `<expr>`. got={:?}", s),
                };
            }
            _ => panic!("expression not if. got={:?}", expr),
        });
        Ok(())
    }

    #[test]
    fn parse_if_else_expression() -> Result<()> {
        let input = r#"
        if (x < y) { x } else { y }
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        parse_expression_statement(s, |expr| match expr {
            ast::Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                // condition
                test_infix_expression(
                    condition,
                    ast::Expression::Identifier(ast::Identifier("x".into())),
                    ast::InfixOperator::LT,
                    ast::Expression::Identifier(ast::Identifier("y".into())),
                );
                // consequence
                assert_eq!(consequence.statements.len(), 1);
                let s = &consequence.statements[0];
                match s {
                    ast::Statement::Expression(expr) => test_identifier_expression(expr, "x"),
                    _ => panic!("statement not `<expr>`. got={:?}", s),
                };
                // alternative
                assert_eq!(alternative.statements.len(), 1);
                let s = &alternative.statements[0];
                match s {
                    ast::Statement::Expression(expr) => test_identifier_expression(expr, "y"),
                    _ => panic!("statement not `<expr>`. got={:?}", s),
                };
            }
            _ => panic!("expression not if. got={:?}", expr),
        });
        Ok(())
    }

    #[test]
    fn parse_function_literal_expression() -> Result<()> {
        let input = r#"
        fn(x, y) { x + y; }
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse()?;
        assert_eq!(program.statements.len(), 1);
        let s = &program.statements[0];
        parse_expression_statement(s, |expr| match expr {
            ast::Expression::Function { parameters, body } => {
                // parameters
                assert_eq!(parameters.len(), 2);
                test_identifier(&parameters[0], "x");
                test_identifier(&parameters[1], "y");
                // body
                assert_eq!(body.statements.len(), 1);
                let s = &body.statements[0];
                match s {
                    ast::Statement::Expression(expr) => test_infix_expression(
                        expr,
                        ast::Expression::Identifier(ast::Identifier("x".into())),
                        ast::InfixOperator::Add,
                        ast::Expression::Identifier(ast::Identifier("y".into())),
                    ),
                    _ => panic!("statement not `<expr>`. got={:?}", s),
                };
            }
            _ => panic!("expression not function. got={:?}", expr),
        });
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
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
                identifier: ast::Identifier("my_var".into()),
                expression: ast::Expression::Identifier(ast::Identifier("another_var".into())),
            }],
        };
        let source = format!("{}", program);
        assert_eq!(source, "let my_var = another_var;".to_string());
    }

    // heplers

    fn parse_expression_statement<F>(s: &ast::Statement, f: F)
    where
        F: FnOnce(&ast::Expression),
    {
        match s {
            ast::Statement::Expression(expr) => f(expr),
            _ => panic!("statement is not `Expression`. got={:?}", s),
        };
    }

    fn test_let_statement(s: &ast::Statement, name: &str) {
        match s {
            ast::Statement::Let { identifier, .. } => test_identifier(identifier, name),
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
                test_identifier(id, name);
            }
            _ => panic!("expression not identifier. got={:?}", expr),
        };
    }

    fn test_identifier(id: &ast::Identifier, name: &str) {
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

    fn test_prefix_expression(
        expr: &ast::Expression,
        operator: ast::PrefixOperator,
        right: ast::Expression,
    ) {
        match expr {
            ast::Expression::Prefix {
                operator: op,
                right: r,
            } => {
                assert_eq!(op, &operator);
                assert_eq!(r, &Box::new(right));
            }
            _ => panic!("expression not prefix. got={:?}", expr),
        }
    }

    fn test_infix_expression(
        expr: &ast::Expression,
        left: ast::Expression,
        operator: ast::InfixOperator,
        right: ast::Expression,
    ) {
        match expr {
            ast::Expression::Infix {
                left: l,
                operator: op,
                right: r,
            } => {
                assert_eq!(l, &Box::new(left));
                assert_eq!(op, &operator);
                assert_eq!(r, &Box::new(right));
            }
            _ => panic!("expression not infix. got={:?}", expr),
        }
    }
}
