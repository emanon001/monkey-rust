use itertools::Itertools;
use std::fmt::{self};

// Program

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?
        }
        Ok(())
    }
}

// Statement

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let { ident: Identifier, expr: Expression },
    Return(Expression),
    Expression(Expression),
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { ident, expr } => write!(f, "let {} = {};", ident, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::Block(block) => write!(f, "{}", block),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?
        }
        Ok(())
    }
}

// Expression

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Prefix {
        operator: PrefixOperator,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
    Boolean(bool),
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: BlockStatement,
    },
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Integer(n) => write!(f, "{}", n),
            Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => write!(f, "if {} {} else {}", condition, consequence, alternative),
            Expression::Function { parameters, body } => {
                let params = parameters.iter().map(|p| p.to_string()).join(", ");
                write!(f, "fn ({}) {}", params, body)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PrefixOperator::Bang => "!",
            PrefixOperator::Minus => "-",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOperator {
    Add,
    Sub,
    Mul,
    Div,
    LT,
    GT,
    Eq,
    NotEq,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            InfixOperator::Add => "+",
            InfixOperator::Sub => "-",
            InfixOperator::Mul => "*",
            InfixOperator::Div => "/",
            InfixOperator::LT => "<",
            InfixOperator::GT => ">",
            InfixOperator::Eq => "==",
            InfixOperator::NotEq => "!=",
        };
        write!(f, "{}", s)
    }
}
