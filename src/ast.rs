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
    Let {
        identifier: Identifier,
        expression: Expression,
    },
    Return(Expression),
    Expression(Expression),
    #[allow(dead_code)]
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let {
                identifier,
                expression,
            } => write!(f, "let {} = {};", identifier, expression),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::Block(block) => write!(f, "{}", block),
        }
    }
}

// Identifier

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// BlockStatement

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
    Function(FunctionExpression),
    Call {
        function: CallExpressionFunction,
        arguments: Vec<Expression>,
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
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Call {
                function,
                arguments,
            } => {
                let args = arguments.into_iter().join(", ");
                write!(f, "{}({})", function, args)
            }
        }
    }
}

impl std::convert::From<Identifier> for Expression {
    fn from(id: Identifier) -> Self {
        Self::Identifier(id)
    }
}

impl std::convert::From<FunctionExpression> for Expression {
    fn from(func: FunctionExpression) -> Self {
        Self::Function(func)
    }
}

// PrefixOperator

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

// InfixOperator

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

// FunctionExpression

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionExpression {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.parameters.iter().map(|p| p.to_string()).join(", ");
        let body = &self.body;
        write!(f, "fn ({}) {}", params, body)
    }
}

// CallExpressionFunction

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CallExpressionFunction {
    Identifier(Identifier),
    Function(FunctionExpression),
}

impl fmt::Display for CallExpressionFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CallExpressionFunction::Identifier(id) => write!(f, "{}", id),
            CallExpressionFunction::Function(func) => write!(f, "{}", func),
        }
    }
}
