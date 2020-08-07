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
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Prefix {
        operator: PrefixOperator,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function(FunctionExpression),
    Call {
        function: CallExpressionFunction,
        args: Vec<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Integer(n) => write!(f, "{}", n),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::String(s) => write!(f, r#""{}""#, s),
            Expression::Array(v) => {
                let s = v.iter().join(", ");
                write!(f, "[{}]", s)
            }
            Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if {} {{ {} }}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {{ {} }}", alt)
                } else {
                    Ok(())
                }
            }
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Call { function, args } => {
                let args = args.into_iter().join(", ");
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

impl std::convert::From<CallExpressionFunction> for Expression {
    fn from(call: CallExpressionFunction) -> Self {
        match call {
            CallExpressionFunction::Identifier(id) => id.into(),
            CallExpressionFunction::Function(f) => f.into(),
        }
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
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.iter().join(", ");
        write!(f, "fn ({}) {}", params, self.body)
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
