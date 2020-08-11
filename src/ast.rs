mod modify;
use itertools::Itertools;
pub use modify::modify;
use std::collections::BTreeMap;
use std::fmt::{self};

// Node

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Node {
    #[allow(dead_code)]
    pub fn program(self) -> Result<Program, String> {
        match self {
            Node::Program(it) => Ok(it),
            _ => Err("node is not Program".into()),
        }
    }

    #[allow(dead_code)]
    pub fn statement(self) -> Result<Statement, String> {
        match self {
            Node::Statement(it) => Ok(it),
            _ => Err("node is not Statement".into()),
        }
    }

    pub fn expression(self) -> Result<Expression, String> {
        match self {
            Node::Expression(it) => Ok(it),
            _ => Err("node is not Expression".into()),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Program(it) => write!(f, "{}", it),
            Self::Statement(it) => write!(f, "{}", it),
            Self::Expression(it) => write!(f, "{}", it),
        }
    }
}

impl std::convert::From<Program> for Node {
    fn from(prog: Program) -> Self {
        Self::Program(prog)
    }
}

impl std::convert::From<Statement> for Node {
    fn from(stat: Statement) -> Self {
        Self::Statement(stat)
    }
}

impl std::convert::From<Expression> for Node {
    fn from(expr: Expression) -> Self {
        Self::Expression(expr)
    }
}

// Program

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Statement {
    Let {
        identifier: Identifier,
        expression: Expression,
    },
    Return(Expression),
    Expression(Expression),
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
            Statement::Block(it) => write!(f, "{}", it),
        }
    }
}

impl std::convert::From<BlockStatement> for Statement {
    fn from(block: BlockStatement) -> Self {
        Self::Block(block)
    }
}

// Identifier

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::convert::From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

impl std::convert::From<String> for Identifier {
    fn from(s: String) -> Self {
        Self(s)
    }
}

// BlockStatement

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Hash(BTreeMap<Expression, Expression>),
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
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
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
            Expression::Hash(h) => {
                let s = h
                    .into_iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .join(", ");
                write!(f, "{{{}}}", s)
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
            Expression::Index { left, index } => write!(f, "({}[{}])", left, index),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
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
