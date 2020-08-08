use crate::ast::{self};
use itertools::Itertools;
use std::collections::HashMap;
use std::convert::{self};
use std::fmt::{self};

// object enum

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
    Null,
    Return(Box<Object>),
    Error(String),
    Let,
    Function {
        params: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    },
    LetFunction {
        id: ast::Identifier,
        params: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    },
    Builtin(fn(Vec<Object>) -> Object),
}

impl Object {
    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(it) => write!(f, "{}", it),
            Object::Boolean(it) => write!(f, "{}", it),
            Object::String(it) => write!(f, r#""{}""#, it),
            Object::Array(it) => {
                let s = it.iter().join(", ");
                write!(f, "[{}]", s)
            }
            Object::Hash(it) => {
                let s = it
                    .into_iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .join(", ");
                write!(f, "{{{}}}", s)
            }
            Object::Null => write!(f, "null"),
            Object::Return(it) => write!(f, "{}", it),
            Object::Error(it) => write!(f, "{}", it),
            Object::Let => write!(f, ""),
            Object::Function { params, body, .. } => {
                let params = params.iter().join(", ");
                write!(f, "fn({}) {{\n{}\n}}", params, body)
            }
            Object::LetFunction { params, body, .. } => {
                let params = params.iter().join(", ");
                write!(f, "fn({}) {{\n{}\n}}", params, body)
            }
            Object::Builtin(_) => write!(f, "builtin"),
        }
    }
}

// HashKey

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HashKey::Integer(it) => write!(f, "{}", it),
            HashKey::String(it) => write!(f, r#""{}""#, it),
            HashKey::Boolean(it) => write!(f, "{}", it),
        }
    }
}

impl convert::TryFrom<Object> for HashKey {
    type Error = (String, Object);

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Integer(it) => Ok(Self::Integer(it)),
            Object::String(it) => Ok(Self::String(it)),
            Object::Boolean(it) => Ok(Self::Boolean(it)),
            o => Err((format!("could not convert `{}` as HashKey", o), o)),
        }
    }
}

// environment

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let store = HashMap::new();
        Self { store, outer: None }
    }

    pub fn new_with_outer(outer: Environment) -> Self {
        let mut env = Self::new();
        env.outer = Some(Box::new(outer));
        env
    }

    pub fn get(&self, id: &ast::Identifier) -> Option<Object> {
        match self.store.get(&id.0) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => outer.get(&id),
                None => None,
            },
        }
    }

    pub fn set(&mut self, id: &ast::Identifier, value: Object) {
        let id = id.to_string();
        self.store.insert(id, value);
    }
}
