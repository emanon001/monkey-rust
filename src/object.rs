use crate::ast::{self};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{self};

// object enum

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Let(Box<Object>),
    Function {
        params: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    },
    String(String),
    Builtin(fn(Vec<Object>) -> Object),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Null => write!(f, "null"),
            Object::Return(v) => write!(f, "{}", v),
            Object::Error(v) => write!(f, "{}", v),
            Object::Let(v) => write!(f, "{}", v),
            Object::Function { params, body, .. } => {
                let params = params.iter().join(", ");
                write!(f, "fn({}) {{\n{}\n}}", params, body)
            }
            Object::String(s) => write!(f, r#""{}""#, s),
            Object::Builtin(_) => write!(f, "builtin"),
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
