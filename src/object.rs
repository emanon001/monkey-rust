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
        }
    }
}

// environment

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        let store = HashMap::new();
        Self { store }
    }

    pub fn get(&self, id: &ast::Identifier) -> Option<Object> {
        let id = id.to_string();
        self.store.get(&id).map(|v| v.clone())
    }

    pub fn set(&mut self, id: &ast::Identifier, value: Object) {
        let id = id.to_string();
        self.store.insert(id, value);
    }
}
