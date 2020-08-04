use std::collections::HashMap;
use std::fmt::{self};

// object enum

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Null => write!(f, "null"),
            Object::Return(v) => write!(f, "{}", v),
            Object::Error(v) => write!(f, "{}", v),
        }
    }
}

// environment

pub struct Environment {
    table: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        let table = HashMap::new();
        Self { table }
    }

    pub fn get(&self, id: &str) -> Option<&Object> {
        self.table.get(id)
    }

    pub fn insert(&mut self, id: String, value: Object) {
        self.table.insert(id, value);
    }
}
