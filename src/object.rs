use std::fmt::{self};

// object enum

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
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

// object structs

// Integer
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer(pub i64);

impl std::convert::From<Integer> for Object {
    fn from(n: Integer) -> Self {
        Self::Integer(n)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// Boolean
#[derive(Debug, PartialEq, Eq)]
pub struct Boolean(pub bool);

impl std::convert::From<Boolean> for Object {
    fn from(b: Boolean) -> Self {
        Self::Boolean(b)
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
