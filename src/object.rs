use std::fmt::{self};

// object enum

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Null(v) => write!(f, "{}", v),
        }
    }
}

// object structs

// Integer
#[derive(Debug, PartialEq, Eq)]
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

// Null
#[derive(Debug, PartialEq, Eq)]
pub struct Null;

impl std::convert::From<Null> for Object {
    fn from(null: Null) -> Self {
        Self::Null(null)
    }
}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}
