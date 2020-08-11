use crate::ast::{self};
use crate::object::Object;

pub fn quote(expr: ast::Expression) -> Object {
    Object::Quote(expr)
}
