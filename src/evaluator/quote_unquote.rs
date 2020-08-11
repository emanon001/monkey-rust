use crate::ast::{self};
use crate::object::Object;

pub fn quote(node: ast::Node) -> Object {
    Object::Quote(node)
}

#[cfg(test)]
mod tests {
    #[test]
    fn quote() {
        // TODO
    }
}
