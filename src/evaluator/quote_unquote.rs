use crate::ast::{self};
use crate::object::Object;

pub fn quote(node: ast::Node) -> Object {
    Object::Quote(node)
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Node, Statement};
    use crate::evaluator::quote_unquote::quote;
    use crate::object::Object;

    #[test]
    fn quote_node() {
        let tests: Vec<Node> = vec![
            Expression::Integer(1).into(),
            Statement::Expression(Expression::Integer(2)).into(),
        ];
        for node in tests {
            let expected = Object::Quote(node.clone());
            assert_eq!(quote(node), expected);
        }
    }
}
