use crate::ast::{self};
use crate::object::Object;

pub type Error = String;
pub type Result<T> = std::result::Result<T, Error>;

pub fn quote(node: ast::Node) -> Result<Object> {
    let node = eval_unquote_calls(node)?;
    Ok(Object::Quote(node))
}

pub fn eval_unquote_calls(quoted: ast::Node) -> Result<ast::Node> {
    ast::modify(quoted, |node| {
        if !is_unquote_call(&node) {
            return node;
        }
        match &node {
            ast::Node::Expression(expr) => match expr {
                ast::Expression::Call { args, .. } => {
                    if args.len() != 1 {
                        return node;
                    }
                    // TODO
                    node
                }
                _ => node,
            },
            _ => node,
        }
    })
}

fn is_unquote_call(node: &ast::Node) -> bool {
    match node {
        ast::Node::Expression(expr) => match expr {
            ast::Expression::Call { function, .. } => match function {
                ast::CallExpressionFunction::Identifier(id) => id.0 == "unquote",
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Node, Statement};
    use crate::evaluator::quote_unquote::quote;
    use crate::object::Object;
}
