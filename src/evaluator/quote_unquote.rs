use crate::ast::modify::modify as ast_modify;
use crate::ast::{self};
use crate::evaluator::eval;
use crate::object::{Environment, Object};

pub type Error = String;
pub type Result<T> = std::result::Result<T, Error>;

pub fn quote(node: ast::Node, env: &mut Environment) -> Result<Object> {
    let node = eval_unquote_calls(node, env)?;
    Ok(Object::Quote(node))
}

fn eval_unquote_calls(quoted: ast::Node, env: &mut Environment) -> Result<ast::Node> {
    ast_modify(quoted, |node| match node {
        ast::Node::Expression(expr) => match expr {
            ast::Expression::Unquote(expr) => {
                let expr = *expr;
                eval(expr.into(), env).into()
            }
            other => other.into(),
        },
        _ => node,
    })
}

// Object -> AST Node
impl std::convert::From<Object> for ast::Node {
    fn from(obj: Object) -> ast::Node {
        match obj {
            Object::Integer(it) => ast::Expression::Integer(it).into(),
            Object::Boolean(it) => ast::Expression::Boolean(it).into(),
            Object::Quote(node) => node,
            _ => panic!("not implemented"),
        }
    }
}
