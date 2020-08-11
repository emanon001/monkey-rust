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
                    let expr = args[0].clone();
                    eval(expr.into(), env).into()
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

// Object -> AST Node
impl std::convert::From<Object> for ast::Node {
    fn from(obj: Object) -> ast::Node {
        match obj {
            Object::Integer(it) => ast::Expression::Integer(it).into(),
            Object::Boolean(it) => ast::Expression::Boolean(it).into(),
            _ => ast::Expression::String("dummy".into()).into(),
        }
    }
}
