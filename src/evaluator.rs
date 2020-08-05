use crate::ast::{self};
use crate::object::Object;
use std::collections::HashMap;

// environment

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

// eval

pub fn eval(program: ast::Program, env: &mut Environment) -> Object {
    eval_program(program, env)
}

fn eval_program(program: ast::Program, env: &mut Environment) -> Object {
    let stmts = program.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s, env);
        match res {
            Object::Return(o) => {
                return *o;
            }
            Object::Error(_) => {
                return res;
            }
            _ => {}
        }
    }
    res
}

fn eval_statement(stmt: ast::Statement, env: &mut Environment) -> Object {
    match stmt {
        ast::Statement::Expression(expr) => eval_expression(expr, env),
        ast::Statement::Return(expr) => {
            let v = eval_expression(expr, env);
            if is_error_object(&v) {
                return v;
            }
            Object::Return(Box::new(v))
        }
        ast::Statement::Let {
            identifier,
            expression,
        } => {
            let expr = eval_expression(expression, env);
            if is_error_object(&expr) {
                return expr;
            }
            env.set(&identifier, expr.clone());
            Object::Let(Box::new(expr))
        }
    }
}

fn eval_expression(expr: ast::Expression, env: &mut Environment) -> Object {
    match expr {
        ast::Expression::Integer(n) => Object::Integer(n),
        ast::Expression::Boolean(b) => Object::Boolean(b),
        ast::Expression::Prefix { operator, right } => {
            let right = eval_expression(*right, env);
            if is_error_object(&right) {
                return right;
            }
            eval_prefix_expression(operator, right)
        }
        ast::Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left, env);
            if is_error_object(&left) {
                return left;
            }
            let right = eval_expression(*right, env);
            if is_error_object(&right) {
                return right;
            }
            eval_infix_expression(operator, left, right)
        }
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, consequence, alternative, env),
        ast::Expression::Identifier(id) => eval_identifier_expression(id, env),
        _ => null_object(),
    }
}

fn eval_prefix_expression(op: ast::PrefixOperator, right: Object) -> Object {
    match op {
        ast::PrefixOperator::Bang => eval_bang_prefix_operator_expression(right),
        ast::PrefixOperator::Minus => eval_minus_prefix_operator_expression(right),
    }
}

fn eval_bang_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(b) => Object::Boolean(!b),
        Object::Null => true_object(),
        _ => false_object(),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(-n).into(),
        r => new_error_object(&format!("unknown operator: -{}", r)),
    }
}

fn eval_infix_expression(op: ast::InfixOperator, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(op, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(op, l, r),
        (l, r) => new_error_object(&format!("unknown operator: {} {} {}", l, op, r)),
    }
}

fn eval_integer_infix_expression(op: ast::InfixOperator, left: i64, right: i64) -> Object {
    match op {
        ast::InfixOperator::Add => Object::Integer(left + right),
        ast::InfixOperator::Sub => Object::Integer(left - right),
        ast::InfixOperator::Mul => Object::Integer(left * right),
        ast::InfixOperator::Div => Object::Integer(left / right),
        ast::InfixOperator::LT => Object::Boolean(left < right),
        ast::InfixOperator::GT => Object::Boolean(left > right),
        ast::InfixOperator::Eq => Object::Boolean(left == right),
        ast::InfixOperator::NotEq => Object::Boolean(left != right),
    }
}

fn eval_boolean_infix_expression(op: ast::InfixOperator, left: bool, right: bool) -> Object {
    match op {
        ast::InfixOperator::Eq => Object::Boolean(left == right),
        ast::InfixOperator::NotEq => Object::Boolean(left != right),
        _ => new_error_object(&format!("unknown operator: {} {} {}", left, op, right)),
    }
}

fn eval_if_expression(
    condition: ast::Expression,
    consequence: ast::BlockStatement,
    alternative: Option<ast::BlockStatement>,
    env: &mut Environment,
) -> Object {
    let condition = eval_expression(condition, env);
    if is_error_object(&condition) {
        return condition;
    }
    if is_truthy(condition) {
        eval_block_statement(consequence, env)
    } else if let Some(alternative) = alternative {
        eval_block_statement(alternative, env)
    } else {
        null_object()
    }
}

fn eval_block_statement(block: ast::BlockStatement, env: &mut Environment) -> Object {
    let stmts = block.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s, env);
        match &res {
            Object::Return(_) => {
                return res;
            }
            Object::Error(_) => {
                return res;
            }
            _ => {}
        }
    }
    res
}

fn eval_identifier_expression(id: ast::Identifier, env: &mut Environment) -> Object {
    if let Some(v) = env.get(&id) {
        v
    } else {
        new_error_object(&format!("identifier not found: {}", id))
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(v) => v,
        _ => true,
    }
}

fn true_object() -> Object {
    Object::Boolean(true)
}

fn false_object() -> Object {
    Object::Boolean(false)
}

fn null_object() -> Object {
    Object::Null
}

fn new_error_object(s: &str) -> Object {
    Object::Error(s.into())
}

fn is_error_object(o: &Object) -> bool {
    match o {
        Object::Error(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, null_object, Environment};
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::parse;

    #[test]
    fn eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Integer(expected));
        }
    }

    #[test]
    fn eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_bang_prefix_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Boolean(expected));
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests: Vec<(&str, Object)> = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", null_object()),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", null_object()),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, Object)> = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 0", Object::Integer(10)),
            ("9; return 2 * 5; 9", Object::Integer(10)),
            ("9; return 2 * 5; 9", Object::Integer(10)),
            (
                r#"
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            "#,
                Object::Integer(10),
            ),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected.into());
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "unknown operator: 5 + true"),
            ("5 + true; 5;", "unknown operator: 5 + true"),
            ("-true", "unknown operator: -true"),
            ("true + false", "unknown operator: true + false"),
            ("5; true + false; 5;", "unknown operator: true + false"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: true + false",
            ),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                "#,
                "unknown operator: true + false",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Error(expected.into()));
        }
    }

    #[test]
    fn eval_let_statements() {
        let tests: Vec<(&str, Object)> = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    // helpers

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut env = Environment::new();
        match parse(lexer) {
            Ok(p) => eval(p, &mut env),
            Err(e) => panic!(format!("{}", e)),
        }
    }
}
