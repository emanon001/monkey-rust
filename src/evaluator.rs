use crate::ast::{self};
use crate::object::{self as obj};

pub fn eval(program: ast::Program) -> obj::Object {
    eval_program(program)
}

fn eval_program(program: ast::Program) -> obj::Object {
    let stmts = program.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s);
        match res {
            obj::Object::Return(o) => {
                return *o;
            }
            obj::Object::Error(_) => {
                return res;
            }
            _ => {}
        }
    }
    res
}

fn eval_statement(stmt: ast::Statement) -> obj::Object {
    match stmt {
        ast::Statement::Expression(expr) => eval_expression(expr),
        ast::Statement::Block(block) => eval_block_statements(block),
        ast::Statement::Return(expr) => {
            let v = eval_expression(expr);
            if is_error_object(&v) {
                return v;
            }
            obj::Object::Return(Box::new(v))
        }
        _ => null_object(),
    }
}

fn eval_block_statements(block: ast::BlockStatement) -> obj::Object {
    let stmts = block.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s);
        match &res {
            obj::Object::Return(_) => {
                return res;
            }
            obj::Object::Error(_) => {
                return res;
            }
            _ => {}
        }
    }
    res
}

fn eval_expression(expr: ast::Expression) -> obj::Object {
    match expr {
        ast::Expression::Integer(n) => obj::Integer(n).into(),
        ast::Expression::Boolean(b) => obj::Boolean(b).into(),
        ast::Expression::Prefix { operator, right } => {
            let right = eval_expression(*right);
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
            let left = eval_expression(*left);
            if is_error_object(&left) {
                return left;
            }
            let right = eval_expression(*right);
            if is_error_object(&right) {
                return right;
            }
            eval_infix_expression(operator, left, right)
        }
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, consequence, alternative),
        _ => null_object(),
    }
}

fn eval_prefix_expression(op: ast::PrefixOperator, right: obj::Object) -> obj::Object {
    match op {
        ast::PrefixOperator::Bang => eval_bang_prefix_operator_expression(right),
        ast::PrefixOperator::Minus => eval_minus_prefix_operator_expression(right),
    }
}

fn eval_bang_prefix_operator_expression(right: obj::Object) -> obj::Object {
    match right {
        obj::Object::Boolean(b) => obj::Boolean(!b.0).into(),
        obj::Object::Null => true_object(),
        _ => false_object(),
    }
}

fn eval_minus_prefix_operator_expression(right: obj::Object) -> obj::Object {
    match right {
        obj::Object::Integer(obj::Integer(n)) => obj::Integer(-n).into(),
        r => new_error_object(&format!("unknown operator: -{}", r)),
    }
}

fn eval_infix_expression(
    op: ast::InfixOperator,
    left: obj::Object,
    right: obj::Object,
) -> obj::Object {
    match (left, right) {
        (obj::Object::Integer(l), obj::Object::Integer(r)) => {
            eval_integer_infix_expression(op, l, r)
        }
        (obj::Object::Boolean(l), obj::Object::Boolean(r)) => {
            eval_boolean_infix_expression(op, l, r)
        }
        (l, r) if l.object_type() != r.object_type() => {
            new_error_object(&format!("type mismatch: {} {} {}", l, op, r))
        }
        (l, r) => new_error_object(&format!("unknown operator: {} {} {}", l, op, r)),
    }
}

fn eval_integer_infix_expression(
    op: ast::InfixOperator,
    left: obj::Integer,
    right: obj::Integer,
) -> obj::Object {
    let left = left.0;
    let right = right.0;
    match op {
        ast::InfixOperator::Add => obj::Integer(left + right).into(),
        ast::InfixOperator::Sub => obj::Integer(left - right).into(),
        ast::InfixOperator::Mul => obj::Integer(left * right).into(),
        ast::InfixOperator::Div => obj::Integer(left / right).into(),
        ast::InfixOperator::LT => obj::Boolean(left < right).into(),
        ast::InfixOperator::GT => obj::Boolean(left > right).into(),
        ast::InfixOperator::Eq => obj::Boolean(left == right).into(),
        ast::InfixOperator::NotEq => obj::Boolean(left != right).into(),
    }
}

fn eval_boolean_infix_expression(
    op: ast::InfixOperator,
    left: obj::Boolean,
    right: obj::Boolean,
) -> obj::Object {
    let left = left.0;
    let right = right.0;
    match op {
        ast::InfixOperator::Eq => obj::Boolean(left == right).into(),
        ast::InfixOperator::NotEq => obj::Boolean(left != right).into(),
        _ => new_error_object(&format!("unknown operator: {} {} {}", left, op, right)),
    }
}

fn eval_if_expression(
    condition: ast::Expression,
    consequence: ast::BlockStatement,
    alternative: Option<ast::BlockStatement>,
) -> obj::Object {
    let condition = eval_expression(condition);
    if is_error_object(&condition) {
        return condition;
    }
    if is_truthy(condition) {
        eval_statement(consequence.into())
    } else if let Some(alternative) = alternative {
        eval_statement(alternative.into())
    } else {
        null_object()
    }
}

fn is_truthy(obj: obj::Object) -> bool {
    match obj {
        obj::Object::Null => false,
        obj::Object::Boolean(b) => b.0,
        _ => true,
    }
}

fn true_object() -> obj::Object {
    obj::Boolean(true).into()
}

fn false_object() -> obj::Object {
    obj::Boolean(false).into()
}

fn null_object() -> obj::Object {
    obj::Object::Null
}

fn new_error_object(s: &str) -> obj::Object {
    obj::Object::Error(s.into())
}

fn is_error_object(o: &obj::Object) -> bool {
    match o {
        obj::Object::Error(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, null_object};
    use crate::lexer::Lexer;
    use crate::object::{self as obj};
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
            assert_eq!(v, obj::Integer(expected).into());
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
            assert_eq!(v, obj::Boolean(expected).into());
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
            assert_eq!(v, obj::Boolean(expected).into());
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests: Vec<(&str, obj::Object)> = vec![
            ("if (true) { 10 }", obj::Integer(10).into()),
            ("if (false) { 10 }", null_object()),
            ("if (1) { 10 }", obj::Integer(10).into()),
            ("if (1 < 2) { 10 }", obj::Integer(10).into()),
            ("if (1 > 2) { 10 }", null_object()),
            ("if (1 > 2) { 10 } else { 20 }", obj::Integer(20).into()),
            ("if (1 < 2) { 10 } else { 20 }", obj::Integer(10).into()),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&str, obj::Object)> = vec![
            ("return 10;", obj::Integer(10).into()),
            ("return 10; 9;", obj::Integer(10).into()),
            ("return 2 * 5; 0", obj::Integer(10).into()),
            ("9; return 2 * 5; 9", obj::Integer(10).into()),
            ("9; return 2 * 5; 9", obj::Integer(10).into()),
            (
                r#"
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            "#,
                obj::Integer(10).into(),
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
            ("5 + true;", "type mismatch: 5 + true"),
            ("5 + true; 5;", "type mismatch: 5 + true"),
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
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, obj::Object::Error(expected.into()));
        }
    }

    #[test]
    fn eval_let_statements() {
        let tests: Vec<(&str, obj::Object)> = vec![
            ("let a = 5; a;", obj::Integer(5).into()),
            ("let a = 5 * 5; a;", obj::Integer(25).into()),
            ("let a = 5; let b = a; b;", obj::Integer(5).into()),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                obj::Integer(15).into(),
            ),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    // helpers

    fn test_eval(input: String) -> obj::Object {
        let lexer = Lexer::new(input);
        match parse(lexer) {
            Ok(p) => eval(p),
            Err(e) => panic!(format!("{}", e)),
        }
    }
}