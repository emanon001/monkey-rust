use crate::ast::{self};
use crate::object::{self as obj};

pub fn eval(program: ast::Program) -> obj::Object {
    let stmts = program.statements;
    eval_statements(stmts)
}

fn eval_statements(stmts: Vec<ast::Statement>) -> obj::Object {
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s);
    }
    res
}

fn eval_statement(stmt: ast::Statement) -> obj::Object {
    match stmt {
        ast::Statement::Expression(expr) => eval_expression(expr),
        _ => null_object(),
    }
}

fn eval_expression(expr: ast::Expression) -> obj::Object {
    match expr {
        ast::Expression::Integer(n) => obj::Integer(n).into(),
        ast::Expression::Boolean(b) => obj::Boolean(b).into(),
        ast::Expression::Prefix { operator, right } => {
            let right = eval_expression(*right);
            eval_prefix_expression(operator, right)
        }
        ast::Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left);
            let right = eval_expression(*right);
            eval_infix_expression(operator, left, right)
        }
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
        obj::Object::Null(_) => true_object(),
        _ => false_object(),
    }
}

fn eval_minus_prefix_operator_expression(right: obj::Object) -> obj::Object {
    match right {
        obj::Object::Integer(obj::Integer(n)) => obj::Integer(-n).into(),
        _ => null_object(),
    }
}

fn eval_infix_expression(
    op: ast::InfixOperator,
    left: obj::Object,
    right: obj::Object,
) -> obj::Object {
    match (left, right) {
        (obj::Object::Integer(a), obj::Object::Integer(b)) => {
            eval_integer_infix_expression(op, a, b)
        }
        _ => null_object(),
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

fn true_object() -> obj::Object {
    obj::Boolean(true).into()
}

fn false_object() -> obj::Object {
    obj::Boolean(false).into()
}

fn null_object() -> obj::Object {
    obj::Null.into()
}

#[cfg(test)]
mod tests {
    use super::eval;
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
            test_integer_object(v, expected);
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
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            test_boolean_object(v, expected);
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
            test_boolean_object(v, expected);
        }
    }

    // helpers

    fn test_eval(input: String) -> obj::Object {
        let lexer = Lexer::new(input);
        match parse(lexer) {
            Ok(p) => eval(p),
            Err(e) => panic!(e),
        }
    }

    fn test_integer_object(obj: obj::Object, expected: i64) {
        match obj {
            obj::Object::Integer(n) => assert_eq!(n.0, expected),
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }

    fn test_boolean_object(obj: obj::Object, expected: bool) {
        match obj {
            obj::Object::Boolean(b) => assert_eq!(b.0, expected),
            _ => panic!("object is not Boolean. got={:?}", obj),
        }
    }
}
