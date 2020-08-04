use crate::ast::{self};
use crate::object::Object;

pub fn eval(program: ast::Program) -> Object {
    let stmts = program.statements;
    eval_statements(stmts)
}

fn eval_statements(stmts: Vec<ast::Statement>) -> Object {
    let mut res = Object::Null;
    for s in stmts {
        res = eval_statement(s);
    }
    res
}

fn eval_statement(stmt: ast::Statement) -> Object {
    match stmt {
        ast::Statement::Expression(expr) => eval_expression(expr),
        _ => Object::Null,
    }
}

fn eval_expression(expr: ast::Expression) -> Object {
    match expr {
        ast::Expression::Integer(n) => Object::Integer(n),
        ast::Expression::Boolean(b) => Object::Boolean(b),
        ast::Expression::Prefix { operator, right } => {
            let right = eval_expression(*right);
            eval_prefix_expression(operator, right)
        }
        _ => Object::Null,
    }
}

fn eval_prefix_expression(op: ast::PrefixOperator, right: Object) -> Object {
    match op {
        ast::PrefixOperator::Bang => eval_bang_operator_expresion(right),
        _ => Object::Null,
    }
}

fn eval_bang_operator_expresion(right: Object) -> Object {
    match right {
        Object::Boolean(b) => Object::Boolean(!b),
        Object::Null => true_object(),
        _ => false_object(),
    }
}

fn true_object() -> Object {
    Object::Boolean(true)
}

fn false_object() -> Object {
    Object::Boolean(false)
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            test_integer_object(v, expected);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            test_boolean_object(v, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
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

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(p) => eval(p),
            Err(e) => panic!(e),
        }
    }

    fn test_integer_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer(n) => assert_eq!(n, expected),
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean(b) => assert_eq!(b, expected),
            _ => panic!("object is not Boolean. got={:?}", obj),
        }
    }
}
