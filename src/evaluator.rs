use crate::ast::{self};
use crate::object::Object;

pub fn eval(_: ast::Program) -> Object {
    Object::Null
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn eval_integer_expression() {
        let cases = vec![("5", 5), ("10", 10)];
        for (input, expected) in cases {
            let v = test_eval(input.into());
            test_integer_object(v, expected);
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
}
