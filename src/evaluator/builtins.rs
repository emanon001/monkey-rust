use crate::ast::Identifier;
use crate::object::Object;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

lazy_static! {
    static ref FUNCTION_MAP: HashMap<&'static str, BuiltinFunction> = {
        let mut map = HashMap::new();
        map.insert("len", len as BuiltinFunction);
        map.insert("first", first as BuiltinFunction);
        map.insert("last", last as BuiltinFunction);
        map.insert("rest", rest as BuiltinFunction);
        map.insert("push", push as BuiltinFunction);
        map.insert("puts", puts as BuiltinFunction);
        map
    };
}

pub fn get(id: &Identifier) -> Option<BuiltinFunction> {
    let id: &str = &id.0;
    FUNCTION_MAP.get(id).copied()
}

// functions

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_wrong_number_arguments_error(args.len(), 1);
    }

    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        Object::Array(array) => Object::Integer(array.len() as i64),
        o => new_not_supported_error("len", o),
    }
}

fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_wrong_number_arguments_error(args.len(), 1);
    }

    match &args[0] {
        Object::Array(array) => match array.first() {
            Some(f) => f.clone(),
            None => Object::Null,
        },
        o => new_not_supported_error("first", o),
    }
}

fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_wrong_number_arguments_error(args.len(), 1);
    }

    match &args[0] {
        Object::Array(array) => match array.last() {
            Some(f) => f.clone(),
            None => Object::Null,
        },
        o => new_not_supported_error("last", o),
    }
}

fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_wrong_number_arguments_error(args.len(), 1);
    }

    match &args[0] {
        Object::Array(array) => {
            if array.is_empty() {
                return Object::Null;
            }
            let v: Vec<Object> = array.into_iter().skip(1).cloned().collect();
            Object::Array(v)
        }
        o => new_not_supported_error("rest", o),
    }
}

fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return new_wrong_number_arguments_error(args.len(), 2);
    }

    match (&args[0], &args[1]) {
        (Object::Array(array), v) => {
            let mut a = array.clone();
            a.push(v.clone());
            Object::Array(a)
        }
        (o, _) => new_not_supported_error("push", o),
    }
}

fn puts(args: Vec<Object>) -> Object {
    for a in args {
        println!("{}", a);
    }
    Object::Null
}

// helpers

fn new_wrong_number_arguments_error(n: usize, expected: usize) -> Object {
    Object::Error(format!(
        "wrong number of arguments. got={}, want={}",
        n, expected
    ))
}

fn new_not_supported_error(fname: &str, o: &Object) -> Object {
    Object::Error(format!(
        "argument to `{}` not supported, got `{}`",
        fname, o
    ))
}

#[cfg(test)]
mod tests {
    use crate::ast::Identifier;
    use crate::evaluator::builtins::{get, BuiltinFunction};
    use crate::object::Object;

    #[test]
    fn len() {
        let len = test_get("len");
        // args, expected
        let tests = vec![
            (vec![new_string("")], new_integer(0)),
            (vec![new_string("abc")], new_integer(3)),
            (vec![new_string("abc def")], new_integer(7)),
            (vec![new_array(Vec::new())], new_integer(0)),
            (vec![new_array(vec![new_integer(1)])], new_integer(1)),
            (
                vec![new_array(vec![new_integer(1), new_integer(2)])],
                new_integer(2),
            ),
        ];
        for (args, expected) in tests {
            assert_eq!(len(args), expected);
        }
    }

    #[test]
    fn first() {
        let first = test_get("first");

        // args, expected
        let tests = vec![
            (vec![new_array(Vec::new())], new_null()),
            (vec![new_array(vec![new_string("a")])], new_string("a")),
            (
                vec![new_array(vec![new_integer(2), new_integer(4)])],
                new_integer(2),
            ),
        ];
        for (args, expected) in tests {
            assert_eq!(first(args), expected);
        }
    }

    #[test]
    fn last() {
        let last = test_get("last");
        // args, expected
        let tests = vec![
            (vec![new_array(Vec::new())], new_null()),
            (vec![new_array(vec![new_string("a")])], new_string("a")),
            (
                vec![new_array(vec![new_integer(2), new_integer(4)])],
                new_integer(4),
            ),
        ];
        for (args, expected) in tests {
            assert_eq!(last(args), expected);
        }
    }

    #[test]
    fn rest() {
        let rest = test_get("rest");
        // args, expected
        let tests = vec![
            (vec![new_array(Vec::new())], new_null()),
            (vec![new_array(vec![new_string("a")])], new_array(vec![])),
            (
                vec![new_array(vec![new_integer(2), new_integer(4)])],
                new_array(vec![new_integer(4)]),
            ),
            (
                vec![new_array(vec![
                    new_integer(2),
                    new_integer(4),
                    new_integer(5),
                ])],
                new_array(vec![new_integer(4), new_integer(5)]),
            ),
        ];
        for (args, expected) in tests {
            assert_eq!(rest(args), expected);
        }
    }

    #[test]
    fn push() {
        let push = test_get("push");
        // args, expected
        let tests = vec![
            (
                vec![new_array(Vec::new()), new_integer(1)],
                new_array(vec![new_integer(1)]),
            ),
            (
                vec![new_array(vec![new_string("a")]), new_string("b")],
                new_array(vec![new_string("a"), new_string("b")]),
            ),
        ];
        for (args, expected) in tests {
            assert_eq!(push(args), expected);
        }
    }

    #[test]
    fn puts() {
        let puts = test_get("puts");
        // args, expected
        let tests = vec![
            (vec![], new_null()),
            (vec![new_integer(1), new_integer(2)], new_null()),
        ];
        for (args, expected) in tests {
            assert_eq!(puts(args), expected);
        }
    }

    // helpers

    fn new_id(s: &str) -> Identifier {
        Identifier(s.into())
    }

    fn new_string(s: &str) -> Object {
        Object::String(s.into())
    }

    fn new_integer(n: i64) -> Object {
        Object::Integer(n)
    }

    fn new_array(a: Vec<Object>) -> Object {
        Object::Array(a)
    }

    fn new_null() -> Object {
        Object::Null
    }

    fn test_get(id: &str) -> BuiltinFunction {
        let f = get(&new_id(id));
        assert!(f.is_some());
        f.unwrap()
    }
}
