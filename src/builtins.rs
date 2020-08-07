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
        map
    };
}

pub fn get(id: &Identifier) -> Option<BuiltinFunction> {
    let id: &str = &id.0;
    FUNCTION_MAP.get(id).copied()
}

fn len(args: Vec<Object>) -> Object {
    // len(o: String | Array) -> Integer
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        Object::Array(array) => Object::Integer(array.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got `{}`", o)),
    }
}

fn first(args: Vec<Object>) -> Object {
    // len(o: String | Array) -> Integer
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => match array.first() {
            Some(f) => f.clone(),
            None => Object::Null,
        },
        o => Object::Error(format!("argument to `len` not supported, got `{}`", o)),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Identifier;
    use crate::builtins::get;
    use crate::object::Object;

    #[test]
    fn test_len() {
        let len = get(&new_id("len"));
        assert!(len.is_some());
        let len = len.unwrap();

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
    fn test_first() {
        let first = get(&new_id("first"));
        assert!(first.is_some());
        let first = first.unwrap();

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
}
