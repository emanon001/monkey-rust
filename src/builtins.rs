use crate::ast::Identifier;
use crate::object::Object;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

lazy_static! {
    static ref FUNCTION_MAP: HashMap<&'static str, BuiltinFunction> = {
        let mut map = HashMap::new();
        map.insert("len", len as BuiltinFunction);
        map
    };
}

pub fn get(id: &Identifier) -> Option<BuiltinFunction> {
    let id: &str = &id.0;
    FUNCTION_MAP.get(id).copied()
}

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got `{}`", o)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_len() {
        // args, expected
        let tests = vec![
            (vec![new_string("")], new_integer(0)),
            (vec![new_string("abc")], new_integer(3)),
            (vec![new_string("abc def")], new_integer(7)),
        ];
        for (args, expected) in tests {
            assert_eq!(len(args), expected);
        }
    }

    // helpers

    fn new_string(s: &str) -> Object {
        Object::String(s.into())
    }

    fn new_integer(n: i64) -> Object {
        Object::Integer(n)
    }
}
