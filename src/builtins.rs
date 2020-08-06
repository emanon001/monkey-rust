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
