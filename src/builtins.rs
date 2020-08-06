use crate::ast::Identifier;
use crate::object::Object;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

pub fn get(id: &Identifier) -> Option<BuiltinFunction> {
    let id: &str = &id.0;
    match id {
        "len" => Some(len),
        _ => None,
    }
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
