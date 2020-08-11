mod builtins;
mod quote_unquote;
use crate::ast::{self};
use crate::evaluator::quote_unquote::quote;
use crate::object::{Environment, HashKey, Object};
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;

// eval

pub fn eval(node: ast::Node, env: &mut Environment) -> Object {
    match node {
        ast::Node::Program(it) => eval_program(it, env),
        ast::Node::Statement(it) => eval_statement(it, env),
        ast::Node::Expression(it) => eval_expression(it, env),
    }
}

fn eval_program(program: ast::Program, env: &mut Environment) -> Object {
    let stmts = program.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s, env);
        match res {
            Object::Return(_) => {
                return unwrap_return_value(res);
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
        ast::Statement::Return(expr) => eval_return_statement(expr, env),
        ast::Statement::Let {
            identifier,
            expression,
        } => eval_let_statement(identifier, expression, env),
        ast::Statement::Block(it) => eval_block_statement(it, env),
    }
}

fn eval_return_statement(expr: ast::Expression, env: &mut Environment) -> Object {
    let v = eval_expression(expr, env);
    if v.is_error() {
        return v;
    }
    Object::Return(Box::new(v))
}

fn eval_let_statement(id: ast::Identifier, expr: ast::Expression, env: &mut Environment) -> Object {
    match expr {
        ast::Expression::Function(f) => eval_let_function_statement(id, f, env),
        expr => {
            let obj = eval_expression(expr, env);
            if obj.is_error() {
                return obj;
            }
            env.set(&id, obj.clone());
            Object::Let
        }
    }
}

fn eval_let_function_statement(
    id: ast::Identifier,
    f: ast::FunctionExpression,
    env: &mut Environment,
) -> Object {
    // let <id> = <function>
    // set <id> in `env` for recursive calls
    let rf = eval_let_function_expression(id.clone(), f.clone(), env);
    let mut fenv = env.clone();
    fenv.set(&id, rf);

    let f = eval_function_expression(f, &mut fenv);
    env.set(&id, f.clone());
    Object::Let
}

fn eval_block_statement(block: ast::BlockStatement, env: &mut Environment) -> Object {
    let mut env = Environment::new_with_outer(env.clone());
    let stmts = block.statements;
    let mut res = null_object();
    for s in stmts {
        res = eval_statement(s, &mut env);
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

fn eval_expression(expr: ast::Expression, env: &mut Environment) -> Object {
    match expr {
        ast::Expression::Integer(it) => eval_integer_expression(it),
        ast::Expression::Boolean(it) => eval_boolean_expression(it),
        ast::Expression::String(it) => eval_string_expression(it),
        ast::Expression::Array(it) => eval_array_expression(it, env),
        ast::Expression::Hash(it) => eval_hash_expression(it, env),
        ast::Expression::Prefix { operator, right } => {
            eval_prefix_expression(operator, *right, env)
        }
        ast::Expression::Infix {
            left,
            operator,
            right,
        } => eval_infix_expression(operator, *left, *right, env),
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if_expression(*condition, consequence, alternative, env),
        ast::Expression::Identifier(id) => eval_identifier_expression(id, env),
        ast::Expression::Function(expr) => eval_function_expression(expr, env),
        ast::Expression::Call { function, args } => eval_call_expression(function, args, env),
        ast::Expression::Index { left, index } => eval_index_expression(*left, *index, env),
    }
}

fn eval_expressions(
    exprs: Vec<ast::Expression>,
    env: &mut Environment,
) -> Result<Vec<Object>, Object> {
    let mut res = Vec::new();
    for expr in exprs {
        let v = eval_expression(expr, env);
        if v.is_error() {
            return Err(v);
        }
        res.push(v);
    }
    Ok(res)
}

fn eval_integer_expression(n: i64) -> Object {
    Object::Integer(n)
}

fn eval_boolean_expression(b: bool) -> Object {
    Object::Boolean(b)
}

fn eval_string_expression(s: String) -> Object {
    Object::String(s)
}

fn eval_array_expression(exprs: Vec<ast::Expression>, env: &mut Environment) -> Object {
    let mut elements = Vec::new();
    for expr in exprs {
        let v = eval_expression(expr, env);
        if v.is_error() {
            return v;
        }
        elements.push(v);
    }
    Object::Array(elements)
}

fn eval_hash_expression(
    map: BTreeMap<ast::Expression, ast::Expression>,
    env: &mut Environment,
) -> Object {
    let mut hash = HashMap::new();
    for (k, v) in map {
        let key = eval_expression(k, env);
        if key.is_error() {
            return key;
        }
        let key = match HashKey::try_from(key) {
            Ok(it) => it,
            Err((_, o)) => return new_error_object(&format!("unusable as hash key: `{}`", o)),
        };
        let value = eval_expression(v, env);
        if value.is_error() {
            return value;
        }
        hash.insert(key, value);
    }
    Object::Hash(hash)
}

fn eval_prefix_expression(
    op: ast::PrefixOperator,
    right: ast::Expression,
    env: &mut Environment,
) -> Object {
    let right = eval_expression(right, env);
    if right.is_error() {
        return right;
    }
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
        r => new_error_object(&format!("unknown operator: `-{}`", r)),
    }
}

fn eval_infix_expression(
    op: ast::InfixOperator,
    left: ast::Expression,
    right: ast::Expression,
    env: &mut Environment,
) -> Object {
    let left = eval_expression(left, env);
    if left.is_error() {
        return left;
    }
    let right = eval_expression(right, env);
    if right.is_error() {
        return right;
    }

    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(op, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(op, l, r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(op, l, r),
        (l, r) => new_error_object(&format!("unknown operator: `{} {} {}`", l, op, r)),
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
        _ => new_error_object(&format!("unknown operator: `{} {} {}`", left, op, right)),
    }
}

fn eval_string_infix_expression(op: ast::InfixOperator, left: String, right: String) -> Object {
    match op {
        ast::InfixOperator::Add => Object::String(left + &right),
        ast::InfixOperator::Eq => Object::Boolean(left == right),
        ast::InfixOperator::NotEq => Object::Boolean(left != right),
        _ => new_error_object(&format!("unknown operator: `{} {} {}`", left, op, right)),
    }
}

fn eval_if_expression(
    condition: ast::Expression,
    consequence: ast::BlockStatement,
    alternative: Option<ast::BlockStatement>,
    env: &mut Environment,
) -> Object {
    let condition = eval_expression(condition, env);
    if condition.is_error() {
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

fn eval_identifier_expression(id: ast::Identifier, env: &Environment) -> Object {
    if let Some(v) = env.get(&id) {
        v
    } else if let Some(f) = builtins::get(&id) {
        Object::Builtin(f)
    } else {
        new_error_object(&format!("identifier not found: `{}`", id))
    }
}

fn eval_function_expression(expr: ast::FunctionExpression, env: &Environment) -> Object {
    let params = expr.params;
    let body = expr.body;
    let env = env.clone();
    Object::Function { params, body, env }
}

fn eval_let_function_expression(
    id: ast::Identifier,
    expr: ast::FunctionExpression,
    env: &Environment,
) -> Object {
    let params = expr.params;
    let body = expr.body;
    let env = env.clone();
    Object::LetFunction {
        id,
        params,
        body,
        env,
    }
}

fn eval_call_expression(
    f: ast::CallExpressionFunction,
    args: Vec<ast::Expression>,
    env: &mut Environment,
) -> Object {
    if let ast::CallExpressionFunction::Identifier(id) = &f {
        if id.to_string() == "quote" {
            // TODO: check args len
            return match quote(args[0].clone().into()) {
                Ok(quoted) => quoted,
                Err(e) => new_error_object(&e),
            };
        }
    }
    let f = eval_expression(f.into(), env);
    if f.is_error() {
        return f;
    }
    match eval_expressions(args, env) {
        Ok(args) => match f {
            Object::Function { body, params, env } => {
                let mut env = extend_function_env(env, params, args);
                unwrap_return_value(eval_block_statement(body, &mut env))
            }
            Object::LetFunction {
                id,
                body,
                params,
                mut env,
            } => {
                // let f = fn(n) { if (n == 0) { 1 } else { n * f(n - 1) } };
                //                                              ^^^^^^^^
                // set <id> in `env` for recursive calls
                env.set(
                    &id,
                    Object::LetFunction {
                        id: id.clone(),
                        body: body.clone(),
                        params: params.clone(),
                        env: env.clone(),
                    },
                );
                let mut env = extend_function_env(env, params, args);
                unwrap_return_value(eval_block_statement(body, &mut env))
            }
            Object::Builtin(f) => f(args),
            _ => new_error_object(&format!("not a function: `{}`", f)),
        },
        Err(v) => v,
    }
}

fn extend_function_env(
    env: Environment,
    params: Vec<ast::Identifier>,
    args: Vec<Object>,
) -> Environment {
    let mut env = Environment::new_with_outer(env);
    for (param, arg) in params.into_iter().zip(args) {
        env.set(&param, arg);
    }
    env
}

fn eval_index_expression(
    left: ast::Expression,
    index: ast::Expression,
    env: &mut Environment,
) -> Object {
    let left = eval_expression(left, env);
    if left.is_error() {
        return left;
    }
    let index = eval_expression(index, env);
    if index.is_error() {
        return index;
    }
    match (left, index) {
        (Object::Array(array), Object::Integer(idx)) => eval_array_index_expression(array, idx),
        (Object::Hash(hash), idx) => eval_hash_index_expression(hash, idx),
        (l, _) => new_error_object(&format!("index operator not supported: `{}`", l)),
    }
}

fn eval_array_index_expression(array: Vec<Object>, idx: i64) -> Object {
    if idx < 0 || idx >= array.len() as i64 {
        return null_object();
    }
    let idx = idx as usize;
    array[idx].clone()
}

fn eval_hash_index_expression(hash: HashMap<HashKey, Object>, idx: Object) -> Object {
    let idx = match HashKey::try_from(idx) {
        Ok(it) => it,
        Err((_, o)) => return new_error_object(&format!("unusable as hash key: `{}`", o)),
    };
    hash.get(&idx).cloned().unwrap_or(null_object())
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(v) => *v,
        _ => obj,
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

#[cfg(test)]
mod tests {
    use super::{eval, null_object, Environment};
    use crate::lexer::Lexer;
    use crate::object::{HashKey, Object};
    use crate::parser::parse;
    use std::collections::HashMap;

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
            (r#""foobar" == "foobar""#, true),
            (r#""foobar" == "foo""#, false),
            (r#""foo" == "foobar""#, false),
            (r#""" == """#, true),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Boolean(expected));
        }
    }

    #[test]
    fn eval_string_expression() {
        let tests = vec![
            (r#""foobar""#, "foobar"),
            (r#""foo bar""#, "foo bar"),
            (r#""""#, ""),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::String(expected.into()));
        }
    }

    #[test]
    fn eval_string_concatenation() {
        let tests = vec![
            (r#""Hello" + " " + "World!""#, "Hello World!"),
            (r#""Hello" + """#, "Hello"),
            (r#""" + "World!""#, "World!"),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::String(expected.into()));
        }
    }

    #[test]
    fn eval_array_expression() {
        let input = r#"
        [1, 2 * 2, 3 + 3]
        "#;
        let v = test_eval(input.into());
        assert_eq!(
            v,
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ])
        );
    }

    #[test]
    fn eval_array_index_expressio() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn eval_hash_expression() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#;
        let expected = vec![
            (HashKey::String("one".into()), new_int(1)),
            (HashKey::String("two".into()), new_int(2)),
            (HashKey::String("three".into()), new_int(3)),
            (HashKey::Integer(4), new_int(4)),
            (HashKey::Boolean(true), new_int(5)),
            (HashKey::Boolean(false), new_int(6)),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>();
        let v = test_eval(input.into());
        assert_eq!(v, Object::Hash(expected));
    }

    #[test]
    fn eval_hash_index_expressio() {
        let tests = vec![
            (r#"{"foo": 5}["foo"]"#, new_int(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, new_int(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, new_int(5)),
            (r#"{true: 5}[true]"#, new_int(5)),
            (r#"{false: 5}[false]"#, new_int(5)),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn eval_bang_expression() {
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
    fn eval_if_else_expression() {
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
    fn eval_return_statements() {
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
    fn error_handling() {
        let tests = vec![
            ("5 + true;", "unknown operator: `5 + true`"),
            ("5 + true; 5;", "unknown operator: `5 + true`"),
            ("-true", "unknown operator: `-true`"),
            ("true + false", "unknown operator: `true + false`"),
            ("5; true + false; 5;", "unknown operator: `true + false`"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: `true + false`",
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
                "unknown operator: `true + false`",
            ),
            ("foobar", "identifier not found: `foobar`"),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, Object::Error(expected.into()));
        }
    }

    #[test]
    fn eval_let_statement() {
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

    #[test]
    fn create_env_in_if_block_statement() {
        let tests: Vec<(&str, Object)> = vec![
            (r#"let a = 1; if (true) { let a = 2; } a;"#, new_int(1)),
            (
                r#"let a = 1; if (false) { } else { let a = 2; } a;"#,
                new_int(1),
            ),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn eval_function_object() {
        let input = r#"
        fn(x) {
            x + 2;
        };
        "#;
        let v = test_eval(input.into());
        match v {
            Object::Function { params, body, .. } => {
                // params
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                // body
                assert_eq!(body.to_string(), "(x + 2)");
            }
            _ => panic!("object is not function. got={:?}", v),
        }
    }

    #[test]
    fn eval_function_call_expression() {
        let tests: Vec<(&str, Object)> = vec![
            (
                "let identity = fn(x) { x }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            (
                // closure
                r#"
                let f = fn(x) {
                    fn(y) { x + y };
                };
                let g = f(2);
                g(2);
                "#,
                Object::Integer(4),
            ),
            ("fn(x) { x; }(5);", Object::Integer(5)),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn eval_builtin_function_expression() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (
                r#"len(1)"#,
                Object::Error("argument to `len` not supported, got `1`".into()),
            ),
            (
                r#"len("one", "two")"#,
                Object::Error("wrong number of arguments. got=2, want=1".into()),
            ),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            assert_eq!(v, expected);
        }
    }

    #[test]
    fn eval_recursive_function() {
        let input = r#"
        let factorial = fn(n) {
            if (n == 1) {
                1
            } else {
                n * factorial(n - 1)
            }
        };
        factorial(10);
        "#;
        let v = test_eval(input.into());
        assert_eq!(v, Object::Integer(3628800));
    }

    #[test]
    fn eval_let_function_call() {
        let input = r#"
        let f = fn(n) {
            if (n == 1) {
                1
            } else {
                n * f(n - 1)
            }
        };
        let g = f;
        let f = 1;
        g(10);
        "#;
        let v = test_eval(input.into());
        assert_eq!(v, Object::Integer(3628800));
    }

    #[test]
    fn eval_quote() {
        let tests = vec![
            (r#"quote(5)"#, r#"5"#),
            (r#"quote(foobar)"#, r#"foobar"#),
            (r#"quote(foobar + barfoo)"#, r#"(foobar + barfoo)"#),
        ];
        for (input, expected) in tests {
            let v = test_eval(input.into());
            match v {
                Object::Quote(it) => assert_eq!(it.to_string(), expected),
                _ => panic!("object is not quote. got={:?}", v),
            }
        }
    }

    #[test]
    fn eval_array_map_function() {
        let input = r#"
        let map = fn(arr, f) {
            let iter = fn(arr, accumulated) {
                if (len(arr) == 0) {
                    accumulated
                } else {
                    iter(rest(arr), push(accumulated, f(first(arr))));
                }
            };
            iter(arr, []);
        };
        let a = [1, 2, 3, 4];
        let double = fn(x) { x * 2 };
        map(a, double);
        "#;
        let v = test_eval(input.into());
        assert_eq!(
            v,
            Object::Array(vec![
                Object::Integer(2),
                Object::Integer(4),
                Object::Integer(6),
                Object::Integer(8)
            ])
        );
    }

    #[test]
    fn eval_array_reduce_function() {
        let input = r#"
        let reduce = fn(arr, initial, f) {
            let iter = fn(arr, result) {
                if (len(arr) == 0) {
                    result
                } else {
                    iter(rest(arr), f(result, first(arr)));
                }
            };
            iter(arr, initial);
        };
        let sum = fn(arr) {
            reduce(arr, 0, fn(initial, el) { initial + el });
        };
        sum([1, 2, 3, 4, 5]);
        "#;
        let v = test_eval(input.into());
        assert_eq!(v, Object::Integer(15));
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

    fn new_int(n: i64) -> Object {
        Object::Integer(n)
    }
}
