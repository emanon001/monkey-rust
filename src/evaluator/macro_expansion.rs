use crate::ast::{self};
use crate::evaluator::eval;
use crate::object::{Environment, Object};

pub type Error = String;
pub type Result<T> = std::result::Result<T, Error>;

// define_macros

pub fn define_macros(prog: ast::Program, env: &mut Environment) -> ast::Program {
    let mut macro_excluded_statements = Vec::new();
    for stmt in prog.statements {
        if let ast::Statement::Let {
            identifier,
            expression: ast::Expression::Macro(m),
        } = stmt
        {
            add_macro(identifier, m, env);
        } else {
            macro_excluded_statements.push(stmt);
        }
    }
    ast::Program {
        statements: macro_excluded_statements,
    }
}

fn add_macro(id: ast::Identifier, m: ast::MacroExpression, env: &mut Environment) {
    let ast::MacroExpression { params, body } = m;
    let m = Object::Macro {
        params,
        body,
        env: env.clone(),
    };
    env.set(&id, m);
}

// expand_macros

pub fn expand_macros(prog: ast::Program, env: &Environment) -> Result<ast::Program> {
    let node = ast::modify(prog.into(), |node| match &node {
        ast::Node::Expression(ast::Expression::Call {
            function: ast::CallExpressionFunction::Identifier(id),
            args,
        }) => {
            if let Some(Object::Macro {
                params,
                body,
                env: macro_env,
            }) = env.get(&id)
            {
                let args = quote_args(args.clone());
                let mut eval_env =
                    extend_macro_env(macro_env.clone(), params.clone(), args.clone());
                let body = ast::Statement::from(body);
                let evaluated = eval(body.into(), &mut eval_env);
                if let Object::Quote(node) = evaluated {
                    node
                } else {
                    // TODO
                    panic!("we only support returning AST-nodes from macros");
                }
            } else {
                node
            }
        }
        _ => node,
    })?;
    node.program()
}

fn quote_args(args: Vec<ast::Expression>) -> Vec<Object> {
    args.into_iter()
        .map(|expr| Object::Quote(expr.into()))
        .collect()
}

fn extend_macro_env(
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

#[cfg(test)]
mod tests {
    use super::{define_macros, expand_macros};
    use crate::ast;
    use crate::lexer::Lexer;
    use crate::object::{Environment, Object};
    use crate::parser::parse;

    #[test]
    fn test_define_macros() {
        let input = r#"
        let numbe = 1;
        let function = fn(x, y) { x + y };
        let mymacro = macro(x, y) { x + y };
        "#;

        let mut env = Environment::new();
        let program = test_parse(input);
        let program = define_macros(program, &mut env);

        assert_eq!(program.statements.len(), 2);
        assert!(env.get(&new_id("number")).is_none());
        assert!(env.get(&new_id("function")).is_none());
        assert!(env.get(&new_id("mymacro")).is_some());
        let m = env.get(&new_id("mymacro")).unwrap();
        match m {
            Object::Macro { params, body, .. } => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].to_string(), "x");
                assert_eq!(params[1].to_string(), "y");
                assert_eq!(body.to_string(), "(x + y)");
            }
            _ => panic!("it is not a macro"),
        }
    }

    #[test]
    fn test_expand_macros() -> Result<(), String> {
        let tests = vec![
            (
                r#"
                let infixExpression = macro() { quote(1 + 2); };
                infixExpression();
                "#,
                "(1 + 2)",
            ),
            (
                r#"
                let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
                reverse(2 + 2, 10 - 5);
                "#,
                "(10 - 5) - (2 + 2)",
            ),
            (
                r#"
                let unless = macro(condition, consequence, alternative) {
                    quote(
                        if (!(unquote(condition))) {
                            unquote(consequence);
                        } else {
                            unquote(alternative);
                        }
                    );
                };
                unless(10 > 5, puts("not greater"), puts("greater"));
                "#,
                r#"if (!(10 > 5)) { puts("not greater") } else { puts("greater") }"#,
            ),
        ];

        for (input, expected) in tests {
            let expected = test_parse(expected);
            let program = test_parse(input);
            let mut env = Environment::new();
            let program = define_macros(program, &mut env);
            let expanded = expand_macros(program, &env)?;
            assert_eq!(expanded.to_string(), expected.to_string());
        }
        Ok(())
    }

    // helpers

    fn test_parse(input: &str) -> ast::Program {
        let lexer = Lexer::new(input.into());
        parse(lexer).unwrap()
    }

    fn new_id(id: &str) -> ast::Identifier {
        id.into()
    }
}
