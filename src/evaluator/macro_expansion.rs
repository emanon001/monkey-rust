use crate::ast::{self};
use crate::object::{Environment, Object};

pub fn define_macros(prog: ast::Program, env: &mut Environment) -> ast::Program {
    let mut macro_excluded_statements = Vec::new();
    for stmt in prog.statements {
        if is_macro_definition(&stmt) {
            add_macro(stmt, env);
        } else {
            macro_excluded_statements.push(stmt);
        }
    }
    ast::Program {
        statements: macro_excluded_statements,
    }
}

fn is_macro_definition(stmt: &ast::Statement) -> bool {
    if let ast::Statement::Let { expression, .. } = stmt {
        if let ast::Expression::Macro { .. } = expression {
            return true;
        }
    }
    false
}

fn add_macro(stmt: ast::Statement, env: &mut Environment) {
    if let ast::Statement::Let {
        identifier,
        expression,
    } = stmt
    {
        if let ast::Expression::Macro(m) = expression {
            let ast::MacroExpression { params, body } = m;
            let m = Object::Macro {
                params,
                body,
                env: env.clone(),
            };
            env.set(&identifier, m);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::define_macros;
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
        let program = test_parse(input.into());
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

    // helpers

    fn test_parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        parse(lexer).unwrap()
    }

    fn new_id(id: &str) -> ast::Identifier {
        id.into()
    }
}
