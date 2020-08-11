use crate::ast::{Expression, Node, Program, Statement};

pub type Error = String;
pub type Result<T> = std::result::Result<T, Error>;

pub fn modify<F>(node: Node, modifier: F) -> Result<Node>
where
    F: Fn(Node) -> Node,
{
    match node {
        Node::Program(it) => Ok(modify_program(it, &modifier)?.into()),
        Node::Statement(it) => Ok(modify_statement(it, &modifier)?.into()),
        Node::Expression(it) => Ok(modify_expression(it, &modifier)?.into()),
    }
}

fn modify_program<F>(mut prog: Program, modifier: &F) -> Result<Program>
where
    F: Fn(Node) -> Node,
{
    for i in 0..prog.statements.len() {
        let stat = modify_statement(prog.statements[i].clone(), modifier)?;
        prog.statements[i] = stat;
    }
    Ok(prog)
}

fn modify_statement<F>(stat: Statement, modifier: &F) -> Result<Statement>
where
    F: Fn(Node) -> Node,
{
    match stat {
        Statement::Expression(expr) => {
            Ok(Statement::Expression(modify_expression(expr, modifier)?))
        }
        _ => Err("err".into()),
    }
}

fn modify_expression<F>(expr: Expression, modifier: &F) -> Result<Expression>
where
    F: Fn(Node) -> Node,
{
    Ok(modifier(expr.into()).expression()?)
}

#[cfg(test)]
mod tests {
    use crate::ast::modify::modify;
    use crate::ast::{Expression, Node, Program, Statement};

    #[test]
    fn modify_integer_expression() -> Result<(), Box<dyn std::error::Error>> {
        let Helpers {
            one,
            two,
            turn_one_into_two,
        } = helpers();
        let node = Node::from(one.clone());
        let expected = two.clone().into();
        let res = modify(node, turn_one_into_two)?;
        assert_eq!(res, expected);
        Ok(())
    }

    #[test]
    fn modify_program_statement() -> Result<(), Box<dyn std::error::Error>> {
        let Helpers {
            one,
            two,
            turn_one_into_two,
        } = helpers();
        let node = Program {
            statements: vec![Statement::Expression(one.clone())],
        }
        .into();
        let expected = Program {
            statements: vec![Statement::Expression(two.clone())],
        }
        .into();
        let res = modify(node, turn_one_into_two)?;
        assert_eq!(res, expected);
        Ok(())
    }

    // helpers

    struct Helpers {
        one: Expression,
        two: Expression,
        turn_one_into_two: Box<dyn Fn(Node) -> Node>,
    }

    fn helpers() -> Helpers {
        let one = Expression::Integer(1);
        let two = Expression::Integer(2);
        let turn_one_into_two = Box::new(turn_one_into_two);
        Helpers {
            one,
            two,
            turn_one_into_two,
        }
    }

    fn turn_one_into_two(node: Node) -> Node {
        if let Node::Expression(expr) = &node {
            if let Expression::Integer(it) = expr {
                if it == &1 {
                    return Expression::Integer(2).into();
                }
            }
        }
        node
    }
}
