use crate::ast::{Expression, Node, Program, Statement};

pub type Error = String;
pub type Result<T> = std::result::Result<T, Error>;

pub fn modify<F: Fn(Node) -> Node>(node: Node, modifier: F) -> Result<Node> {
    match node {
        Node::Program(it) => Ok(modify_program(it, &modifier)?.into()),
        Node::Statement(it) => Ok(modify_statement(it, &modifier)?.into()),
        Node::Expression(it) => Ok(modify_expression(it, &modifier)?.into()),
    }
}

fn modify_program<F: Fn(Node) -> Node>(prog: Program, modifier: &F) -> Result<Program> {
    let mut statements = Vec::new();
    for stat in prog.statements {
        let stat = modify_statement(stat, modifier)?;
        statements.push(stat);
    }
    let prog = Program { statements };
    Ok(prog)
}

fn modify_statement<F: Fn(Node) -> Node>(stat: Statement, modifier: &F) -> Result<Statement> {
    match stat {
        Statement::Expression(expr) => {
            Ok(Statement::Expression(modify_expression(expr, modifier)?))
        }
        other => Ok(modifier(other.into()).statement()?.into()),
    }
}

fn modify_expression<F: Fn(Node) -> Node>(expr: Expression, modifier: &F) -> Result<Expression> {
    match expr {
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = modify_expression(*left, modifier)?;
            let right = modify_expression(*right, modifier)?;
            Ok(Expression::Infix {
                left: left.into(),
                operator,
                right: right.into(),
            })
        }
        other => Ok(modifier(other.into()).expression()?),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::modify::modify;
    use crate::ast::{Expression, InfixOperator, Node, Program, Statement};

    #[test]
    fn modify_integer_expression() -> Result<(), Box<dyn std::error::Error>> {
        let node = Node::from(one());
        let expected = two().into();
        let res = modify(node, turn_one_into_two)?;
        assert_eq!(res, expected);
        Ok(())
    }

    #[test]
    fn modify_infix_expression() -> Result<(), Box<dyn std::error::Error>> {
        let tests = vec![
            (
                Expression::Infix {
                    left: one().into(),
                    operator: InfixOperator::Add,
                    right: two().into(),
                },
                Expression::Infix {
                    left: two().into(),
                    operator: InfixOperator::Add,
                    right: two().into(),
                },
            ),
            (
                Expression::Infix {
                    left: two().into(),
                    operator: InfixOperator::Add,
                    right: one().into(),
                },
                Expression::Infix {
                    left: two().into(),
                    operator: InfixOperator::Add,
                    right: two().into(),
                },
            ),
        ];
        for (expr, expected) in tests {
            let node = Node::from(expr);
            let res = modify(node, turn_one_into_two)?;
            assert_eq!(res, expected.into());
        }
        Ok(())
    }

    #[test]
    fn modify_program_statement() -> Result<(), Box<dyn std::error::Error>> {
        let node = Program {
            statements: vec![Statement::Expression(one())],
        }
        .into();
        let expected = Program {
            statements: vec![Statement::Expression(two())],
        }
        .into();
        let res = modify(node, turn_one_into_two)?;
        assert_eq!(res, expected);
        Ok(())
    }

    // helpers

    fn one() -> Expression {
        Expression::Integer(1)
    }

    fn two() -> Expression {
        Expression::Integer(2)
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
