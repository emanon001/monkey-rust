pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {}

pub trait Expression: Node {}

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or("".into())
    }
}
