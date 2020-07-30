pub struct Lexer {
    input: Vec<char>,
    position: usize,
    readPosition: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = input.chars().collect::<Vec<char>>();
        Self {
            input,
            position: 0,
            readPosition: 0,
            ch: None,
        }
    }
}
