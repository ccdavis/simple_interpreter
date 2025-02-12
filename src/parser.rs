use super::token::{Token, TokenValue};

pub struct ParserState {
    tokens: Vec<Token>,
    current: usize,
    // TODO: errors and the file path of the source if any
}

impl ParserState {
    // We hope to move the tokens into the ParserState, not borrow them.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // Conditionally match next token and consume it.
    pub fn matches(&mut self, types: &[TokenValue]) -> bool {
        let found = types.iter().any(|t| self.check(t));
        if found {
            self.advance();
        }
        found
    }

    // Consume next token if an exact match or return error
    pub fn consume(&mut self, t: &TokenValue) -> bool {
        if self.check(t) {
            self.advance();
            true
        } else {
            eprintln!("Wanted '{:?}', Unexpected token {:?}", &t, &self.peek());
            //std::process::exit(1);
            false
        }
    }

    fn check(&self, token: &TokenValue) -> bool {
        self.is_finished() || self.peek().same_type(token)
    }

    fn is_finished(&self) -> bool {
        matches!(self.peek().value(), &TokenValue::Eof)
    }

    pub fn peek(&self) -> &Token {
        &self
            .tokens
            .get(self.current)
            .expect("Expected an EOF token before reaching the end of the token list.")
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_finished() {
            self.current += 1
        }
        self.previous()
    }
}
