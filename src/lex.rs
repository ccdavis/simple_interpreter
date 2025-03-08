use super::token::{
    reserved_words_lookup, CompareOp, LogicalOp, Op, Token, TokenLocation, TokenValue,
};
use std::collections::HashMap;

pub struct Scanner {
    text: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    reserved_words: HashMap<String, TokenValue>,
}

impl Scanner {
    pub fn new(script: &str) -> Self {
        Self {
            text: script.to_owned().chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            reserved_words: reserved_words_lookup(),
        }
    }

    // TODO place-holder
    fn error(&self, msg: String) {
        eprintln!("Syntax Error at {}, {}: {}", self.line, self.column, &msg)
    }

    fn is_finished(&self) -> bool {
        self.current >= self.text.len()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_finished() {
            self.start = self.current;
            match self.scan_token() {
                Err(msg) => self.error(msg),
                Ok(token) => tokens.push(token),
            }
        }

        tokens.push(self.make_token(TokenValue::Eof));

        tokens
            .into_iter()
            .filter(|t| !matches!(t.value(), TokenValue::Comment(_)))
            .collect()
    }

    fn make_token(&self, token_value: TokenValue) -> Token {
        let location = TokenLocation {
            column_end: self.current,
            line: self.line,
            column: self.column,
        };
        Token(token_value, location)
    }

    fn scan_token(&mut self) -> Result<Token, String> {
        // I include comments in the match because they are returned as tokens
        // for use in preprocessing, like annotations or for pretty-printing.
        self.skip_whitespace();
        let c = self.advance();
        let token_value = match c {
            '\0' => TokenValue::Eof,
            '(' => TokenValue::LeftParen,
            ')' => TokenValue::RightParen,
            '{' => TokenValue::LeftBrace,
            '}' => TokenValue::RightBrace,
            ',' => TokenValue::Comma,
            '@' => TokenValue::At,
            '.' => {
                if self.match_char('.') {
                    TokenValue::DotDot
                } else {
                    TokenValue::Dot
                }
            }
            '-' => TokenValue::Operator(Op::Sub),
            '*' => TokenValue::Operator(Op::Mul),
            '+' => TokenValue::Operator(Op::Add),
            ';' => TokenValue::SemiColon,
            ':' => {
                if self.match_char('=') {
                    TokenValue::Assign
                } else {
                    TokenValue::Colon
                }
            }
            '>' => {
                if self.match_char('=') {
                    TokenValue::CompareOperator(CompareOp::Gte)
                } else {
                    TokenValue::CompareOperator(CompareOp::Gt)
                }
            }
            '<' => {
                if self.match_char('=') {
                    TokenValue::CompareOperator(CompareOp::Lte)
                } else if self.match_char('>') {
                    TokenValue::CompareOperator(CompareOp::Ne)
                } else {
                    TokenValue::CompareOperator(CompareOp::Lt)
                }
            }
            '=' => TokenValue::CompareOperator(CompareOp::Eq),
            '/' => {
                if self.match_char('/') {
                    let mut content = "".to_string();
                    while self.this_char() != '\n' && !self.is_finished() {
                        content.push(self.this_char());
                        self.advance();
                    }
                    TokenValue::Comment(content)
                } else {
                    TokenValue::Operator(Op::Div)
                }
            }
            '"' => self.string_literal()?,
            _ => {
                if self.is_digit(c) {
                    self.number_literal()?
                } else if self.is_alpha(c) {
                    self.identifier_or_keyword()
                } else {
                    return Err(format!(
                        "Unrecognized character {} at {}, {}",
                        c, self.line, self.column
                    ));
                }
            }
        };
        Ok(self.make_token(token_value))
    }

    // This will be called after advance() so 'current' will be the char following
    // the one we got from advance() -- a look-ahead.
    fn match_char(&mut self, expected: char) -> bool {
        let not_matched = self.is_finished() || expected != self.text[self.current];
        if not_matched {
            return false;
        }
        //self.current += 1;
        self.advance();
        true
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_whitespace(&self, c: char) -> bool {
        c == '\n' || c == ' ' || c == '\t' || c == '\r'
    }

    fn skip_whitespace(&mut self) {
        while !self.is_finished() && self.is_whitespace(self.this_char()) {
            self.advance();
        }
        self.start = self.current;
    }

    fn this_char(&self) -> char {
        if self.is_finished() {
            return '\0';
        }
        self.text[self.current]
    }

    fn next_char(&self) -> char {
        if self.current + 1 >= self.text.len() {
            return '\0';
        }
        self.text[self.current + 1]
    }

    fn number_literal(&mut self) -> Result<TokenValue, String> {
        let mut integer_literal = true;
        while self.is_digit(self.this_char()) {
            self.advance();
        }
        if self.this_char() == '.' && self.is_digit(self.next_char()) {
            integer_literal = false;
            self.advance(); // eat the "."
            while self.is_digit(self.this_char()) {
                self.advance();
            }
        }
        let content: String = self.text[self.start..self.current].iter().collect();
        if integer_literal {
            match content.parse() {
                Ok(value) => Ok(TokenValue::Integer(value)),
                Err(msg) => Err(format!("{} when parsing '{}'", msg, &content)),
            }
        } else {
            match content.parse() {
                Ok(value) => Ok(TokenValue::Float(value)),
                Err(msg) => Err(format!("{} when parsing '{}'", msg, &content)),
            }
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_' || c == '?' || c == '!'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier_or_keyword(&mut self) -> TokenValue {
        while self.is_alpha_numeric(self.this_char()) {
            self.advance();
        }
        let content: String = self.text[self.start..self.current]
            .iter()
            .collect::<String>();
        match content.as_ref() {
            "true" => TokenValue::Bool(true),
            "false" => TokenValue::Bool(false),
            "and" => TokenValue::LogicalOperator(LogicalOp::And),
            "or" => TokenValue::LogicalOperator(LogicalOp::Or),
            _ => match self.reserved_words.get(&content) {
                Some(word) => word.clone(),
                None => TokenValue::Ident(content),
            },
        }
    }

    fn string_literal(&mut self) -> Result<TokenValue, String> {
        // For better reporting on unterminated strings
        let starting_line = self.line;
        let starting_column = self.column;

        let mut content = "".to_string();
        while self.this_char() != '"' && !self.is_finished() {
            content.push(self.this_char());
            self.advance();
        }

        if self.is_finished() {
            let msg: String = format!(
                "Unterminated string starting at {}, {}",
                starting_line, starting_column
            );
            Err(msg)
        } else {
            // Eat the second "
            self.advance();
            Ok(TokenValue::Str(content))
        }
    }

    fn advance(&mut self) -> char {
        if self.is_finished() {
            return '\0';
        }
        let c = self.text[self.current];
        self.current += 1;
        self.column += 1;
        if '\n' == c {
            self.line += 1;
            self.column = 0;
        }
        c
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_literals() {
        let literals = "35 8.5 \"abc\" true false";
        let mut s = Scanner::new(literals);
        let tokens = s.tokenize();
        assert_eq!(6, tokens.len());
        assert_eq!(TokenValue::Integer(35), tokens[0].value().clone());
        assert_eq!(TokenValue::Float(8.5), tokens[1].value().clone());
        assert_eq!(
            TokenValue::Str("abc".to_string()),
            tokens[2].value().clone()
        );
        assert_eq!(TokenValue::Bool(true), tokens[3].value().clone());
        assert_eq!(TokenValue::Bool(false), tokens[4].value().clone());
        assert_eq!(TokenValue::Eof, tokens[5].value().clone());
    }

    #[test]
    fn test_keywords() {
        let text = "if for     \n else let output input";
        let mut s = Scanner::new(text);
        let tokens = s.tokenize();
        assert_eq!(TokenValue::If, tokens[0].value().clone());
        assert_eq!(TokenValue::For, tokens[1].value().clone());
        assert_eq!(TokenValue::Else, tokens[2].value().clone());
        assert_eq!(TokenValue::Let, tokens[3].value().clone());
        assert_eq!(TokenValue::Output, tokens[4].value().clone());
        assert_eq!(TokenValue::Input, tokens[5].value().clone());
    }

    #[test]
    fn test_symbols() {
        let text = "{}()  @. .. ;: = := < <= <> and or";
        let mut s = Scanner::new(text);
        let tokens = s.tokenize();
        assert_eq!(TokenValue::LeftBrace, tokens[0].value().clone());
        assert_eq!(TokenValue::RightBrace, tokens[1].value().clone());
        assert_eq!(TokenValue::LeftParen, tokens[2].value().clone());
        assert_eq!(TokenValue::RightParen, tokens[3].value().clone());
        assert_eq!(TokenValue::At, tokens[4].value().clone());
        assert_eq!(TokenValue::Dot, tokens[5].value().clone());
        assert_eq!(TokenValue::DotDot, tokens[6].value().clone());
        assert_eq!(TokenValue::SemiColon, tokens[7].value().clone());
        assert_eq!(TokenValue::Colon, tokens[8].value().clone());
        assert_eq!(
            TokenValue::CompareOperator(CompareOp::Eq),
            tokens[9].value().clone()
        );
        assert_eq!(TokenValue::Assign, tokens[10].value().clone());
        assert_eq!(
            TokenValue::CompareOperator(CompareOp::Lt),
            tokens[11].value().clone()
        );
        assert_eq!(
            TokenValue::CompareOperator(CompareOp::Lte),
            tokens[12].value().clone()
        );
        assert_eq!(
            TokenValue::CompareOperator(CompareOp::Ne),
            tokens[13].value().clone()
        );
        assert_eq!(
            TokenValue::LogicalOperator(LogicalOp::And),
            tokens[14].value().clone()
        );
        assert_eq!(
            TokenValue::LogicalOperator(LogicalOp::Or),
            tokens[15].value().clone()
        );
    }

    #[test]
    fn test_program() {
        let program = "let a:=2;\n let result := 12;\n a := a + 1; if (a = 2){ result = 99} else {result = 0}; output result";
        let mut s = Scanner::new(program);
        let tokens = s.tokenize();
        assert_eq!("let", &tokens[0].value().to_string());

        let parts: Vec<String> = tokens.iter().map(|t| t.value().to_string()).collect();
        assert_eq!("a", &parts[1]);
        assert_eq!(":=", &parts[2]);
        assert_eq!("2", &parts[3]);
        assert_eq!(";", &parts[4]);
        assert_eq!("let", &parts[5]);
        assert_eq!("result", &parts[6]);
        assert_eq!(":=", &parts[7]);
        assert_eq!("12", &parts[8]);
        assert_eq!(";", &parts[9]);
        assert_eq!("a", &parts[10]);
        assert_eq!(":=", &parts[11]);
        assert_eq!("a", &parts[12]);
        assert_eq!("+", &parts[13]);
        assert_eq!("1", &parts[14]);
    }
}
