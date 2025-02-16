use super::expression::{Expr, ExprRef, ExpressionPool};
use super::parser::ParserState;
use super::symbols::SymbolTable;
use super::token::{Op, Token, TokenValue};
use super::types::LangType;

pub type CompileResult = (ExprRef, LangType);

// Translates from source to target
pub struct LanguageParser {
    source: ParserState,
    target: ExpressionPool,
    symbols: SymbolTable,
    current_frame: usize,
}

impl LanguageParser {
    // TODO this is really basic
    fn print_error(&self, message: &str, t: &Token) {
        eprintln!(
            "{} at {}, {}",
            message,
            t.location().line,
            t.location().column
        );
    }

    fn next_token(&self) -> Token {
        self.source.peek().clone()
    }

    pub fn symbol_table_frame(&self) -> usize {
        self.symbols.current_frame()
    }

    pub fn new(code: Vec<Token>) -> Self {
        Self {
            source: ParserState::new(code),
            target: ExpressionPool::default(),
            symbols: SymbolTable::default(),
            current_frame: 0,
        }
    }

    // The grammar specific logic ---------------------
    pub fn parse_program(&mut self) -> Option<ExpressionPool> {
        self.statement_list();
        self.source.consume(&TokenValue::Eof);
        // TODO move this to a separate clone function
        Some(self.target.clone())
    }

    // I'm going to convert these statements into expression-statements.

    fn statement_list(&mut self) -> CompileResult {
        let stmt_list_addr = self.target.add(Expr::StmtList(ExprRef(0), ExprRef(0)));
        let (mut stmt_addr, mut stmt_type) = self.statement();
        let first_stmt_addr = stmt_addr;

        let mut look_ahead = self.next_token();
        while !matches!(look_ahead.value(), TokenValue::Eof)
            && !matches!(look_ahead.value(), TokenValue::RightBrace)
            && !matches!(look_ahead.value(), TokenValue::Else)
        {
            // This is the statement separator
            self.source.consume(&TokenValue::SemiColon);
            (stmt_addr, stmt_type) = self.statement();
            look_ahead = self.next_token();
        }
        self.target
            .update(stmt_list_addr, Expr::StmtList(first_stmt_addr, stmt_addr));
        (stmt_list_addr, stmt_type)
    }

    fn statement(&mut self) -> CompileResult {
        let look_ahead = self.next_token();
        let (stmt_addr, stmt_type) = match look_ahead.value() {
            TokenValue::Let => self.let_stmt(),
            TokenValue::Assign => self.assign_stmt(),
            TokenValue::If => self.if_stmt(),
            TokenValue::Output => self.output_stmt(),
            _ => panic!("Statement not implemented! Token was {:?}", &look_ahead),
        };

        (stmt_addr, stmt_type)
    }

    fn let_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::Let);
        let look_ahead = self.next_token();
        if let TokenValue::Ident(ref name) = look_ahead.value() {
            self.source.advance();
            self.source.consume(&TokenValue::EqualSign);
            let (variable_value, variable_type) = self.expression();
            let let_addr = self
                .target
                .add(Expr::Let(variable_value, variable_type.clone()));
            self.symbols.set(self.current_frame, name, let_addr);
            (let_addr, variable_type)
        } else {
            self.print_error("Expected identifier", &look_ahead);
            std::process::exit(1);
        }
    }

    fn assign_stmt(&mut self) -> CompileResult {
        (ExprRef(0), LangType::Unit)
    }

    fn if_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::If);
        self.source.consume(&TokenValue::LeftParen);
        let (cond_addr, cond_et) = self.expression();
        if !matches!(cond_et, LangType::Boolean) {
            self.print_error(
                "Must use a boolean type of expression in an if-statement.",
                &self.next_token(),
            );
            std::process::exit(1);
        }
        self.source.consume(&TokenValue::RightParen);
        // Insert the 'if' statement before the statement-list branches
        let if_stmt_addr = self.target.add(Expr::If(cond_addr, ExprRef(0), None));

        self.source.consume(&TokenValue::LeftBrace);
        let (then_addr, then_type) = self.statement_list();
        self.source.consume(&TokenValue::RightBrace);

        // The then_addr points to a statement list from which we can compute the location past
        // the last statement in the 'then' branch. This is the location the interpreter
        //must jump to if the condition is false, regardless of the presence of an 'else' branch.
        let conditional_branch_addr = if let Expr::StmtList(_, l) = self.target.get(then_addr) {
            ExprRef(l.0 + 1)
        } else {
            panic!("Internal compiler error. A 'then' branch of an if statement must be a statement list.");
        };

        if self.source.matches(&[TokenValue::Else]) {
            self.source.consume(&TokenValue::LeftBrace);
            let (else_addr, else_type) = self.statement_list();
            self.source.consume(&TokenValue::RightBrace);
            if then_type.same_as(&else_type) {
                // The 'else_addr' marks the location where the interpreter should jump to if
                // the condition is true and we executed the statements in the 'then' branch, and
                // an 'else' branch exists, so it must jump past it.
                let continue_addr = if let Expr::StmtList(_, l) = self.target.get(else_addr) {
                    ExprRef(l.0 + 1)
                } else {
                    panic!("Internal compiler error. A 'else' branch of an if statement must be a statement list.");
                };

                let completed_if =
                    Expr::If(cond_addr, conditional_branch_addr, Some(continue_addr));
                self.target.update(if_stmt_addr, completed_if);
            } else {
                let msg = format!(
                    "Type mismatch between 'if' branches: THEN = '{:?}' ELSE = '{:?}'",
                    &then_type, &else_type
                );
                self.print_error(&msg, &self.next_token());
                std::process::exit(1);
            }
        } else {
            let completed_if = Expr::If(cond_addr, conditional_branch_addr, None);
            self.target.update(if_stmt_addr, completed_if);
        }
        (if_stmt_addr, then_type)
    }

    fn output_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::Output);
        let (value_addr, value_type) = self.expression();
        let output_addr = self.target.add(Expr::Output(value_addr, value_type));
        (output_addr, LangType::Unit)
    }

    fn expression_list(&mut self) {
        let (_, _) = self.expression();
        while let TokenValue::Comma = self.next_token().value() {
            self.source.matches(&[TokenValue::Comma]);
            let (_, _) = self.expression();
        }
        // The address of the first expression or any of the types aren't needed anywhere
    }

    fn expression(&mut self) -> CompileResult {
        let (lhs_addr, expression_type) = self.simple_expression();
        let look_ahead = self.next_token();
        if let TokenValue::CompareOperator(bool_op) = look_ahead.value().clone() {
            self.source.advance();
            let (rhs_addr, rhs_expression_type) = self.simple_expression();

            // Some basic type checking
            if !LangType::comparable(expression_type, rhs_expression_type) {
                let message = format!("Can't compare arguments to '{:?}'", look_ahead.value());
                self.print_error(&message, &look_ahead);
                std::process::exit(1);
            }

            let this_address = self.target.add(Expr::Compare(bool_op, lhs_addr, rhs_addr));
            (this_address, LangType::Boolean)
        } else {
            (lhs_addr, expression_type)
        }
    }

    fn simple_expression(&mut self) -> CompileResult {
        // A leading '-' is possible. We set the leading operator to '+' by default.
        let mut leading_op = Op::Add;
        let look_ahead = self.next_token();
        if look_ahead.is_add_op() {
            if let TokenValue::Operator(ref op) = look_ahead.value() {
                if matches!(op, Op::Sub) {
                    leading_op = Op::Sub;
                }
            }
            self.source.advance();
        }

        let (lhs_addr, lhs_type) = self.term();
        if matches!(leading_op, Op::Sub) {
            // Insert a -1
            let negator_addr = self.target.add(Expr::LiteralInt(-1));
            let reverse_sign_addr = self
                .target
                .add(Expr::Binary(Op::Mul, negator_addr, lhs_addr));
            let (rhs_addr, rhs_type) = self.simple_part(reverse_sign_addr, &LangType::Integer);
            if matches!(rhs_type, LangType::Float) || matches!(lhs_type, LangType::Float) {
                (rhs_addr, LangType::Float)
            } else {
                (rhs_addr, LangType::Integer)
            }
        } else {
            let (rhs_addr, rhs_type) = self.simple_part(lhs_addr, &lhs_type);
            let simple_part_type = LangType::type_of_expression_parts(&lhs_type, &rhs_type);
            (rhs_addr, simple_part_type)
        }
    }

    fn term(&mut self) -> CompileResult {
        let (lhs_addr, lhs_type) = self.factor();
        // Multiply by the term part
        let (rhs_addr, rhs_type) = self.term_part(lhs_addr, &lhs_type);
        let term_type = LangType::type_of_expression_parts(&lhs_type, &rhs_type);
        (rhs_addr, term_type)
    }

    fn term_part(&mut self, lhs_addr: ExprRef, last_factor_type: &LangType) -> CompileResult {
        let look_ahead = self.next_token();
        if look_ahead.is_mul_op() {
            let op = if let TokenValue::Operator(mul_op) = look_ahead.value() {
                mul_op
            } else {
                panic!("Internal error.");
            };
            self.source.advance();

            // Parse the right-hand side and get the type of the argument to 'op'
            let (rhs_addr, rhs_type) = self.factor();
            let mul_op_addr = self
                .target
                .add(Expr::Binary(op.clone(), lhs_addr, rhs_addr));

            let (next_part_addr, next_part_type) = self.term_part(mul_op_addr, &rhs_type);

            // Mul or Div can only be done on scalars:
            if !LangType::both_scalar(&next_part_type, &rhs_type) {
                let msg = format!(
                    "Left and right must be scalar: '{:?}', '{:?}'",
                    &next_part_type, &rhs_type
                );
                self.print_error(&msg, &look_ahead);
                std::process::exit(1);
            }
            if matches!(rhs_type, LangType::Float) || matches!(next_part_type, LangType::Float) {
                (next_part_addr, LangType::Float)
            } else {
                (next_part_addr, LangType::Integer)
            }
        } else {
            (lhs_addr, last_factor_type.clone())
        }
    }

    fn simple_part(&mut self, lhs_addr: ExprRef, last_lhs_type: &LangType) -> CompileResult {
        let look_ahead = self.next_token();
        if look_ahead.is_add_op() {
            if let TokenValue::Operator(ref op) = look_ahead.value() {
                self.source.advance();
                let (rhs_addr, rhs_type) = self.term();
                let term_addr = self
                    .target
                    .add(Expr::Binary(op.clone(), lhs_addr, rhs_addr));

                let last_part_type = LangType::type_of_expression_parts(last_lhs_type, &rhs_type);
                let (next_part_addr, next_part_type) = self.simple_part(term_addr, &last_part_type);

                let simple_part_type =
                    LangType::type_of_expression_parts(&next_part_type, &rhs_type);
                (next_part_addr, simple_part_type)
            } else {
                panic!("Internal parser error.");
            }
        } else {
            // No more parts to the expression, preserve type
            (lhs_addr, last_lhs_type.clone())
        }
    }

    // AKA "Primary expressions"
    fn factor(&mut self) -> CompileResult {
        let look_ahead = self.next_token();
        let data_type = match look_ahead.value() {
            TokenValue::Float(f) => {
                let expr_addr = self.target.add(Expr::LiteralFloat(*f));
                self.source.advance();
                (expr_addr, LangType::Float)
            }
            TokenValue::Integer(i) => {
                let expr_addr = self.target.add(Expr::LiteralInt(*i));
                self.source.advance();
                (expr_addr, LangType::Integer)
            }
            TokenValue::Str(s) => {
                let expr_addr = self.target.add(Expr::LiteralString(s.clone()));
                self.source.advance();
                (expr_addr, LangType::String)
            }
            TokenValue::Ident(name) => {
                let ste = self.symbols.get(self.current_frame, name).clone();
                if let Some(value_storage) = ste {
                    let call_addr = self.target.add(Expr::Call(*value_storage));
                    self.source.advance();

                    // get type of the referenced value
                    let orig_expr: &Expr = self.target.get(*value_storage);
                    let call_type = if let Expr::Let(_, data_type) = orig_expr {
                        data_type.clone()
                    } else {
                        panic!("Internal parser error. A call to '{}' has a reference to  something other than a 'let' statement.", &name );
                    };
                    (call_addr, call_type)
                } else {
                    let msg = format!("Undeclared identifier '{}'", name);
                    self.print_error(&msg, &look_ahead);
                    std::process::exit(1);
                }
            }
            TokenValue::LeftParen => {
                self.source.advance();
                let (expression_addr, expression_type) = self.expression();
                if self.source.matches(&[TokenValue::RightParen]) {
                    (expression_addr, expression_type)
                } else {
                    self.print_error("Expected ')'", &look_ahead);
                    std::process::exit(1);
                }
            }
            _ => {
                let msg = format!(
                    "Unexpected token, not a legal part of a factor: {:?}",
                    &look_ahead.value()
                );
                self.print_error(&msg, &look_ahead);
                std::process::exit(1);
            }
        };

        data_type
    }
} // impl

#[cfg(test)]
mod test {

    use super::super::expression::*;
    use super::super::token::{Op, Token, TokenLocation, TokenValue};
    use super::super::types::LangType;
    use super::super::*;

    // helper functions
    pub fn no_loc() -> TokenLocation {
        TokenLocation {
            column: 0,
            line: 0,
            column_end: 0,
        }
    }

    pub fn output_tok() -> Token {
        Token(TokenValue::Output, no_loc())
    }

    pub fn int_tok(n: i64) -> Token {
        Token(TokenValue::Integer(n), no_loc())
    }

    pub fn flt_tok(n: f64) -> Token {
        Token(TokenValue::Float(n), no_loc())
    }

    pub fn str_tok(s: &str) -> Token {
        Token(TokenValue::Str(s.to_string()), no_loc())
    }

    pub fn op_tok(o: Op) -> Token {
        Token(TokenValue::Operator(o), no_loc())
    }

    pub fn stmt_terminator_tok() -> Token {
        Token(TokenValue::SemiColon, no_loc())
    }

    pub fn let_stmt_tok() -> Token {
        Token(TokenValue::Let, no_loc())
    }

    pub fn equals_tok() -> Token {
        Token(TokenValue::EqualSign, no_loc())
    }

    pub fn ident_tok(n: &str) -> Token {
        Token(TokenValue::Ident(n.to_string()), no_loc())
    }

    pub fn eof_tok() -> Token {
        Token(TokenValue::Eof, no_loc())
    }

    fn program1_tokens() -> Vec<super::Token> {
        vec![
            output_tok(),
            int_tok(5),
            op_tok(Op::Add),
            int_tok(9),
            op_tok(Op::Mul),
            int_tok(55),
            stmt_terminator_tok(),
            output_tok(),
            int_tok(0),
            eof_tok(),
        ]
    }

    #[test]
    fn test_multi_stmt_program() {
        let code = program1_tokens();
        let mut language_parser = LanguageParser::new(code);
        let expr_pool = language_parser
            .parse_program()
            .expect("Error during parsing.");
        assert!(expr_pool.size() > 0);
    }

    // TODO this needs to wait for proper Result return types
    fn test_invalid_program() {
        let tokens = vec![int_tok(1), output_tok(), op_tok(Op::Add)];
        let program = try_parsing(tokens);
        assert!(program.is_err());
    }

    #[test]
    fn test_let_stmt() {
        let code = vec![
            let_stmt_tok(),
            ident_tok("a"),
            equals_tok(),
            int_tok(8),
            eof_tok(),
        ];
        let program = try_parsing(code);
        assert!(program.is_ok());
        let exprs = program.unwrap();

        // Has the assigned value expr, the 'let' expr, and the statement-list expr.
        assert_eq!(3, exprs.size());
        assert_eq!(exprs.exprs[0], Expr::StmtList(ExprRef(2), ExprRef(2)));
        assert_eq!(exprs.exprs[1], Expr::LiteralInt(8));
        assert_eq!(exprs.exprs[2], Expr::Let(ExprRef(1), LangType::Integer));
    }

    #[test]
    fn test_literals() {
        let string_test = vec![output_tok(), str_tok("Hello"), eof_tok()];
        let string_program = try_parsing(string_test);
        assert!(string_program.is_ok());
        let program = string_program.unwrap();
        // Three expressions: 1. string, 2. 'output', 3. stmt_list
        assert_eq!(3, program.size());

        let string_literal = Expr::LiteralString("Hello".to_string());
        let output_stmt = Expr::Output(ExprRef(1), LangType::String);
        let stmt_list = Expr::StmtList(ExprRef(2), ExprRef(2));

        assert_eq!(string_literal, program.expr(1));
        assert_eq!(output_stmt, program.expr(2));

        let string_test = vec![output_tok(), int_tok(99), eof_tok()];
        let int_program = try_parsing(string_test);
        assert!(int_program.is_ok());
        let program = int_program.unwrap();
        let int_literal = Expr::LiteralInt(99);
        let output_stmt = Expr::Output(ExprRef(1), LangType::Integer);
        assert_eq!(int_literal, program.expr(1));
        assert_eq!(output_stmt, program.expr(2));
    }

    fn try_parsing(code: Vec<Token>) -> Result<ExpressionPool, String> {
        let mut language_parser = LanguageParser::new(code);
        assert_eq!(1, language_parser.symbol_table_frame());
        let expr_pool = language_parser.parse_program();

        // TODO Put in real error handling
        if let Some(output) = expr_pool {
            Ok(output)
        } else {
            Err(format!("No results from parsing. Check STDERR."))
        }
    }
}
