use crate::token::TokenLocation;

use super::expression::{Expr, ExprRef, ExpressionPool};
use super::parser::ParserState;
use super::symbols::SymbolTable;
use super::token::{LogicalOp, Op, Token, TokenValue};
use super::types::LangType;
use std::fmt;

pub type CompileResult = Result<(ExprRef, LangType), CompilerError>;

const PARSER_DEBUG: bool = true;

// Translates from source to target
pub struct LanguageParser {
    source: ParserState,
    target: ExpressionPool,
    symbols: SymbolTable,
    current_frame: usize,
    errors: Vec<CompilerError>,
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
            target: ExpressionPool::new(),
            symbols: SymbolTable::default(),
            current_frame: 0,
            errors: Vec::new(),
        }
    }

    // The grammar specific logic ---------------------
    pub fn parse_program(&mut self) -> Result<ExpressionPool, Vec<CompilerError>> {
        // TODO: Instead of returning from the first error, parse as many statements as possible.
        if let Err(e) = self.statement_list() {
            return Err(vec![e]);
        }
        self.source.consume(&TokenValue::Eof);
        // TODO move this to a separate clone function
        if self.errors.is_empty() {
            Ok(self.target.clone())
        } else {
            Err(self.errors.clone())
        }
    }

    fn statement_list(&mut self) -> CompileResult {
        let stmt_list_addr = self.target.add(Expr::StmtList(ExprRef(0), ExprRef(0)));
        let (mut stmt_addr, mut stmt_type) = self.statement()?;
        let first_stmt_addr = stmt_addr;

        // TODO: if a statement has an error, read to the next ';' token or the end of the list to
        // be able to parse the next statement, to catch  more than one error at a time.
        let mut look_ahead = self.next_token();
        while !matches!(look_ahead.value(), TokenValue::Eof)
            && !matches!(look_ahead.value(), TokenValue::RightBrace)
            && !matches!(look_ahead.value(), TokenValue::Else)
        {
            // This is the statement separator
            self.source.consume(&TokenValue::SemiColon);
            (stmt_addr, stmt_type) = self.statement()?;
            look_ahead = self.next_token();
        }
        self.target
            .update(stmt_list_addr, Expr::StmtList(first_stmt_addr, stmt_addr));
        Ok((stmt_list_addr, stmt_type))
    }

    fn statement(&mut self) -> CompileResult {
        let look_ahead = self.next_token();
        let (stmt_addr, stmt_type) = match look_ahead.value() {
            TokenValue::Let => self.let_stmt()?,
            TokenValue::Ident(ref name) => {
                self.source.advance();
                let after_ident = self.next_token();
                // The identifier might be followed by a '(' : a left hand side function call;
                // or '[': an index into a collection of some kind; or just an assignment symbol.])
                match after_ident.value() {
                    TokenValue::Assign => self.assign_stmt(name, &look_ahead)?,
                    // Other paths not implemented yet
                    _ => {
                        self.print_error("Syntax error.", &after_ident);
                        std::process::exit(1);
                    }
                }
            }
            TokenValue::If => self.if_stmt()?,
            TokenValue::For => self.for_stmt()?,
            TokenValue::Output => self.output_stmt()?,
            _ => {
                if look_ahead.same_type(&TokenValue::RightBrace) {
                    self.empty_stmt()?
                } else {
                    self.print_error("Parse error on statement.", &look_ahead);
                    std::process::exit(1);
                }
            }
        };

        Ok((stmt_addr, stmt_type))
    }

    fn empty_stmt(&mut self) -> CompileResult {
        let addr = self.target.add_with_type(Expr::Unit, &LangType::Unit);
        if PARSER_DEBUG {
            println!("Added empty statement at: {}", addr.0);
        }
        Ok((addr, LangType::Unit))
    }

    fn let_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::Let);
        let look_ahead = self.next_token();
        if let TokenValue::Ident(ref name) = look_ahead.value() {
            self.source.advance();
            self.source.consume(&TokenValue::Assign);
            let (variable_value, variable_type) = self.logical_expression()?;
            let let_addr = self.target.add_with_type(
                Expr::Let(variable_value, variable_type.clone()),
                &variable_type,
            );
            self.symbols.set(self.current_frame, name, let_addr);
            Ok((let_addr, variable_type))
        } else {
            self.print_error("Expected identifier", &look_ahead);
            std::process::exit(1);
        }
    }

    fn assign_stmt(&mut self, identifier: &str, id_token: &Token) -> CompileResult {
        self.source.consume(&TokenValue::Assign);
        let (variable_value, variable_type) = self.logical_expression()?;
        let ste = self.symbols.get(self.current_frame, identifier);
        if let Some(symbol) = ste {
            let rhs_type = self.target.get_type(*symbol);
            if !variable_type.same_as(&rhs_type) {
                self.print_error("Type mismatch in assignment.", id_token);
                std::process::exit(1);
            }
            let assign_addr = self.target.add(Expr::Assign(*symbol, variable_value));
            Ok((assign_addr, rhs_type))
        } else {
            let msg = format!("Unknown identifier: '{}'", identifier);
            self.print_error(&msg, id_token);
            std::process::exit(1);
        }
    }

    fn if_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::If);
        self.source.consume(&TokenValue::LeftParen);
        let (cond_addr, cond_et) = self.logical_expression()?;
        if !matches!(cond_et, LangType::Boolean) {
            let msg = format!("Must use a boolean type of expression in an if-statement condition, but was '{:?}'.",cond_et);
            self.print_error(&msg, &self.next_token());
            std::process::exit(1);
        }
        self.source.consume(&TokenValue::RightParen);
        // Insert the 'if' statement before the statement-list branches
        let if_stmt_addr = self.target.add(Expr::If(cond_addr, ExprRef(0), None));

        self.source.consume(&TokenValue::LeftBrace);
        let (then_addr, then_type) = self.statement_list()?;
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
            let (else_addr, else_type) = self.statement_list()?;
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
        Ok((if_stmt_addr, then_type))
    }

    pub fn for_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::For);
        self.source.consume(&TokenValue::LeftParen);
        // This form of 'for' functions like a 'while' loop.
        let (cond_addr, _cond_type) = self.logical_expression()?;
        // Other forms of 'for' can have three expressions: index, in low_exp to high_exp
        self.source.consume(&TokenValue::RightParen);
        let for_stmt_addr = self.target.add(Expr::For(cond_addr, ExprRef(0)));

        self.source.consume(&TokenValue::LeftBrace);
        let (loop_addr, loop_type) = self.statement_list()?;
        self.source.consume(&TokenValue::RightBrace);
        self.target
            .update(for_stmt_addr, Expr::For(cond_addr, loop_addr));
        Ok((for_stmt_addr, loop_type))
    }

    fn output_stmt(&mut self) -> CompileResult {
        self.source.consume(&TokenValue::Output);
        let (value_addr, value_type) = self.logical_expression()?;
        if PARSER_DEBUG {
            println!("Added value to output: {:?}", self.target.get(value_addr));
        }
        let output_addr = self
            .target
            .add_with_type(Expr::Output(value_addr, value_type.clone()), &value_type);
        if PARSER_DEBUG {
            println!(
                "{}: Added output statement: {:?}",
                output_addr.0,
                self.target.get(output_addr)
            );
        }
        Ok((output_addr, value_type))
    }

    fn expression_list(&mut self) -> CompileResult {
        let (expr_ref, _) = self.logical_expression()?;
        while let TokenValue::Comma = self.next_token().value() {
            self.source.matches(&[TokenValue::Comma]);
            let (expr_ref, _) = self.logical_expression()?;
        }
        // The address of the first expression or any of the types aren't needed anywhere
        Ok((expr_ref, LangType::Unit))
    }

    fn logical_expression(&mut self) -> CompileResult {
        let (lhs_addr, expression_type) = self.expression()?;
        let look_ahead = self.next_token();
        if let TokenValue::LogicalOperator(logical_op) = look_ahead.value() {
            if PARSER_DEBUG {
                println!("Parsing logical operation {:?}", &logical_op);
            }
            self.source.advance();

            let logical_expr_ref = if matches!(logical_op, LogicalOp::Or) {
                let (rhs_addr, rhs_expression_type) = self.expression()?;
                if !LangType::both_boolean(&expression_type, &rhs_expression_type) {
                    let message = format!(
                        "Can't compare arguments to '{}', must be boolean types.",
                        look_ahead.value()
                    );
                    self.print_error(&message, &look_ahead);
                    let error = CompilerError::Type(message, look_ahead.location().clone());
                    return CompileResult::Err(error);
                };

                self.target.add_with_type(
                    Expr::Logical(LogicalOp::Or, lhs_addr, rhs_addr),
                    &LangType::Boolean,
                )
            } else {
                // Add 'and' before compiling the right-hand side. After compiling
                // the rhs we can update the expression with the rhs address.
                let this_address = self.target.add_with_type(
                    Expr::Logical(LogicalOp::And, lhs_addr, ExprRef(0)),
                    &LangType::Boolean,
                );
                let (rhs_addr, rhs_expression_type) = self.expression()?;
                if !LangType::both_boolean(&expression_type, &rhs_expression_type) {
                    let message = format!(
                        "Can't compare arguments to '{}', must be boolean types. LHS type: {}, RHS type {}",
                        look_ahead.value(),
                        &expression_type,
                        &rhs_expression_type
                    );
                    self.print_error(&message, &look_ahead);
                    let error = CompilerError::Type(message, look_ahead.location().clone());
                    return CompileResult::Err(error);
                };
                self.target.update(
                    this_address,
                    Expr::Logical(LogicalOp::And, lhs_addr, rhs_addr),
                );
                // In the case the lhs is false we can have the vm move the program pointer
                // here and place the result. In the case it's true on the lhs we have to
                // evaluate the rhs anyhow and that result ends up at rhs_addr.
                rhs_addr
            };
            Ok((logical_expr_ref, LangType::Boolean))
        } else {
            Ok((lhs_addr, expression_type))
        }
    }

    fn expression(&mut self) -> CompileResult {
        let (lhs_addr, expression_type) = self.simple_expression()?;
        let look_ahead = self.next_token();
        if let TokenValue::LogicalOperator(logical_op) = look_ahead.value() {
            if PARSER_DEBUG {
                println!(
                    "From expression(), Parsing logical operation {:?}",
                    &logical_op
                );
            }
            self.source.advance();
            let logical_expr_ref = if matches!(logical_op, LogicalOp::Or) {
                let (rhs_addr, rhs_expression_type) = self.simple_expression()?;
                if !LangType::both_boolean(&expression_type, &rhs_expression_type) {
                    let message = format!(
                        "Can't compare arguments to 'or', must be boolean types. LHS type: {}, RHS type: {}",
                        &expression_type, &rhs_expression_type                        
                    );
                    self.print_error(&message, &look_ahead);
                    let error = CompilerError::Type(message, look_ahead.location().clone());
                    return CompileResult::Err(error);
                };

                self.target.add_with_type(
                    Expr::Logical(LogicalOp::Or, lhs_addr, rhs_addr),
                    &LangType::Boolean,
                )
            } else {
                // Add 'and' before compiling the right-hand side. After compiling
                // the rhs we can update the expression with the rhs address.
                let this_address = self.target.add_with_type(
                    Expr::Logical(LogicalOp::And, lhs_addr, ExprRef(0)),
                    &LangType::Boolean,
                );
                let (rhs_addr, rhs_expression_type) = self.simple_expression()?;
                if !LangType::both_boolean(&expression_type, &rhs_expression_type) {
                    let message = format!(
                        "Can't compare arguments to '{}', must be boolean types. LHS type: {}, RHS type: {}.",
                        look_ahead.value(),
                        &expression_type, &rhs_expression_type

                    );
                    self.print_error(&message, &look_ahead);
                    let error = CompilerError::Type(message, look_ahead.location().clone());
                    return CompileResult::Err(error);
                };
                self.target.update(
                    this_address,
                    Expr::Logical(LogicalOp::And, lhs_addr, rhs_addr),
                );
                this_address
            };
            Ok((logical_expr_ref, LangType::Boolean))
        } else if let TokenValue::CompareOperator(bool_op) = look_ahead.value().clone() {
            if PARSER_DEBUG {
                println!("Parsing compare operation {}", &bool_op);
            }
            self.source.advance();
            let (rhs_addr, rhs_expression_type) = self.simple_expression()?;

            // Some basic type checking
            if !LangType::comparable(expression_type, rhs_expression_type) {
                let message = format!("Can't compare arguments to '{:?}'", look_ahead.value());
                self.print_error(&message, &look_ahead);
                std::process::exit(1);
            }

            let this_address = self.target.add_with_type(
                Expr::Compare(bool_op, lhs_addr, rhs_addr),
                &LangType::Boolean,
            );
            Ok((this_address, LangType::Boolean))
        } else {
            Ok((lhs_addr, expression_type))
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

        let (lhs_addr, lhs_type) = self.term()?;
        if matches!(leading_op, Op::Sub) {
            // Insert a -1
            let negator_addr = self
                .target
                .add_with_type(Expr::LiteralInt(-1), &LangType::Integer);
            let reverse_sign_addr = self
                .target
                .add(Expr::Binary(Op::Mul, negator_addr, lhs_addr));
            if PARSER_DEBUG {
                println!("Added negation term.");
            }
            let (rhs_addr, rhs_type) = self.simple_part(reverse_sign_addr, &LangType::Integer)?;

            if matches!(rhs_type, LangType::Float) || matches!(lhs_type, LangType::Float) {
                Ok((rhs_addr, LangType::Float))
            } else {
                Ok((rhs_addr, LangType::Integer))
            }
        } else {
            let (rhs_addr, rhs_type) = self.simple_part(lhs_addr, &lhs_type)?;
            let simple_part_type = LangType::type_of_expression_parts(&lhs_type, &rhs_type);
            Ok((rhs_addr, simple_part_type))
        }
    }

    fn term(&mut self) -> CompileResult {
        let (lhs_addr, lhs_type) = self.factor()?;
        // Multiply by the term part
        let (rhs_addr, rhs_type) = self.term_part(lhs_addr, &lhs_type)?;
        let term_type = LangType::type_of_expression_parts(&lhs_type, &rhs_type);
        Ok((rhs_addr, term_type))
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
            let (rhs_addr, rhs_type) = self.factor()?;
            let mul_op_addr = self.target.add_with_type(
                Expr::Binary(op.clone(), lhs_addr, rhs_addr),
                &LangType::type_of_expression_parts(last_factor_type, &rhs_type),
            );

            let (next_part_addr, next_part_type) = self.term_part(mul_op_addr, &rhs_type)?;

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
                Ok((next_part_addr, LangType::Float))
            } else {
                Ok((next_part_addr, LangType::Integer))
            }
        } else {
            Ok((lhs_addr, last_factor_type.clone()))
        }
    }

    fn simple_part(&mut self, lhs_addr: ExprRef, last_lhs_type: &LangType) -> CompileResult {
        let look_ahead = self.next_token();
        if look_ahead.is_add_op() {
            if let TokenValue::Operator(ref op) = look_ahead.value() {
                self.source.advance();
                let (rhs_addr, rhs_type) = self.term()?;
                let last_part_type = LangType::type_of_expression_parts(last_lhs_type, &rhs_type);
                let term_addr = self.target.add_with_type(
                    Expr::Binary(op.clone(), lhs_addr, rhs_addr),
                    &last_part_type,
                );

                let (next_part_addr, next_part_type) =
                    self.simple_part(term_addr, &last_part_type)?;
                let simple_part_type =
                    LangType::type_of_expression_parts(&next_part_type, &rhs_type);
                Ok((next_part_addr, simple_part_type))
            } else {
                panic!("Internal parser error.");
            }
        } else {
            // No more parts to the expression, preserve type
            Ok((lhs_addr, last_lhs_type.clone()))
        }
    }

    // AKA "Primary expressions"
    fn factor(&mut self) -> CompileResult {
        let look_ahead = self.next_token();
        let result: CompileResult = match look_ahead.value() {
            TokenValue::Float(f) => {
                let expr_addr = self
                    .target
                    .add_with_type(Expr::LiteralFloat(*f), &LangType::Float);
                self.source.advance();
                Ok((expr_addr, LangType::Float))
            }
            TokenValue::Integer(i) => {
                let expr_addr = self
                    .target
                    .add_with_type(Expr::LiteralInt(*i), &LangType::Integer);
                self.source.advance();
                Ok((expr_addr, LangType::Integer))
            }
            TokenValue::Str(s) => {
                let expr_addr = self
                    .target
                    .add_with_type(Expr::LiteralString(s.clone()), &LangType::String);
                self.source.advance();
                Ok((expr_addr, LangType::String))
            }
            TokenValue::Bool(b) => {
                let expr_addr = self
                    .target
                    .add_with_type(Expr::LiteralBool(*b), &LangType::Boolean);
                self.source.advance();
                Ok((expr_addr, LangType::Boolean))
            }
            TokenValue::Ident(name) => {
                let ste = self.symbols.get(self.current_frame, name);
                if let Some(value_storage) = ste {
                    let declared_type = self.target.get_type(*value_storage);
                    let _call_addr = self
                        .target
                        .add_with_type(Expr::Call(*value_storage), &declared_type);
                    self.source.advance();
                    Ok((*value_storage, declared_type))
                } else {
                    let msg = format!("Undeclared identifier '{}'", name);
                    self.print_error(&msg, &look_ahead);
                    std::process::exit(1);
                }
            }
            TokenValue::LeftParen => {
                self.source.advance();
                let (expression_addr, expression_type) = self.logical_expression()?;
                if self.source.matches(&[TokenValue::RightParen]) {
                    Ok((expression_addr, expression_type))
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
        // println!("Parsed factor: '{:?}'", &result);

        result
    }
} // impl

#[derive(Debug, Clone)]
pub enum CompilerError {
    Syntax(String, TokenLocation),
    Type(String, TokenLocation),
    UnknownIdentifier(String, TokenLocation),
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CompilerError::*;

        match self {
            Syntax(err, loc) => write!(f, "Syntax error at {}, {}: {}", loc.column, loc.line, err),
            Type(err, loc) => write!(f, "Type error at {}, {}: {}", loc.column, loc.line, err),
            UnknownIdentifier(err, loc) => write!(
                f,
                "Unknown identifier at {}, {}: {}",
                loc.column, loc.line, err
            ),
        }
    }
}

pub fn syntax_error(msg: &str, loc: &TokenLocation) -> CompilerError {
    CompilerError::Syntax(msg.to_string(), loc.clone())
}

pub fn type_error(msg: &str, loc: &TokenLocation) -> CompilerError {
    CompilerError::Type(msg.to_string(), loc.clone())
}

pub fn unknown_identifier_error(msg: &str, loc: &TokenLocation) -> CompilerError {
    CompilerError::UnknownIdentifier(msg.to_string(), loc.clone())
}

impl std::error::Error for CompilerError {}

#[cfg(test)]
mod test {

    use super::super::expression::*;
    use super::super::token::*;
    use super::super::types::LangType;
    use super::super::*;

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
            assign_tok(),
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
    fn test_logical_operations() {
        let code = vec![
            output_tok(),
            int_tok(5),
            compare_op_tok(CompareOp::Eq),
            int_tok(5),
            logical_tok(LogicalOp::Or),
            int_tok(5),
            compare_op_tok(CompareOp::Eq),
            int_tok(3),
            eof_tok(),
        ];
        let program = try_parsing(code);
        assert!(program.is_ok());
        let exprs = program.unwrap();
        assert_eq!(9, exprs.size());
        assert_eq!(
            &Expr::Output(ExprRef(7), LangType::Boolean),
            &exprs.exprs[8]
        );
        assert_eq!(
            &Expr::Logical(LogicalOp::Or, ExprRef(3), ExprRef(6)),
            &exprs.exprs[7]
        );
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

        let integer_test = vec![output_tok(), int_tok(99), eof_tok()];
        let int_program = try_parsing(integer_test);
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
        match expr_pool {
            Ok(output) => Ok(output),
            Err(e) => Err(e
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")),
        }
    }
}
