use ast::*;
use lexer::Lexer;
use token::Token;
use token::TokenType;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    unused_lookahead: Option<Token>,
    had_error: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser { lexer, unused_lookahead: None, had_error: false }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    fn get_next_token(&mut self, allow_eof: bool) -> Result<Option<Token>, SyntaxError> {
        let unused_lookahead = self.unused_lookahead.clone();
        self.unused_lookahead = None;
        match unused_lookahead {
            None => match self.lexer.next() {
                None if allow_eof => Ok(None),
                Some(ref token) => Ok(Some(token.clone())),
                None => Err(SyntaxError::new("Unexpected end of file.".to_string(), None))
            }
            Some(ref token) => Ok(Some(token.clone()))
        }
    }

    fn parse(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        match self.parse_stmt()? {
            None => Ok(None),
            Some(stmt) => {
                match self.get_next_token(false)? {
                    Some(ref token) if !token.token_type.eq(&TokenType::EOL) =>
                        return Err(SyntaxError::new("No newline at end of statement.".to_string(),
                                                    Some(token.clone()))),
                    _ => Ok(Some(stmt))
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        let curr_token = match self.get_next_token(true)? {
            None => return Ok(None),
            Some(token) => token
        };
        match curr_token.token_type {
            TokenType::SyntaxError => Err(SyntaxError::new("Syntax error while lexing.".to_string(),
                                                           Some(curr_token))),
            TokenType::EOL => self.parse_stmt(),
            TokenType::LeftBrace => self.parse_block_stmt(),
            TokenType::If => self.parse_if_stmt(&curr_token),
            TokenType::Elif => Err(SyntaxError::new("Found elif with no matching if.".to_string(),
                                                    Some(curr_token))),
            TokenType::Else => Err(SyntaxError::new("Found else with no matching if.".to_string(),
                                                    Some(curr_token))),
            TokenType::While => {
                let cond = match self.parse_expr()? {
                    None => return Err(SyntaxError::new("While statement missing condition expression.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => expr
                };

                let opening_brace = match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                        None)),
                    Some(ref token) if !token.token_type.eq(&TokenType::LeftBrace) => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                                                                                  Some(token.clone()))),
                    Some(ref token) => token.clone()
                };

                match self.parse_block_stmt()? {
                    None => return Err(SyntaxError::new("While statement missing body.".to_string(),
                                                        Some(opening_brace))),
                    Some(stmt) => Ok(Some(Stmt::While { cond: Box::new(cond), body: Box::new(stmt) }))
                }
            }
            TokenType::Next => {
                Ok(Some(Stmt::Next))
            }
            TokenType::Break => {
                Ok(Some(Stmt::Break))
            }
            TokenType::Let => {
                let ident = match self.parse_ident()? {
                    None => return Err(SyntaxError::new("Let statement missing identifier.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => expr
                };
                match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Let statement missing =.".to_string(), Some(curr_token))),
                    Some(ref token) if !token.token_type.eq(&TokenType::Equal) => return Err(SyntaxError::new("Let statement identifier followed by invalid token.".to_string(),
                                                                                                              Some(token.clone()))),
                    _ => ()
                };
                match self.parse_expr()? {
                    None => return Err(SyntaxError::new("Let statement missing assignment expression.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Let { ident: Box::new(ident), expr: Box::new(expr) }))
                }
            }
            TokenType::Print => {
                match self.parse_expr() {
                    Err(e) => return Err(e),
                    Ok(opt_expr) => match opt_expr {
                        None => return Err(SyntaxError::new("Print statement missing expression.".to_string(),
                                                            Some(curr_token))),
                        Some(expr) => Ok(Some(Stmt::Print { expr: Box::new(expr) }))
                    }
                }
            }
            TokenType::Err => {
                match self.parse_expr()? {
                    None => return Err(SyntaxError::new("Err statement missing expression.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Err { expr: Box::new(expr) }))
                }
            }
            TokenType::Scan => {
                match self.parse_ident()? {
                    None => return Err(SyntaxError::new("Scan statement missing identifier.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Scan { ident: Box::new(expr) }))
                }
            }
            TokenType::Return => {
                match self.parse_expr()? {
                    None => return Err(SyntaxError::new("Return statement missing expression.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Return { expr: Box::new(expr) }))
                }
            }
            TokenType::Def => {
                let ident = match self.parse_ident() {
                    Err(e) => return Err(e),
                    Ok(opt_expr) => match opt_expr {
                        None => return Err(SyntaxError::new("Function definition missing identifier.".to_string(),
                                                            Some(curr_token))),
                        Some(expr) => expr
                    }
                };
                match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Function definition missing (.".to_string(),
                                                        Some(curr_token))),
                    Some(ref token) if !token.token_type.eq(&TokenType::LeftParen) => return Err(SyntaxError::new("Function definition missing (.".to_string(),
                                                                                                                  Some(token.clone()))),
                    _ => ()
                };
                let mut params = Vec::new();
                while let Some(curr_token) = self.get_next_token(false)? {
                    if curr_token.token_type.eq(&TokenType::Comma) {
                        // TODO ugly hack that technically doesn't meet the spec
                        continue;
                    }
                    if curr_token.token_type.eq(&TokenType::RightParen) {
                        let token = match self.get_next_token(false)? {
                            None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                                None)),
                            Some(token) => token
                        };
                        if !token.token_type.eq(&TokenType::LeftBrace) {
                            return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                        Some(token)));
                        }

                        match self.parse_block_stmt()? {
                            None => return Err(SyntaxError::new("While statement missing body.".to_string(),
                                                                Some(curr_token))),
                            Some(stmt) => return Ok(Some(Stmt::FnDecl {
                                ident: Box::new(ident),
                                params,
                                body: Box::new(stmt),
                            }))
                        }
                    } else {
                        self.unused_lookahead = Some(curr_token.clone());
                        let param = match self.parse_ident()? {
                            None => return Err(SyntaxError::new("Function definition parameter not a valid lvalue.".to_string(),
                                                                Some(curr_token.clone()))),
                            Some(expr) => expr
                        };
                        params.push(Box::new(param));
                    }
                }
                Err(SyntaxError::new("Unterminated function parameter list.".to_string(),
                                     Some(curr_token)))
            }
            _ => {
                self.unused_lookahead = Some(curr_token);
                let expr = match self.parse_expr()? {
                    None => return Ok(None),
                    Some(expr) => expr
                };
                match self.get_next_token(true)? {
                    None => Ok(Some(Stmt::Expr { expr: Box::new(expr) })),
                    Some(token) => match token.token_type {
                        TokenType::Equal => match self.parse_expr()? {
                            None => return Ok(None),
                            Some(assign) => Ok(Some(Stmt::Assign {
                                ident: Box::new(expr),
                                expr: Box::new(assign),
                            }))
                        },
                        _ => {
                            self.unused_lookahead = Some(token);
                            Ok(Some(Stmt::Expr { expr: Box::new(expr) }))
                        }
                    }
                }
            }
        }
    }

    fn parse_if_stmt(&mut self, curr_token: &Token) -> Result<Option<Stmt>, SyntaxError> {
        let cond = match self.parse_expr()? {
            None => return Err(SyntaxError::new("If statement missing condition expression.".to_string(),
                                                Some(curr_token.clone()))),
            Some(expr) => expr
        };

        match self.get_next_token(false)? {
            None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                None)),
            Some(ref token) if !token.token_type.eq(&TokenType::LeftBrace) => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                                                                          Some(token.clone()))),
            _ => ()
        };

        let body = match self.parse_block_stmt()? {
            None => return Err(SyntaxError::new("If statement missing body.".to_string(),
                                                Some(curr_token.clone()))),
            Some(stmt) => stmt
        };

        let next_token = match self.get_next_token(true)? {
            None => return Ok(Some(Stmt::If { cond: Box::new(cond), true_body: Box::new(body), false_body: None })),
            Some(token) => token
        };

        match next_token.token_type {
            TokenType::Elif => {
                let elif_stmt = match self.parse_if_stmt(&next_token)? {
                    None => return Err(SyntaxError::new("Elif missing body.".to_string(), Some(next_token))),
                    Some(stmt) => stmt
                };
                Ok(Some(Stmt::If {
                    cond: Box::new(cond),
                    true_body: Box::new(body),
                    false_body: Some(Box::new(elif_stmt)),
                }))
            }
            TokenType::Else => {
                match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                        None)),
                    Some(ref token) if !token.token_type.eq(&TokenType::LeftBrace) => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                                                                                  Some(token.clone()))),
                    _ => ()
                };

                let false_body = match self.parse_block_stmt()? {
                    None => return Err(SyntaxError::new("Elif missing body.".to_string(), Some(next_token))),
                    Some(stmt) => stmt
                };
                Ok(Some(Stmt::If {
                    cond: Box::new(cond),
                    true_body: Box::new(body),
                    false_body: Some(Box::new(false_body)),
                }))
            }
            _ => {
                self.unused_lookahead = Some(next_token);
                Ok(Some(Stmt::If { cond: Box::new(cond), true_body: Box::new(body), false_body: None }))
            }
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        let mut stmts = Vec::new();
        let token = match self.get_next_token(false)? {
            None => return Err(SyntaxError::new("Expected body of block statement but found end of file.".to_string(),
                                                None)),
            Some(token) => token
        };
        match token.token_type {
            TokenType::EOL => {
                // multi-line block statement
                while let Some(stmt) = self.parse()? {
                    stmts.push(Box::new(stmt));
                    match self.get_next_token(false)? {
                        None => return Err(SyntaxError::new("Unterminated block statement.".to_string(),
                                                            None)),
                        Some(ref token) if token.token_type.eq(&TokenType::RightBrace) => break,
                        Some(ref token) => self.unused_lookahead = Some(token.clone())
                    };
                }
            }
            _ => {
                // one-line block statement
                self.unused_lookahead = Some(token);
                match self.parse_stmt()? {
                    None => return Err(SyntaxError::new("Expected body of block statement but found end of file.".to_string(),
                                                        None)),
                    Some(stmt) => {
                        stmts.push(Box::new(stmt));
                        match self.get_next_token(false)? {
                            None => return Err(SyntaxError::new("Unterminated block statement.".to_string(),
                                                                None)),
                            Some(ref token) if !token.token_type.eq(&TokenType::RightBrace) => return Err(SyntaxError::new("Block statement missing closing brace.".to_string(),
                                                                                                                           Some(token.clone()))),
                            _ => ()
                        };
                    }
                }
            }
        };

        Ok(Some(Stmt::Block { stmts }))
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_and_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::Or) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_and_expr()? {
                None => return Err(SyntaxError::new("Or expression missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_and_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_eq_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::And) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_eq_expr()? {
                None => return Err(SyntaxError::new("And expression missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_eq_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_comp_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::EqualEqual) && !operator.token_type.eq(&TokenType::BangEqual) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_comp_expr()? {
                None => return Err(SyntaxError::new("Equality comparison missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_comp_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_concat_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::GreaterEqual)
                && !operator.token_type.eq(&TokenType::Greater)
                && !operator.token_type.eq(&TokenType::LesserEqual)
                && !operator.token_type.eq(&TokenType::Lesser) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_concat_expr()? {
                None => return Err(SyntaxError::new("Comparison operator missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_concat_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_add_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::Ampersand) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_add_expr()? {
                None => return Err(SyntaxError::new("Concatenation expression missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_add_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_mul_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::Plus) && !operator.token_type.eq(&TokenType::Minus) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_mul_expr()? {
                None => return Err(SyntaxError::new("Expression missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_mul_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_unary_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            if !operator.token_type.eq(&TokenType::Star) && !operator.token_type.eq(&TokenType::Slash) {
                self.unused_lookahead = Some(operator);
                break;
            }
            let right = match self.parse_unary_expr()? {
                None => return Err(SyntaxError::new("Expression missing right side.".to_string(),
                                                    Some(operator))),
                Some(expr) => expr
            };
            expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
        }
        Ok(Some(expr))
    }

    fn parse_unary_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let curr_token = match self.get_next_token(true)? {
            None => return Ok(None),
            Some(token) => token
        };

        match curr_token.token_type {
            TokenType::Plus => {
                match self.parse_unary_expr()? {
                    None => return Err(SyntaxError::new("Plus unary expression missing operand.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            TokenType::Minus => {
                match self.parse_unary_expr()? {
                    None => return Err(SyntaxError::new("Minus unary expression missing operand.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            TokenType::Bang => {
                match self.parse_unary_expr()? {
                    None => return Err(SyntaxError::new("Negate unary expression missing operand.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            TokenType::Ampersand => {
                match self.parse_unary_expr()? {
                    None => return Err(SyntaxError::new("Stringify unary expression missing operand.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            TokenType::LeftParen => {
                match self.parse_expr()? {
                    None => Err(SyntaxError::new("Unexpected end of file.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => {
                        match self.get_next_token(false)? {
                            Some(ref token) if token.token_type.eq(&TokenType::RightParen) =>
                                Ok(Some(Expr::Grouping { expr: Box::new(expr) })),
                            _ => Err(SyntaxError::new("Unexpected end of grouping.".to_string(),
                                                      Some(curr_token))),
                        }
                    }
                }
            }
            TokenType::String => Ok(Some(Expr::Literal { value: curr_token })),
            TokenType::Integer => Ok(Some(Expr::Literal { value: curr_token })),
            TokenType::Float => Ok(Some(Expr::Literal { value: curr_token })),
            TokenType::True => Ok(Some(Expr::Literal { value: curr_token })),
            TokenType::False => Ok(Some(Expr::Literal { value: curr_token })),
            TokenType::Identifier => {
                self.unused_lookahead = Some(curr_token);
                match self.parse_ident()? {
                    None => Ok(None),
                    Some(ident) => {
                        match self.get_next_token(true)? {
                            None => Ok(Some(ident)),
                            Some(ref token) if token.token_type.eq(&TokenType::LeftParen) => {
                                // function call
                                let mut args = Vec::new();
                                while let Some(curr_token) = self.get_next_token(false)? {
                                    match curr_token.token_type {
                                        TokenType::Comma => continue,
                                        TokenType::RightParen => return Ok(Some(Expr::FnCall { ident: Box::new(ident), args })),
                                        _ => {
                                            self.unused_lookahead = Some(curr_token.clone());
                                            let param = match self.parse_ident()? {
                                                None => return Err(SyntaxError::new(
                                                    "Function call argument not a valid lvalue.".to_string(),
                                                    Some(curr_token))),
                                                Some(expr) => expr
                                            };
                                            args.push(Box::new(param));
                                        }
                                    }
                                }
                                Err(SyntaxError::new("Unterminated function call parameter list.".to_string(),
                                                     Some(token.clone())))
                            }
                            Some(ref token) => {
                                self.unused_lookahead = Some(token.clone());
                                Ok(Some(ident))
                            }
                        }
                    }
                }
            }
            _ => Err(SyntaxError::new("Unexpected token.".to_string(), Some(curr_token)))
        }
    }

    fn parse_ident(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let curr_token = match self.unused_lookahead {
            None => match self.lexer.next() {
                None => return Err(SyntaxError::new("Unexpected end of input".to_string(), None)),
                Some(ref token) => token.clone(),
            }
            Some(ref token) => token.clone()
        };
        self.unused_lookahead = None;

        if curr_token.token_type.eq(&TokenType::Identifier) {
            return Ok(Some(Expr::Identifier { ident: curr_token }));
        }
        Err(SyntaxError::new("Expected identifier.".to_string(), Some(curr_token)))
    }

    fn sync(&mut self) {
        while let Some(token) = self.lexer.next() {
            match token.token_type {
                TokenType::EOL => return,
                TokenType::If => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::While => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Next => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Break => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Let => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Print => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Err => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Scan => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Return => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                TokenType::Def => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                _ => continue
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Stmt> {
        match self.parse() {
            Ok(ast) => ast,
            Err(e) => {
                println!("Found syntax error while parsing: {}", e.msg);
                if let Some(offending) = e.token {
                    println!("Offending token: {:?}", offending);
                }
                self.had_error = true;
                self.sync();
                self.next()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::TokenLiteral;

    #[test]
    fn test_parse_missing_newline() {
        let mut parser = Parser::new(Lexer::new("next"));
        match parser.next() {
            None => assert!(parser.had_error()),
            Some(stmt) => {
                panic!("Found stmt: {:?}", stmt)
            }
        }
    }

    #[test]
    fn test_parse_if_stmt_no_false_body() {
        let mut parse = Parser::new(Lexer::new("if a {\n    break\n}\n"));
        match parse.next().unwrap() {
            Stmt::If { cond, true_body, false_body } => {
                match *cond {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                };
                match *true_body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match stmt {
                            &Stmt::Break => (),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                };
                match false_body {
                    None => return,
                    _ => panic!()
                };
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_multilevel_if_stmt() {
        let mut parse = Parser::new(Lexer::new("if a {\n    break\n} elif b {\n    next\n} else {\n    break\n}\n"));
        match parse.next().unwrap() {
            Stmt::If { cond, true_body, false_body } => {
                match *cond {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                };
                match *true_body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match stmt {
                            &Stmt::Break => (),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                };
                match *false_body.unwrap() {
                    Stmt::If { cond, true_body, false_body } => {
                        match *cond {
                            Expr::Identifier { ident } => {
                                assert_eq!(ident,
                                           Token::new(TokenType::Identifier,
                                                      Some(TokenLiteral::Identifier("b".to_string())),
                                                      2));
                            }
                            _ => panic!()
                        };
                        match *true_body {
                            Stmt::Block { stmts } => {
                                let stmt = &**stmts.iter().next().unwrap();
                                match stmt {
                                    &Stmt::Next => (),
                                    _ => panic!()
                                }
                            }
                            _ => panic!()
                        };
                        match *false_body.unwrap() {
                            Stmt::Block { stmts } => {
                                let stmt = &**stmts.iter().next().unwrap();
                                match stmt {
                                    &Stmt::Break => return,
                                    _ => panic!()
                                }
                            }
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_elif_no_if() {
        let mut parser = Parser::new(Lexer::new("elif {\n    break\n}\n"));
        match parser.next() {
            None => panic!(),
            Some(stmt) => match stmt {
                Stmt::Break => return,
                _ => panic!()
            }
        }
    }

    #[test]
    fn test_parse_else_no_if() {
        let mut parser = Parser::new(Lexer::new("else {\n    break\n}\n"));
        match parser.next() {
            None => panic!(),
            Some(stmt) => match stmt {
                Stmt::Break => return,
                _ => panic!()
            }
        }
    }

    #[test]
    fn test_parse_while_stmt() {
        let mut parser = Parser::new(Lexer::new("while a {\n    break\n}\n"));
        match parser.next().unwrap() {
            Stmt::While { cond, body } => {
                match *cond {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                };
                match *body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match stmt {
                            &Stmt::Break => return,
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_next_stmt() {
        let mut parser = Parser::new(Lexer::new("next\n"));
        match parser.next().unwrap() {
            Stmt::Next => return,
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_break_stmt() {
        let mut parser = Parser::new(Lexer::new("break\n"));
        match parser.next().unwrap() {
            Stmt::Break => return,
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_let_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = 1 + 2\n"));
        match parser.next().unwrap() {
            Stmt::Let { ident, expr } => {
                match *ident {
                    Expr::Identifier { ident } => assert_eq!(ident,
                                                             Token::new(TokenType::Identifier,
                                                                        Some(TokenLiteral::Identifier("a".to_string())),
                                                                        0)),
                    _ => panic!()
                };
                match *expr {
                    Expr::Binary { ref left, ref operator, ref right } => {
                        if !operator.eq(&Token::new(TokenType::Plus, None, 0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Literal { ref value } => assert_eq!(value, &Token::new(TokenType::Integer,
                                                                                         Some(TokenLiteral::Int(1)),
                                                                                         0)),
                            _ => panic!()
                        };
                        match **right {
                            Expr::Literal { ref value } => assert_eq!(value, &Token::new(TokenType::Integer,
                                                                                         Some(TokenLiteral::Int(2)),
                                                                                         0)),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_assign_stmt() {
        let mut parser = Parser::new(Lexer::new("a = \"hi \" & \"user\"\n"));
        match parser.next().unwrap() {
            Stmt::Assign { ident, expr } => {
                match *ident {
                    Expr::Identifier { ident } => assert_eq!(ident,
                                                             Token::new(TokenType::Identifier,
                                                                        Some(TokenLiteral::Identifier("a".to_string())),
                                                                        0)),
                    _ => panic!()
                };
                match *expr {
                    Expr::Binary { ref left, ref operator, ref right } => {
                        if !operator.eq(&Token::new(TokenType::Ampersand, None, 0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Literal { ref value } => assert_eq!(value, &Token::new(TokenType::String,
                                                                                         Some(TokenLiteral::String("hi ".to_string())),
                                                                                         0)),
                            _ => panic!()
                        };
                        match **right {
                            Expr::Literal { ref value } => assert_eq!(value, &Token::new(TokenType::String,
                                                                                         Some(TokenLiteral::String("user".to_string())),
                                                                                         0)),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_print_stmt() {
        let mut parser = Parser::new(Lexer::new("print a\n"));
        match parser.next().unwrap() {
            Stmt::Print { expr } => {
                match *expr {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_err_stmt() {
        let mut parser = Parser::new(Lexer::new("err a\n"));
        match parser.next().unwrap() {
            Stmt::Err { expr } => {
                match *expr {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_scan_stmt() {
        let mut parser = Parser::new(Lexer::new("scan a\n"));
        match parser.next().unwrap() {
            Stmt::Scan { ident } => {
                match *ident {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_return_stmt() {
        let mut parser = Parser::new(Lexer::new("return a\n"));
        match parser.next().unwrap() {
            Stmt::Return { expr } => {
                match *expr {
                    Expr::Identifier { ident } => {
                        assert_eq!(ident,
                                   Token::new(TokenType::Identifier,
                                              Some(TokenLiteral::Identifier("a".to_string())),
                                              0));
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_fn_decl() {
        let mut parser = Parser::new(Lexer::new("def f(a,b) {\n return 1.0\n}\n"));
        match parser.next().unwrap() {
            Stmt::FnDecl { ident, params, body } => {
                match *ident {
                    Expr::Identifier { ident } => assert_eq!(ident,
                                                             Token::new(TokenType::Identifier,
                                                                        Some(TokenLiteral::Identifier("f".to_string())),
                                                                        0)),
                    _ => panic!()
                };
                let mut iter = params.iter();
                let param_a = &**iter.next().unwrap();
                match param_a {
                    &Expr::Identifier { ref ident } => {
                        assert_eq!(ident,
                                   &Token::new(TokenType::Identifier,
                                               Some(TokenLiteral::Identifier("a".to_string())),
                                               0));
                        let param_b = &**iter.next().unwrap();
                        match param_b {
                            &Expr::Identifier { ref ident } => assert_eq!(ident,
                                                                          &Token::new(TokenType::Identifier,
                                                                                      Some(TokenLiteral::Identifier("b".to_string())),
                                                                                      0)),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                };
                match *body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match stmt {
                            &Stmt::Return { ref expr } => {
                                match **expr {
                                    Expr::Literal { ref value } => assert_eq!(value,
                                                                              &Token::new(TokenType::Float,
                                                                                          Some(TokenLiteral::Float(1.0)),
                                                                                          1)),
                                    _ => panic!()
                                }
                            }
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_block_stmt_one_line() {
        let mut parser = Parser::new(Lexer::new("{ next }\n"));
        match parser.next().unwrap() {
            Stmt::Block { stmts } => {
                let stmt = &**stmts.iter().next().unwrap();
                match stmt {
                    &Stmt::Next => return,
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_block_stmt_multi_line() {
        let mut parser = Parser::new(Lexer::new("{\n    next\n    break\n}\n"));
        match parser.next().unwrap() {
            Stmt::Block { stmts } => {
                let mut iter = stmts.iter();
                let next_stmt = &**iter.next().unwrap();
                match next_stmt {
                    &Stmt::Next => {
                        let break_stmt = &**iter.next().unwrap();
                        match break_stmt {
                            &Stmt::Break => return,
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_block_stmt_unterminated() {
        let mut parser = Parser::new(Lexer::new("{\n    next\n"));
        match parser.next() {
            None => assert!(parser.had_error()),
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_identifier() {
        let mut parser = Parser::new(Lexer::new("a\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Identifier { ident } => assert_eq!(ident, Token::new(TokenType::Identifier,
                                                                           Some(TokenLiteral::Identifier("a".to_string())), 0)),
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_or_expr() {
        let mut parser = Parser::new(Lexer::new("a or b or c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Or, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Or, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_and_expr() {
        let mut parser = Parser::new(Lexer::new("a and b and c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::And, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::And, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_eq_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a == b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::EqualEqual, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_neq_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a != b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::BangEqual, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_gt_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a > b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Greater, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_ge_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a >= b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::GreaterEqual, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_lt_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a < b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Lesser, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_le_comp_expr() {
        let mut parser = Parser::new(Lexer::new("a <= b\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::LesserEqual, None, 0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("b".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_concat_expr() {
        let mut parser = Parser::new(Lexer::new("a & b & c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Ampersand, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Ampersand, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_add_expr() {
        let mut parser = Parser::new(Lexer::new("a + b + c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Plus, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Plus, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_sub_expr() {
        let mut parser = Parser::new(Lexer::new("a - b - c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Minus, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Minus, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_mul_expr() {
        let mut parser = Parser::new(Lexer::new("a * b * c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Star, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Star, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_div_expr() {
        let mut parser = Parser::new(Lexer::new("a / b / c\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Binary { ref left, ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Slash, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("c".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::new(TokenType::Slash, None, 0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("a".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::new(TokenType::Identifier,
                                                                                       Some(TokenLiteral::Identifier("b".to_string())),
                                                                                       0)) => (),
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_unary_plus_expr() {
        let mut parser = Parser::new(Lexer::new("+a\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Unary { ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Plus, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_unary_minus_expr() {
        let mut parser = Parser::new(Lexer::new("-a\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Unary { ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Minus, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_unary_negate_expr() {
        let mut parser = Parser::new(Lexer::new("!a\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Unary { ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Bang, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_unary_stringify_expr() {
        let mut parser = Parser::new(Lexer::new("&a\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Unary { ref operator, ref right } => {
                    if !operator.eq(&Token::new(TokenType::Ampersand, None, 0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::new(TokenType::Identifier,
                                                     Some(TokenLiteral::Identifier("a".to_string())),
                                                     0)) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_grouping_expr() {
        let mut parser = Parser::new(Lexer::new("(a + b)\n"));
        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::Grouping { expr } => match *expr {
                    Expr::Binary { ref left, ref operator, ref right } => {
                        if !operator.eq(&Token::new(TokenType::Plus, None, 0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Identifier { ref ident } => {
                                if !ident.eq(&Token::new(TokenType::Identifier,
                                                         Some(TokenLiteral::Identifier("a".to_string())),
                                                         0)) {
                                    panic!()
                                }
                            }
                            _ => panic!()
                        };
                        match **right {
                            Expr::Identifier { ref ident } => {
                                if !ident.eq(&Token::new(TokenType::Identifier,
                                                         Some(TokenLiteral::Identifier("b".to_string())),
                                                         0)) {
                                    panic!()
                                }
                            }
                            _ => panic!()
                        };
                    }
                    _ => panic!()
                }
                _ => panic!()
            },
            _ => panic!()
        }
    }

    #[test]
    fn test_parse_func_call() {
        let mut parser = Parser::new(Lexer::new("f(a,b)\n"));

        match parser.next().unwrap() {
            Stmt::Expr { expr } => match *expr {
                Expr::FnCall { ref ident, ref args } => {
                    match **ident {
                        Expr::Identifier { ref ident } => assert_eq!(ident, &Token::new(TokenType::Identifier,
                                                                                        Some(TokenLiteral::Identifier("f".to_string())),
                                                                                        0)),
                        _ => panic!()
                    }
                    let mut iter = args.iter();
                    let arg_a = &**iter.next().unwrap();
                    match arg_a {
                        &Expr::Identifier { ref ident } => {
                            assert_eq!(ident, &Token::new(TokenType::Identifier,
                                                          Some(TokenLiteral::Identifier("a".to_string())),
                                                          0));
                            let arg_b = &**iter.next().unwrap();
                            match arg_b {
                                &Expr::Identifier { ref ident } => {
                                    assert_eq!(ident, &Token::new(TokenType::Identifier,
                                                                  Some(TokenLiteral::Identifier("b".to_string())),
                                                                  0));
                                }
                                _ => panic!()
                            }
                        }
                        _ => panic!()
                    }
                }
                _ => panic!()
            }
            _ => panic!()
        }
    }
}
