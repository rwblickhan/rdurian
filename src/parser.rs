use ast::*;
use lexer::Lexer;
use token::Token;

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
                    Some(ref token) => match *token {
                        Token::EOL(_line) => Ok(Some(stmt)),
                        _ => Err(SyntaxError::new("No newline at end of statement.".to_string(),
                                                  Some(token.clone()))),
                    }
                    _ => Err(SyntaxError::new("No newline at end of file.".to_string(), None))
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        let curr_token = match self.get_next_token(true)? {
            None => return Ok(None),
            Some(token) => token
        };
        match curr_token {
            Token::SyntaxError { .. } => Err(SyntaxError::new("Syntax error while lexing.".to_string(),
                                                              Some(curr_token.clone()))),
            Token::EOL(_line) => self.parse_stmt(),
            Token::LeftBrace(_line) => self.parse_block_stmt(),
            Token::If(_line) => self.parse_if_stmt(&curr_token),
            Token::Elif(_line) => Err(SyntaxError::new("Found elif with no matching if.".to_string(),
                                                       Some(curr_token))),
            Token::Else(_line) => Err(SyntaxError::new("Found else with no matching if.".to_string(),
                                                       Some(curr_token))),
            Token::While(_line) => {
                let cond = match self.parse_expr()? {
                    None => return Err(SyntaxError::new("While statement missing condition expression.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => expr
                };

                let opening_brace = match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                        None)),
                    Some(ref token) => match *token {
                        Token::LeftBrace(_line) => token.clone(),
                        _ => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                         Some(token.clone())))
                    }
                };

                match self.parse_block_stmt()? {
                    None => Err(SyntaxError::new("While statement missing body.".to_string(),
                                                 Some(opening_brace))),
                    Some(stmt) => Ok(Some(Stmt::While { cond: Box::new(cond), body: Box::new(stmt) }))
                }
            }
            Token::Next(_line) => {
                Ok(Some(Stmt::Next))
            }
            Token::Break(_line) => {
                Ok(Some(Stmt::Break))
            }
            Token::Let(_line) => {
                let ident = match self.parse_ident()? {
                    None => return Err(SyntaxError::new("Let statement missing identifier.".to_string(),
                                                        Some(curr_token))),
                    Some(expr) => expr
                };
                match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Let statement missing =.".to_string(), Some(curr_token))),
                    Some(ref token) => match *token {
                        Token::Equal(_line) => (),
                        _ => return Err(SyntaxError::new("Let statement identifier followed by invalid token.".to_string(),
                                                         Some(token.clone()))),
                    }
                };
                match self.parse_expr()? {
                    None => Err(SyntaxError::new("Let statement missing assignment expression.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Let { ident: Box::new(ident), expr: Box::new(expr) }))
                }
            }
            Token::Print(_line) => {
                match self.parse_expr() {
                    Err(e) => Err(e),
                    Ok(opt_expr) => match opt_expr {
                        None => Err(SyntaxError::new("Print statement missing expression.".to_string(),
                                                     Some(curr_token))),
                        Some(expr) => Ok(Some(Stmt::Print { expr: Box::new(expr) }))
                    }
                }
            }
            Token::Err(_line) => {
                match self.parse_expr()? {
                    None => Err(SyntaxError::new("Err statement missing expression.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Err { expr: Box::new(expr) }))
                }
            }
            Token::Scan(_line) => {
                match self.parse_ident()? {
                    None => Err(SyntaxError::new("Scan statement missing identifier.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Scan { ident: Box::new(expr) }))
                }
            }
            Token::Return(_line) => {
                match self.parse_expr()? {
                    None => Err(SyntaxError::new("Return statement missing expression.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Stmt::Return { expr: Box::new(expr) }))
                }
            }
            Token::Def(_line) => {
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
                    Some(ref token) => match *token {
                        Token::LeftParen(_line) => (),
                        _ => return Err(SyntaxError::new("Function definition missing (.".to_string(),
                                                         Some(token.clone()))),
                    }
                };
                let mut params = Vec::new();
                while let Some(curr_token) = self.get_next_token(false)? {
                    match curr_token {
                        Token::Comma(_line) => continue,
                        Token::RightParen(_line) => {
                            let token = match self.get_next_token(false)? {
                                None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                                    None)),
                                Some(token) => token
                            };

                            match token {
                                Token::LeftBrace(_line) => (),
                                _ => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                                 Some(token)))
                            };

                            match self.parse_block_stmt()? {
                                None => return Err(SyntaxError::new("While statement missing body.".to_string(),
                                                                    Some(curr_token))),
                                Some(stmt) => return Ok(Some(Stmt::FnDecl {
                                    ident: Box::new(ident),
                                    params,
                                    body: Box::new(stmt),
                                }))
                            }
                        }
                        _ => {
                            self.unused_lookahead = Some(curr_token.clone());
                            let param = match self.parse_ident()? {
                                None => return Err(SyntaxError::new("Function definition parameter not a valid lvalue.".to_string(),
                                                                    Some(curr_token.clone()))),
                                Some(expr) => expr
                            };
                            params.push(Box::new(param));
                        }
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
                    Some(token) => match token {
                        Token::Equal(_line) => match self.parse_expr()? {
                            None => Ok(None),
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
            Some(ref token) => match *token {
                Token::LeftBrace(_line) => (),
                _ => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                 Some(token.clone()))),
            }
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

        match next_token {
            Token::Elif(_line) => {
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
            Token::Else(_line) => {
                match self.get_next_token(false)? {
                    None => return Err(SyntaxError::new("Expected beginning of block statement but found end of file.".to_string(),
                                                        None)),
                    Some(ref token) => match *token {
                        Token::LeftBrace(_line) => (),
                        _ => return Err(SyntaxError::new("Block statement missing opening brace.".to_string(),
                                                         Some(token.clone()))),
                    }
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
        match token {
            Token::EOL(_line) => {
                // multi-line block statement
                while let Some(stmt) = self.parse()? {
                    stmts.push(Box::new(stmt));
                    match self.get_next_token(false)? {
                        None => return Err(SyntaxError::new("Unterminated block statement.".to_string(),
                                                            None)),
                        Some(ref token) => match *token {
                            Token::RightBrace(_line) => break,
                            _ => self.unused_lookahead = Some(token.clone())
                        }
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
                            Some(ref token) => match *token {
                                Token::RightBrace(_line) => (),
                                _ => return Err(SyntaxError::new("Block statement missing closing brace.".to_string(),
                                                                 Some(token.clone()))),
                            }
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
            match operator {
                Token::Or(_line) => {
                    let right = match self.parse_and_expr()? {
                        None => return Err(SyntaxError::new("Or expression missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_and_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_eq_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::And(_line) => {
                    let right = match self.parse_eq_expr()? {
                        None => return Err(SyntaxError::new("And expression missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_eq_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_comp_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::EqualEqual(_line) | Token::BangEqual(_line) => {
                    let right = match self.parse_comp_expr()? {
                        None => return Err(SyntaxError::new("Equality comparison missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_comp_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_concat_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::Greater(_line) |
                Token::GreaterEqual(_line) |
                Token::Lesser(_line) |
                Token::LesserEqual(_line) => {
                    let right = match self.parse_concat_expr()? {
                        None => return Err(SyntaxError::new("Comparison operator missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_concat_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_add_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::Ampersand(_line) => {
                    let right = match self.parse_add_expr()? {
                        None => return Err(SyntaxError::new("Concatenation expression missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_add_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_mul_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::Plus(_line) | Token::Minus(_line) => {
                    let right = match self.parse_mul_expr()? {
                        None => return Err(SyntaxError::new("Expression missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_mul_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let mut expr = match self.parse_unary_expr()? {
            None => return Ok(None),
            Some(expr) => expr
        };
        while let Some(operator) = self.get_next_token(true)? {
            match operator {
                Token::Star(_line) | Token::Slash(_line) => {
                    let right = match self.parse_unary_expr()? {
                        None => return Err(SyntaxError::new("Expression missing right side.".to_string(),
                                                            Some(operator))),
                        Some(expr) => expr
                    };
                    expr = Expr::Binary { left: Box::new(expr), operator, right: Box::new(right) };
                }
                _ => {
                    self.unused_lookahead = Some(operator);
                    break;
                }
            }
        }
        Ok(Some(expr))
    }

    fn parse_unary_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        let curr_token = match self.get_next_token(true)? {
            None => return Ok(None),
            Some(token) => token
        };

        match curr_token {
            Token::Plus(_line) => {
                match self.parse_unary_expr()? {
                    None => Err(SyntaxError::new("Plus unary expression missing operand.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            Token::Minus(_line) => {
                match self.parse_unary_expr()? {
                    None => Err(SyntaxError::new("Minus unary expression missing operand.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            Token::Bang(_line) => {
                match self.parse_unary_expr()? {
                    None => Err(SyntaxError::new("Negate unary expression missing operand.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            Token::Ampersand(_line) => {
                match self.parse_unary_expr()? {
                    None => Err(SyntaxError::new("Stringify unary expression missing operand.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => Ok(Some(Expr::Unary { operator: curr_token, right: Box::new(expr) }))
                }
            }
            Token::LeftParen(_line) => {
                match self.parse_expr()? {
                    None => Err(SyntaxError::new("Unexpected end of file.".to_string(),
                                                 Some(curr_token))),
                    Some(expr) => {
                        match self.get_next_token(false)? {
                            Some(ref token) => match *token {
                                Token::RightParen(_line) => Ok(Some(Expr::Grouping { expr: Box::new(expr) })),
                                _ => Err(SyntaxError::new("Unexpected end of grouping.".to_string(),
                                                          Some(curr_token))),
                            }
                            _ => Err(SyntaxError::new("Unexpected end of file.".to_string(), None))
                        }
                    }
                }
            }
            Token::String { .. } => Ok(Some(Expr::Literal { value: curr_token.clone() })),
            Token::Integer { .. } => Ok(Some(Expr::Literal { value: curr_token.clone() })),
            Token::Float { .. } => Ok(Some(Expr::Literal { value: curr_token.clone() })),
            Token::True(_line) => Ok(Some(Expr::Literal { value: curr_token })),
            Token::False(_line) => Ok(Some(Expr::Literal { value: curr_token })),
            Token::Identifier { .. } => {
                self.unused_lookahead = Some(curr_token.clone());
                match self.parse_ident()? {
                    None => Ok(None),
                    Some(ident) => {
                        match self.get_next_token(true)? {
                            None => Ok(Some(ident)),
                            Some(ref token) => match *token {
                                Token::LeftParen(_line) => self.parse_fn_call(ident),
                                _ => {
                                    self.unused_lookahead = Some(token.clone());
                                    Ok(Some(ident))
                                }
                            }
                        }
                    }
                }
            }
            _ => Err(SyntaxError::new("Unexpected token.".to_string(), Some(curr_token)))
        }
    }

    fn parse_fn_call(&mut self, ident: Expr) -> Result<Option<Expr>, SyntaxError> {
        let mut args = Vec::new();
        while let Some(curr_token) = self.get_next_token(false)? {
            match curr_token {
                Token::Comma(_line) => continue,
                Token::RightParen(_line) => return Ok(Some(Expr::FnCall { ident: Box::new(ident), args })),
                _ => {
                    self.unused_lookahead = Some(curr_token.clone());
                    let param = match self.parse_expr()? {
                        None => return Err(SyntaxError::new(
                            "Unterminated function call parameter list.".to_string(),
                            Some(curr_token))),
                        Some(expr) => expr
                    };
                    args.push(Box::new(param));
                }
            }
        }
        Err(SyntaxError::new("Unterminated function call parameter list.".to_string(), None))
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

        match curr_token {
            Token::Identifier { .. } => Ok(Some(Expr::Identifier { ident: curr_token.clone() })),
            _ => Err(SyntaxError::new("Expected identifier.".to_string(), Some(curr_token)))
        }
    }

    fn sync(&mut self) {
        while let Some(token) = self.lexer.next() {
            match token {
                Token::EOL(_line) => return,
                Token::If(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::While(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Next(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Break(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Let(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Print(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Err(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Scan(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Return(_line) => {
                    self.unused_lookahead = Some(token);
                    return;
                }
                Token::Def(_line) => {
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
                    _ => panic!()
                };
                match *true_body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match *stmt {
                            Stmt::Break => (),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
                    _ => panic!()
                };
                match *true_body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match *stmt {
                            Stmt::Break => (),
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                };
                match *false_body.unwrap() {
                    Stmt::If { cond, true_body, false_body } => {
                        match *cond {
                            Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 2, literal: "b".to_string() }),
                            _ => panic!()
                        };
                        match *true_body {
                            Stmt::Block { stmts } => {
                                let stmt = &**stmts.iter().next().unwrap();
                                match *stmt {
                                    Stmt::Next => (),
                                    _ => panic!()
                                }
                            }
                            _ => panic!()
                        };
                        match *false_body.unwrap() {
                            Stmt::Block { stmts } => {
                                let stmt = &**stmts.iter().next().unwrap();
                                match *stmt {
                                    Stmt::Break => return,
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
                    _ => panic!()
                };
                match *body {
                    Stmt::Block { stmts } => {
                        let stmt = &**stmts.iter().next().unwrap();
                        match *stmt {
                            Stmt::Break => return,
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
                    _ => panic!()
                };
                match *expr {
                    Expr::Binary { ref left, ref operator, ref right } => {
                        if !operator.eq(&Token::Plus(0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Literal { ref value } => assert_eq!(*value, Token::Integer { line: 0, literal: 1 }),
                            _ => panic!()
                        };
                        match **right {
                            Expr::Literal { ref value } => assert_eq!(*value, Token::Integer { line: 0, literal: 2 }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
                    _ => panic!()
                };
                match *expr {
                    Expr::Binary { ref left, ref operator, ref right } => {
                        if !operator.eq(&Token::Ampersand(0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Literal { ref value } => assert_eq!(*value,
                                                                      Token::String { line: 0, literal: "hi ".to_string() }),
                            _ => panic!()
                        };
                        match **right {
                            Expr::Literal { ref value } => assert_eq!(*value,
                                                                      Token::String { line: 0, literal: "user".to_string() }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
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
                    Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "f".to_string() }),
                    _ => panic!()
                };
                let mut iter = params.iter();
                let param_a = &**iter.next().unwrap();
                match param_a {
                    &Expr::Identifier { ref ident } => {
                        assert_eq!(*ident, Token::Identifier { line: 0, literal: "a".to_string() });
                        let param_b = &**iter.next().unwrap();
                        match param_b {
                            &Expr::Identifier { ref ident } => assert_eq!(*ident, Token::Identifier { line: 0, literal: "b".to_string() }),
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
                                    Expr::Literal { ref value } => assert_eq!(*value,
                                                                              Token::Float { line: 1, literal: 1.0 }),
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
                Expr::Identifier { ident } => assert_eq!(ident, Token::Identifier { line: 0, literal: "a".to_string() }),
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
                    if !operator.eq(&Token::Or(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Or(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::And(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::And(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::EqualEqual(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::BangEqual(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::Greater(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::GreaterEqual(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::Lesser(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::LesserEqual(0)) {
                        panic!()
                    }
                    match **left {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                    if !operator.eq(&Token::Ampersand(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Ampersand(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::Plus(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Plus(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::Minus(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Minus(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::Star(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Star(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::Slash(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "c".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    };
                    match **left {
                        Expr::Binary { ref left, ref operator, ref right } => {
                            if !operator.eq(&Token::Slash(0)) {
                                panic!()
                            }
                            match **left {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) => (),
                                _ => panic!()
                            };
                            match **right {
                                Expr::Identifier { ref ident } if ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) => (),
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
                    if !operator.eq(&Token::Plus(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
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
                    if !operator.eq(&Token::Minus(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
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
                    if !operator.eq(&Token::Bang(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!()
                            }
                        }
                        _ => panic!()
                    }
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
                    if !operator.eq(&Token::Ampersand(0)) {
                        panic!()
                    }
                    match **right {
                        Expr::Identifier { ref ident } => {
                            if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                panic!();
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
                        if !operator.eq(&Token::Plus(0)) {
                            panic!()
                        }
                        match **left {
                            Expr::Identifier { ref ident } => {
                                if !ident.eq(&Token::Identifier { line: 0, literal: "a".to_string() }) {
                                    panic!()
                                }
                            }
                            _ => panic!()
                        };
                        match **right {
                            Expr::Identifier { ref ident } => {
                                if !ident.eq(&Token::Identifier { line: 0, literal: "b".to_string() }) {
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
                        Expr::Identifier { ref ident } => assert_eq!(*ident, Token::Identifier { line: 0, literal: "f".to_string() }),
                        _ => panic!()
                    }
                    let mut iter = args.iter();
                    let arg_a = &**iter.next().unwrap();
                    match arg_a {
                        &Expr::Identifier { ref ident } => {
                            assert_eq!(*ident, Token::Identifier { line: 0, literal: "a".to_string() });
                            let arg_b = &**iter.next().unwrap();
                            match arg_b {
                                &Expr::Identifier { ref ident } => {
                                    assert_eq!(*ident, Token::Identifier { line: 0, literal: "b".to_string() });
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
