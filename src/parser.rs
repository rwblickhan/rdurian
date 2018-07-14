use ast::*;
use lexer::Lexer;
use token::Token;
use token::TokenType;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    unused_lookahead: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser { lexer, unused_lookahead: None }
    }

    fn parse(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        match self.parse_stmt()? {
            None => Ok(None),
            Some(stmt) => {
                match self.lexer.next() {
                    None => Err(SyntaxError::new("No newline at end of file.".to_string(),
                                                 None)),
                    Some(token) => {
                        if !token.token_type.eq(&TokenType::EOL) {
                            return Err(SyntaxError::new("No newline at end of statement.".to_string(),
                                                        Some(token)));
                        }
                        Ok(Some(stmt))
                    }
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        let curr_token = match self.unused_lookahead {
            None => match self.lexer.next() {
                None => return Ok(None),
                Some(ref token) => token.clone(),
            }
            Some(ref token) => token.clone()
        };
        self.unused_lookahead = None;

        match curr_token.token_type {
            TokenType::SyntaxError => Err(SyntaxError::new("Syntax error while lexing.".to_string(),
                                                           Some(curr_token))),
            TokenType::EOL => self.parse_stmt(),
            TokenType::If => self.parse_if_stmt(),
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

                match self.parse_block_stmt()? {
                    None => return Err(SyntaxError::new("While statement missing body.".to_string(),
                                                        Some(curr_token))),
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
                let token = match self.lexer.next() {
                    None => return Err(SyntaxError::new("Let statement missing =.".to_string(), Some(curr_token))),
                    Some(token) => token
                };
                if !token.token_type.eq(&TokenType::Equal) {
                    return Err(SyntaxError::new("Let statement identifier followed by invalid token.".to_string(),
                                                Some(token)));
                }
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
                let token = match self.lexer.next() {
                    None => return Err(SyntaxError::new("Function definition missing (.".to_string(),
                                                        Some(curr_token))),
                    Some(token) => token
                };
                if !token.token_type.eq(&TokenType::LeftParen) {
                    return Err(SyntaxError::new("Function definition missing (.".to_string(),
                                                Some(token)));
                }
                let mut params = Vec::new();
                while let Some(curr_token) = self.lexer.next() {
                    if curr_token.token_type.eq(&TokenType::Comma) {
                        // TODO ugly hack that technically doesn't meet the spec
                        continue;
                    }
                    if curr_token.token_type.eq(&TokenType::RightParen) {
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
                        let param = match self.parse_ident()? {
                            None => return Err(SyntaxError::new("Function definition parameter not a valid lvalue.".to_string(),
                                                                Some(curr_token))),
                            Some(expr) => expr
                        };
                        params.push(Box::new(param));
                    }
                }
                Err(SyntaxError::new("Unterminated function parameter list.".to_string(),
                                     Some(curr_token)))
            }
            _ => {
                let expr = match self.parse_expr()? {
                    None => return Ok(None),
                    Some(expr) => expr
                };
                match self.lexer.next() {
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

    fn parse_if_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        // TODO
        Err(SyntaxError::new("Unimplemented.".to_string(), None))
    }

    fn parse_block_stmt(&mut self) -> Result<Option<Stmt>, SyntaxError> {
        // TODO
        Err(SyntaxError::new("Unimplemented.".to_string(), None))
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>, SyntaxError> {
        // TODO
        Err(SyntaxError::new("Unimplemented.".to_string(), None))
    }

    fn parse_ident(&mut self) -> Result<Option<Expr>, SyntaxError> {
        // TODO
        Err(SyntaxError::new("Unimplemented.".to_string(), None))
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
    fn test_next_stmt() {
        let mut parser = Parser::new(Lexer::new("next\n"));
        match parser.next().unwrap() {
            Stmt::Next => return,
            _ => panic!()
        }
    }

    #[test]
    fn test_break_stmt() {
        let mut parser = Parser::new(Lexer::new("break\n"));
        match parser.next().unwrap() {
            Stmt::Break => return,
            _ => panic!()
        }
    }
}
