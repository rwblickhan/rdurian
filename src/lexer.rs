use std::iter::Peekable;
use std::str::Chars;
use token::*;

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    curr_line: TokenLine,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let iter = source.chars().peekable();
        Lexer { iter, curr_line: 0 }
    }
}

impl<'a> Lexer<'a> {
    fn match_token(&mut self, c: char) -> Option<Token> {
        match c {
            ' ' => self.next(),
            ',' => Some(Token::new(TokenType::Comma, None, self.curr_line)),
            '{' => Some(Token::new(TokenType::LeftBrace, None, self.curr_line)),
            '}' => Some(Token::new(TokenType::RightBrace, None, self.curr_line)),
            '(' => Some(Token::new(TokenType::LeftBrace, None, self.curr_line)),
            ')' => Some(Token::new(TokenType::RightParen, None, self.curr_line)),
            '&' => Some(Token::new(TokenType::Ampersand, None, self.curr_line)),
            '+' => Some(Token::new(TokenType::Plus, None, self.curr_line)),
            '-' => Some(Token::new(TokenType::Minus, None, self.curr_line)),
            '*' => Some(Token::new(TokenType::Star, None, self.curr_line)),
            '/' => Some(Token::new(TokenType::Slash, None, self.curr_line)),
            '=' => {
                if self.iter.peek()?.eq(&'=') {
                    self.iter.next();
                    Some(Token::new(TokenType::EqualEqual, None, self.curr_line))
                } else {
                    Some(Token::new(TokenType::Equal, None, self.curr_line))
                }
            }
            '!' => {
                if self.iter.peek()?.eq(&'=') {
                    self.iter.next();
                    Some(Token::new(TokenType::BangEqual, None, self.curr_line))
                } else {
                    Some(Token::new(TokenType::Bang, None, self.curr_line))
                }
            }
            '>' => {
                if self.iter.peek()?.eq(&'=') {
                    self.iter.next();
                    Some(Token::new(TokenType::GreaterEqual, None, self.curr_line))
                } else {
                    Some(Token::new(TokenType::Greater, None, self.curr_line))
                }
            }
            '<' => {
                if self.iter.peek()?.eq(&'=') {
                    self.iter.next();
                    Some(Token::new(TokenType::LesserEqual, None, self.curr_line))
                } else {
                    Some(Token::new(TokenType::Lesser, None, self.curr_line))
                }
            }
            '\n' => {
                self.curr_line = self.curr_line + 1;
                Some(Token::new(TokenType::EOL, None, self.curr_line))
            }
            '\r' => {
                if self.iter.peek()?.eq(&'\n') {
                    self.curr_line = self.curr_line + 1;
                    Some(Token::new(TokenType::EOL, None, self.curr_line))
                } else {
                    None
                }
            }
            '"' => {
                let mut lit = String::new();
                while !self.iter.peek()?.eq(&'"') {
                    lit.push(self.iter.next()?);
                }
                Some(Token::new(TokenType::String,
                                Some(TokenLiteral::String(lit)),
                                self.curr_line))
            }
            _ => {
                let mut lit = String::new();
                lit.push(c);
                if c.is_digit(10) {
                    while self.iter.peek()?.is_digit(10) {
                        lit.push(self.iter.next()?);
                    }
                    if self.iter.peek()?.eq(&'.') {
                        lit.push(self.iter.next()?);
                        while self.iter.peek()?.is_digit(10) {
                            lit.push(self.iter.next()?);
                        }
                        return Some(Token::new(TokenType::Float,
                                               Some(TokenLiteral::Float(lit.parse().unwrap())),
                                               self.curr_line));
                    } else {
                        return Some(Token::new(TokenType::Integer,
                                               Some(TokenLiteral::Int(lit.parse().unwrap())),
                                               self.curr_line));
                    }
                } else if c.is_alphabetic() {
                    while self.iter.peek()?.is_alphabetic() ||
                        self.iter.peek()?.is_digit(10) ||
                        self.iter.peek()?.eq(&'_') {
                        lit.push(self.iter.next()?);
                    }
                    if lit.eq(&"and") {
                        return Some(Token::new(TokenType::And, None, self.curr_line));
                    }
                    if lit.eq(&"or") {
                        return Some(Token::new(TokenType::Or, None, self.curr_line));
                    }
                    if lit.eq(&"True") {
                        return Some(Token::new(TokenType::True,
                                               Some(TokenLiteral::Bool(true)),
                                               self.curr_line));
                    }
                    if lit.eq(&"False") {
                        return Some(Token::new(TokenType::False,
                                               Some(TokenLiteral::Bool(false)),
                                               self.curr_line));
                    }
                    if lit.eq(&"def") {
                        return Some(Token::new(TokenType::Def, None, self.curr_line));
                    }
                    if lit.eq(&"let") {
                        return Some(Token::new(TokenType::Let, None, self.curr_line));
                    }
                    if lit.eq(&"if") {
                        return Some(Token::new(TokenType::If, None, self.curr_line));
                    }
                    if lit.eq(&"else") {
                        return Some(Token::new(TokenType::Else, None, self.curr_line));
                    }
                    if lit.eq(&"elif") {
                        return Some(Token::new(TokenType::Elif, None, self.curr_line));
                    }
                    if lit.eq(&"while") {
                        return Some(Token::new(TokenType::While, None, self.curr_line));
                    }
                    if lit.eq(&"return") {
                        return Some(Token::new(TokenType::Return, None, self.curr_line));
                    }
                    if lit.eq(&"next") {
                        return Some(Token::new(TokenType::Next, None, self.curr_line));
                    }
                    if lit.eq(&"break") {
                        return Some(Token::new(TokenType::Break, None, self.curr_line));
                    }
                    if lit.eq(&"print") {
                        return Some(Token::new(TokenType::Print, None, self.curr_line));
                    }
                    if lit.eq(&"scan") {
                        return Some(Token::new(TokenType::Scan, None, self.curr_line));
                    }
                    if lit.eq(&"err") {
                        return Some(Token::new(TokenType::Err, None, self.curr_line));
                    }
                }
                None
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.iter.next() {
            Some(c) => self.match_token(c),
            None => None
        }
    }
}
