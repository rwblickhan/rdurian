use std::str::Chars;
use token::*;

pub struct Lexer<'a> {
    iter: Chars<'a>,
    curr_line: Line,
    unused_lookahead: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let iter = source.chars();
        Lexer { iter, curr_line: 0, unused_lookahead: None }
    }

    fn match_token(&mut self, c: char) -> Option<Token> {
        match c {
            ' ' => self.next(),
            ',' => Some(Token::Comma(self.curr_line)),
            '{' => Some(Token::LeftBrace(self.curr_line)),
            '}' => Some(Token::RightBrace(self.curr_line)),
            '(' => Some(Token::LeftParen(self.curr_line)),
            ')' => Some(Token::RightParen(self.curr_line)),
            '&' => Some(Token::Ampersand(self.curr_line)),
            '+' => Some(Token::Plus(self.curr_line)),
            '-' => Some(Token::Minus(self.curr_line)),
            '*' => Some(Token::Star(self.curr_line)),
            '/' => Some(Token::Slash(self.curr_line)),
            '=' => {
                match self.iter.next() {
                    None => Some(Token::Equal(self.curr_line)),
                    Some(ch) => match ch {
                        '=' => Some(Token::EqualEqual(self.curr_line)),
                        _ => {
                            self.unused_lookahead = Some(ch);
                            Some(Token::Equal(self.curr_line))
                        }
                    }
                }
            }
            '!' => {
                match self.iter.next() {
                    None => Some(Token::Bang(self.curr_line)),
                    Some(ch) => match ch {
                        '=' => Some(Token::BangEqual(self.curr_line)),
                        _ => {
                            self.unused_lookahead = Some(ch);
                            Some(Token::Bang(self.curr_line))
                        }
                    }
                }
            }
            '>' => {
                match self.iter.next() {
                    None => Some(Token::Greater(self.curr_line)),
                    Some(ch) => match ch {
                        '=' => Some(Token::GreaterEqual(self.curr_line)),
                        _ => {
                            self.unused_lookahead = Some(ch);
                            Some(Token::Greater(self.curr_line))
                        }
                    }
                }
            }
            '<' => {
                match self.iter.next() {
                    None => Some(Token::Lesser(self.curr_line)),
                    Some(ch) => match ch {
                        '=' => Some(Token::LesserEqual(self.curr_line)),
                        _ => {
                            self.unused_lookahead = Some(ch);
                            Some(Token::Lesser(self.curr_line))
                        }
                    }
                }
            }
            '\n' => {
                self.curr_line += 1;
                Some(Token::EOL(self.curr_line - 1))
            }
            '\r' => {
                match self.iter.next() {
                    None => Some(Token::SyntaxError {
                        line: self.curr_line,
                        error: "\\r line terminator is currently unsupported by Durian".to_string(),
                    }),
                    Some(ch) => match ch {
                        '\n' => {
                            self.curr_line += 1;
                            Some(Token::EOL(self.curr_line - 1))
                        }
                        _ => {
                            self.unused_lookahead = Some(ch);
                            Some(Token::SyntaxError {
                                line: self.curr_line,
                                error:
                                "\\r line terminator is currently unsupported by Durian".to_string(),
                            })
                        }
                    }
                }
            }
            '"' => self.tokenize_string_literal(),
            _ => {
                let mut lit = String::new();
                lit.push(c);
                if c.is_digit(10) {
                    return self.tokenize_numeric_literal(&mut lit);
                } else if c.is_alphabetic() {
                    self.scan_into_buffer(&mut lit);
                    if lit.eq(&"and") {
                        return Some(Token::And(self.curr_line));
                    }
                    if lit.eq(&"or") {
                        return Some(Token::Or(self.curr_line));
                    }
                    if lit.eq(&"True") {
                        return Some(Token::True(self.curr_line));
                    }
                    if lit.eq(&"False") {
                        return Some(Token::False(self.curr_line));
                    }
                    if lit.eq(&"def") {
                        return Some(Token::Def(self.curr_line));
                    }
                    if lit.eq(&"let") {
                        return Some(Token::Let(self.curr_line));
                    }
                    if lit.eq(&"if") {
                        return Some(Token::If(self.curr_line));
                    }
                    if lit.eq(&"else") {
                        return Some(Token::Else(self.curr_line));
                    }
                    if lit.eq(&"elif") {
                        return Some(Token::Elif(self.curr_line));
                    }
                    if lit.eq(&"while") {
                        return Some(Token::While(self.curr_line));
                    }
                    if lit.eq(&"return") {
                        return Some(Token::Return(self.curr_line));
                    }
                    if lit.eq(&"next") {
                        return Some(Token::Next(self.curr_line));
                    }
                    if lit.eq(&"break") {
                        return Some(Token::Break(self.curr_line));
                    }
                    if lit.eq(&"print") {
                        return Some(Token::Print(self.curr_line));
                    }
                    if lit.eq(&"scan") {
                        return Some(Token::Scan(self.curr_line));
                    }
                    if lit.eq(&"err") {
                        return Some(Token::Err(self.curr_line));
                    }
                    return Some(Token::Identifier { line: self.curr_line, literal: lit });
                }
                Some(Token::SyntaxError {
                    line: self.curr_line,
                    error: format!("Unexpected input: {}", c),
                })
            }
        }
    }

    fn tokenize_string_literal(&mut self) -> Option<Token> {
        let mut lit = String::new();
        while let Some(ch) = self.iter.next() {
            match ch {
                '"' => return Some(Token::String {
                    line: self.curr_line,
                    literal: lit,
                }),
                _ => lit.push(ch)
            }
        }
        Some(Token::SyntaxError {
            line: self.curr_line,
            error: "Unterminated string literal".to_string(),
        })
    }

    fn tokenize_numeric_literal(&mut self, lit: &mut String) -> Option<Token> {
        while let Some(ch) = self.iter.next() {
            if ch.is_digit(10) {
                lit.push(ch);
            } else {
                self.unused_lookahead = Some(ch);
                break;
            }
        }

        if let Some(lookahead) = self.unused_lookahead {
            if lookahead.eq(&'.') {
                lit.push(lookahead);
                match self.iter.next() {
                    None => return Some(Token::SyntaxError {
                        line: self.curr_line,
                        error: "Floating point literal missing fractional part".to_string(),
                    }),
                    Some(ch) => {
                        if !ch.is_digit(10) {
                            self.unused_lookahead = Some(ch);
                            return Some(Token::SyntaxError {
                                line: self.curr_line,
                                error: "Floating point literal missing fractional part".to_string(),
                            });
                        }
                        lit.push(ch);
                    }
                }
                while let Some(ch) = self.iter.next() {
                    if ch.is_digit(10) {
                        lit.push(ch);
                    } else {
                        self.unused_lookahead = Some(ch);
                        break;
                    }
                }
                return Some(Token::Float {
                    line: self.curr_line,
                    literal: lit.parse().unwrap(),
                });
            }
        }

        Some(Token::Integer { line: self.curr_line, literal: lit.parse().unwrap() })
    }

    fn scan_into_buffer(&mut self, lit: &mut String) {
        while let Some(ch) = self.iter.next() {
            if ch.is_alphabetic() ||
                ch.is_digit(10) ||
                ch.eq(&'_') {
                lit.push(ch)
            } else {
                self.unused_lookahead = Some(ch);
                break;
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let c = match self.unused_lookahead {
            Some(c) => c,
            None => match self.iter.next() {
                Some(c) => c,
                None => return None
            }
        };
        self.unused_lookahead = None;
        self.match_token(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_single_comma() {
        let mut lexer = Lexer::new(&",");
        assert_eq!(Token::Comma(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_left_brace() {
        let mut lexer = Lexer::new(&"{");
        assert_eq!(Token::LeftBrace(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_right_brace() {
        let mut lexer = Lexer::new(&"}");
        assert_eq!(Token::RightBrace(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_left_paren() {
        let mut lexer = Lexer::new(&"(");
        assert_eq!(Token::LeftParen(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_right_paren() {
        let mut lexer = Lexer::new(&")");
        assert_eq!(Token::RightParen(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_ampersand() {
        let mut lexer = Lexer::new(&"&");
        assert_eq!(Token::Ampersand(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_plus() {
        let mut lexer = Lexer::new(&"+");
        assert_eq!(Token::Plus(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_minus() {
        let mut lexer = Lexer::new(&"-");
        assert_eq!(Token::Minus(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_star() {
        let mut lexer = Lexer::new(&"*");
        assert_eq!(Token::Star(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_slash() {
        let mut lexer = Lexer::new(&"/");
        assert_eq!(Token::Slash(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_equal() {
        let mut lexer = Lexer::new(&"=");
        assert_eq!(Token::Equal(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_equal_equal() {
        let mut lexer = Lexer::new(&"==");
        assert_eq!(Token::EqualEqual(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_greater() {
        let mut lexer = Lexer::new(&">");
        assert_eq!(Token::Greater(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_greater_equal() {
        let mut lexer = Lexer::new(&">=");
        assert_eq!(Token::GreaterEqual(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_lesser() {
        let mut lexer = Lexer::new(&"<");
        assert_eq!(Token::Lesser(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_lesser_equal() {
        let mut lexer = Lexer::new(&"<=");
        assert_eq!(Token::LesserEqual(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_bang() {
        let mut lexer = Lexer::new(&"!");
        assert_eq!(Token::Bang(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_bang_equal() {
        let mut lexer = Lexer::new(&"!=");
        assert_eq!(Token::BangEqual(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_cr_newline() {
        let mut lexer = Lexer::new(&"\n");
        assert_eq!(Token::EOL(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_crlf_newline() {
        let mut lexer = Lexer::new(&"\r\n");
        assert_eq!(Token::EOL(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_single_invalid_newline() {
        let mut lexer = Lexer::new(&"\r");
        assert_eq!(Token::SyntaxError {
            line: 0,
            error: "\\r line terminator is currently unsupported by Durian".to_string(),
        }, lexer.next().unwrap());
    }

    #[test]
    fn test_lex_and() {
        let mut lexer = Lexer::new(&"and");
        assert_eq!(Token::And(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_or() {
        let mut lexer = Lexer::new(&"or");
        assert_eq!(Token::Or(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_def() {
        let mut lexer = Lexer::new(&"def");
        assert_eq!(Token::Def(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_let() {
        let mut lexer = Lexer::new(&"let");
        assert_eq!(Token::Let(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_if() {
        let mut lexer = Lexer::new(&"if");
        assert_eq!(Token::If(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_else() {
        let mut lexer = Lexer::new(&"else");
        assert_eq!(Token::Else(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_elif() {
        let mut lexer = Lexer::new(&"elif");
        assert_eq!(Token::Elif(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_while() {
        let mut lexer = Lexer::new(&"while");
        assert_eq!(Token::While(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_return() {
        let mut lexer = Lexer::new(&"return");
        assert_eq!(Token::Return(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_next() {
        let mut lexer = Lexer::new(&"next");
        assert_eq!(Token::Next(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_break() {
        let mut lexer = Lexer::new(&"break");
        assert_eq!(Token::Break(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_print() {
        let mut lexer = Lexer::new(&"print");
        assert_eq!(Token::Print(0), lexer.next().unwrap())
    }

    #[test]
    fn test_lex_scan() {
        let mut lexer = Lexer::new(&"scan");
        assert_eq!(Token::Scan(0), lexer.next().unwrap())
    }

    #[test]
    fn test_lex_err() {
        let mut lexer = Lexer::new(&"err");
        assert_eq!(Token::Err(0), lexer.next().unwrap())
    }

    #[test]
    fn test_lex_int_lit() {
        let mut lexer = Lexer::new(&"10");
        assert_eq!(Token::Integer { line: 0, literal: 10 },
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_float_lit() {
        let mut lexer = Lexer::new(&"10.0");
        assert_eq!(Token::Float { line: 0, literal: 10.0 },
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_true_lit() {
        let mut lexer = Lexer::new(&"True");
        assert_eq!(Token::True(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_false_lit() {
        let mut lexer = Lexer::new(&"False");
        assert_eq!(Token::False(0),
                   lexer.next().unwrap())
    }

    #[test]
    fn test_lex_string_lit() {
        let mut lexer = Lexer::new(&"\"Hi, this is a string.\"");
        assert_eq!(Token::String { line: 0, literal: "Hi, this is a string.".to_string() },
                   lexer.next().unwrap());
    }

    #[test]
    fn test_lex_unterminated_string_lit() {
        let mut lexer = Lexer::new(&"\"Hi, this is a string.");
        assert_eq!(Token::SyntaxError { line: 0, error: "Unterminated string literal".to_string() },
                   lexer.next().unwrap());
    }

    #[test]
    fn test_lex_ident() {
        let mut lexer = Lexer::new(&"a_ident10");
        assert_eq!(Token::Identifier { line: 0, literal: "a_ident10".to_string() },
                   lexer.next().unwrap());
    }

    #[test]
    fn test_lexer_ignore_whitespace() {
        let mut lexer = Lexer::new(&"  ( ,) }");
        assert_eq!(Token::LeftParen(0),
                   lexer.next().unwrap());
        assert_eq!(Token::Comma(0),
                   lexer.next().unwrap());
        assert_eq!(Token::RightParen(0),
                   lexer.next().unwrap());
        assert_eq!(Token::RightBrace(0),
                   lexer.next().unwrap());
    }
}
