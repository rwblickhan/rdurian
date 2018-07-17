use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Option<TokenLiteral>,
    pub line: TokenLine
}

impl Token {
    pub fn new(token_type: TokenType, literal: Option<TokenLiteral>, line: TokenLine) -> Token {
        Token {token_type, literal, line}
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.token_type {
            TokenType::Comma => write!(f, ","),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::And => write!(f, "and"),
            TokenType::Or => write!(f, "or"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Lesser => write!(f, "<"),
            TokenType::LesserEqual => write!(f, "<="),
            TokenType::String => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::String(ref string) => string.clone(),
                _ => "Something has gone terribly wrong...".to_string()
            }),
            TokenType::Integer => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::Int(int) => int,
                _ => -1
            }),
            TokenType::Float => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::Float(float) => float,
                _ => -1.0
            }),
            TokenType::True => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::Bool(b) => b,
                _ => true
            }),
            TokenType::False => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::Bool(b) => b,
                _ => false
            }),
            TokenType::Def => write!(f, "def"),
            TokenType::Let => write!(f, "let"),
            TokenType::Equal => write!(f, "="),
            TokenType::Identifier => write!(f, "{}", match self.clone().literal.unwrap() {
                TokenLiteral::Identifier(string) => string,
                _ => "Something has gone terribly wrong...".to_string()
            }),
            TokenType::Bang => write!(f, "!"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Elif => write!(f, "elif"),
            TokenType::While => write!(f, "while"),
            TokenType::Return => write!(f, "return"),
            TokenType::Next => write!(f, "next"),
            TokenType::Break => write!(f, "break"),
            TokenType::Print => write!(f, "print"),
            TokenType::Scan => write!(f, "scan"),
            TokenType::Err => write!(f, "err"),
            TokenType::EOL => write!(f, "EOL"),
            TokenType::SyntaxError => write!(f, "Syntax error: {}", match self.clone().literal.unwrap() {
                TokenLiteral::Error(err) => err,
                _ => "Something has gone terribly wrong...".to_string()
            })
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Delimiters
    Comma, LeftBrace, RightBrace, LeftParen, RightParen,

    // General operators
    And, Or, Ampersand, Plus, Minus, Star, Slash,

    // Comparison operators
    BangEqual, EqualEqual, Greater, GreaterEqual, Lesser, LesserEqual,

    // Literals
    String, Integer, Float, True, False,

    // State-related tokens
    Def, Let, Equal, Identifier,

    // Control-flow keywords
    Bang, If, Else, Elif, While, Return, Next, Break,

    // I/O keywords
    Print, Scan, Err,

    // Control tokens
    EOL, SyntaxError
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenLiteral {
    Bool(bool),
    Int(i32),
    Float(f64),
    String(String),
    Identifier(String),
    Error(String)
}

pub type TokenLine = u32;