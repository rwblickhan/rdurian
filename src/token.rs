#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
    EOL, EOF
}

#[derive(Debug, PartialEq)]
pub enum TokenLiteral {
    Bool(bool),
    Int(i32),
    Float(f64),
    String(String),
    Identifier(String)
}

pub type TokenLine = u32;