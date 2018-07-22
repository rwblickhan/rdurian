use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Delimiters
    Comma(Line),
    LeftBrace(Line),
    RightBrace(Line),
    LeftParen(Line),
    RightParen(Line),

    // General operators
    And(Line),
    Or(Line),
    Ampersand(Line),
    Plus(Line),
    Minus(Line),
    Star(Line),
    Slash(Line),

    // Comparison operators
    BangEqual(Line),
    EqualEqual(Line),
    Greater(Line),
    GreaterEqual(Line),
    Lesser(Line),
    LesserEqual(Line),

    // Literals
    String { line: Line, literal: String },
    Integer { line: Line, literal: i32 },
    Float { line: Line, literal: f64 },
    True(Line),
    False(Line),

    // State-related tokens
    Def(Line),
    Let(Line),
    Equal(Line),
    Identifier { line: Line, literal: String },

    // Control-flow keywords
    Bang(Line),
    If(Line),
    Else(Line),
    Elif(Line),
    While(Line),
    Return(Line),
    Next(Line),
    Break(Line),

    // I/O keywords
    Print(Line),
    Scan(Line),
    Err(Line),

    // Control tokens
    EOL(Line),
    SyntaxError { line: Line, error: String },
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Comma(_line) => write!(f, ","),
            Token::LeftBrace(_line) => write!(f, "{{"),
            Token::RightBrace(_line) => write!(f, "}}"),
            Token::LeftParen(_line) => write!(f, "("),
            Token::RightParen(_line) => write!(f, ")"),
            Token::And(_line) => write!(f, "and"),
            Token::Or(_line) => write!(f, "or"),
            Token::Ampersand(_line) => write!(f, "&"),
            Token::Plus(_line) => write!(f, "+"),
            Token::Minus(_line) => write!(f, "-"),
            Token::Star(_line) => write!(f, "*"),
            Token::Slash(_line) => write!(f, "/"),
            Token::BangEqual(_line) => write!(f, "!="),
            Token::EqualEqual(_line) => write!(f, "=="),
            Token::Greater(_line) => write!(f, ">"),
            Token::GreaterEqual(_line) => write!(f, ">="),
            Token::Lesser(_line) => write!(f, "<"),
            Token::LesserEqual(_line) => write!(f, "<="),
            Token::String { ref literal, .. } => write!(f, "{}", literal),
            Token::Integer { ref literal, .. } => write!(f, "{}", literal),
            Token::Float { ref literal, .. } => write!(f, "{}", literal),
            Token::True(_line) => write!(f, "True"),
            Token::False(_line) => write!(f, "False"),
            Token::Def(_line) => write!(f, "def"),
            Token::Let(_line) => write!(f, "let"),
            Token::Equal(_line) => write!(f, "="),
            Token::Identifier { ref literal, .. } => write!(f, "{}", literal),
            Token::Bang(_line) => write!(f, "!"),
            Token::If(_line) => write!(f, "if"),
            Token::Else(_line) => write!(f, "else"),
            Token::Elif(_line) => write!(f, "elif"),
            Token::While(_line) => write!(f, "while"),
            Token::Return(_line) => write!(f, "return"),
            Token::Next(_line) => write!(f, "next"),
            Token::Break(_line) => write!(f, "break"),
            Token::Print(_line) => write!(f, "print"),
            Token::Scan(_line) => write!(f, "scan"),
            Token::Err(_line) => write!(f, "err"),
            Token::EOL(_line) => write!(f, "EOL"),
            Token::SyntaxError { ref error, .. } => write!(f, "Syntax error: {}", error)
        }
    }
}

pub type Line = u32;