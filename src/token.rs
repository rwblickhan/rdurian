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
    Modulo(Line),
    Caret(Line),

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
            Token::Comma(line) => write!(f, ", (line {})", line),
            Token::LeftBrace(line) => write!(f, "{{ (line {})", line),
            Token::RightBrace(line) => write!(f, "}} (line {})", line),
            Token::LeftParen(line) => write!(f, "( (line {})", line),
            Token::RightParen(line) => write!(f, ") (line {})", line),
            Token::And(line) => write!(f, "and (line {})", line),
            Token::Or(line) => write!(f, "or (line {})", line),
            Token::Ampersand(line) => write!(f, "& (line {})", line),
            Token::Plus(line) => write!(f, "+ (line {})", line),
            Token::Minus(line) => write!(f, "- (line {})", line),
            Token::Star(line) => write!(f, "* (line {})", line),
            Token::Slash(line) => write!(f, "/ (line {})", line),
            Token::Modulo(line) => write!(f, "% (line {})", line),
            Token::Caret(line) => write!(f, "^ (line {})", line),
            Token::BangEqual(line) => write!(f, "!= (line {})", line),
            Token::EqualEqual(line) => write!(f, "== (line {})", line),
            Token::Greater(line) => write!(f, "> (line {})", line),
            Token::GreaterEqual(line) => write!(f, ">= (line {})", line),
            Token::Lesser(line) => write!(f, "< (line {})", line),
            Token::LesserEqual(line) => write!(f, "<= (line {})", line),
            Token::String { ref line, ref literal } => write!(f, "{} (line {})", literal, line),
            Token::Integer { ref line, ref literal } => write!(f, "{} (line {})", literal, line),
            Token::Float { ref line, ref literal } => write!(f, "{} (line {})", literal, line),
            Token::True(line) => write!(f, "True (line {})", line),
            Token::False(line) => write!(f, "False (line {})", line),
            Token::Def(line) => write!(f, "def (line {})", line),
            Token::Let(line) => write!(f, "let (line {})", line),
            Token::Equal(line) => write!(f, "= (line {})", line),
            Token::Identifier { ref line, ref literal } => write!(f, "{} (line {})", literal, line),
            Token::Bang(line) => write!(f, "! (line {})", line),
            Token::If(line) => write!(f, "if (line {})", line),
            Token::Else(line) => write!(f, "else (line {})", line),
            Token::Elif(line) => write!(f, "elif (line {})", line),
            Token::While(line) => write!(f, "while (line {})", line),
            Token::Return(line) => write!(f, "return (line {})", line),
            Token::Next(line) => write!(f, "next (line {})", line),
            Token::Break(line) => write!(f, "break (line {})", line),
            Token::Print(line) => write!(f, "print (line {})", line),
            Token::Scan(line) => write!(f, "scan (line {})", line),
            Token::Err(line) => write!(f, "err (line {})", line),
            Token::EOL(line) => write!(f, "EOL (line {})", line),
            Token::SyntaxError { ref line, ref error } => write!(f, "Syntax error: {} (line {})", error, line)
        }
    }
}

pub type Line = u32;