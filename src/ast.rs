use token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block { stmts: Vec<Box<Stmt>> },
    If { token: Token, cond: Box<Expr>, true_body: Box<Stmt>, false_body: Option<Box<Stmt>> },
    While { token: Token, cond: Box<Expr>, body: Box<Stmt> },
    Next(Token),
    Break(Token),
    Let { token: Token, ident: Box<Expr>, expr: Box<Expr> },
    Assign { ident: Box<Expr>, expr: Box<Expr> },
    Print { token: Token, expr: Box<Expr> },
    Err { token: Token, expr: Box<Expr> },
    Scan { token: Token, ident: Box<Expr> },
    Return { token: Token, expr: Box<Expr> },
    FnDecl { token: Token, ident: Box<Expr>, params: Vec<Box<Expr>>, body: Box<Stmt> },
    Expr { expr: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Unary { operator: Token, right: Box<Expr> },
    Literal { value: Token },
    Identifier { ident: Token },
    Grouping { expr: Box<Expr> },
    FnCall { ident: Box<Expr>, args: Vec<Box<Expr>> },
    Exec { command: String, args: Option<Vec<Box<Expr>>> },
    Pipeline { commands: Vec<Box<Expr>> },
}

pub struct SyntaxError {
    pub msg: String,
    pub token: Option<Token>,
}

impl SyntaxError {
    pub fn new(msg: String, token: Option<Token>) -> SyntaxError {
        SyntaxError { msg, token }
    }
}
