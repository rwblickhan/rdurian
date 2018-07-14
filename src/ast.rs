use token::Token;

pub enum Stmt {
    Block { stmts: Vec<Box<Stmt>> },
    If { cond: Box<Expr>, true_body: Box<Stmt>, false_body: Option<Box<Stmt>> },
    While { cond: Box<Expr>, body: Box<Stmt> },
    Next,
    Break,
    Let { ident: Box<Expr>, expr: Box<Expr> },
    Assign { ident: Box<Expr>, expr: Box<Expr> },
    Print { expr: Box<Expr> },
    Err { expr: Box<Expr> },
    Scan { ident: Box<Expr> },
    Return { expr: Box<Expr> },
    FnDecl { ident: Box<Expr>, params: Vec<Box<Expr>>, body: Box<Stmt> },
    Expr { expr: Box<Expr> },
}

pub enum Expr {
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Unary { operator: Token, right: Box<Expr> },
    Literal { value: Token },
    FnCall { ident: Token, args: Vec<Box<Expr>> }
}

pub struct SyntaxError {
    msg: String,
    token: Option<Token>,
}

impl SyntaxError {
    pub fn new(msg: String, token: Option<Token>) -> SyntaxError {
        SyntaxError { msg, token }
    }
}
