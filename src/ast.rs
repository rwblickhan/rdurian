use token::Token;

pub enum Stmt {
    BlockStmt { stmts: Vec<Box<Stmt>> },
    IfStmt { cond: Box<Expr>, true_body: Box<Stmt>, false_body: Option<Box<Stmt>> },
    WhileStmt { cond: Box<Expr>, body: Box<Stmt> },
    NextStmt,
    BreakStmt,
    LetStmt { ident: Box<Expr>, expr: Box<Expr> },
    PrintStmt { expr: Box<Expr> },
    ErrStmt { expr: Box<Expr> },
    ScanStmt { ident: Box<Expr> },
    ReturnStmt { expr: Box<Expr> },
    FnDecl { ident: Box<Expr>, params: Vec<Box<Expr>>, body: Box<Stmt> },
    ExprStmt { expr: Box<Expr> }
}

pub enum Expr {
    BinaryExpr { left: Box<Expr>, operator: Token, right: Box<Expr> },
    UnaryExpr { operator: Token, right: Box<Expr> },
    Literal { value: Token },
    FnCall { ident: Token, args: Vec<Box<Expr>> }
}
