use ast::{Expr, Stmt};
use std::fmt::Write;

pub fn pretty_print_stmt(stmt: &Stmt) -> String {
    match *stmt {
        Stmt::Block { ref stmts } => {
            let mut buffer = String::new();
            writeln!(&mut buffer, "{{").unwrap();
            let mut iter = stmts.iter();
            for stmt in iter {
                writeln!(&mut buffer, "    {}", pretty_print_stmt(&stmt)).unwrap();
            }
            write!(&mut buffer, "}}").unwrap();
            buffer
        }
        Stmt::If { ref cond, ref true_body, ref false_body } => format!("if {} do {} else do {};",
                                                                        pretty_print_expr(&cond),
                                                                        pretty_print_stmt(&true_body),
                                                                        match *false_body {
                                                                            None => "nothing;".to_string(),
                                                                            Some(ref stmt) => pretty_print_stmt(stmt)
                                                                        }),
        Stmt::While { ref cond, ref body } => format!("while {} do {};",
                                                      pretty_print_expr(cond),
                                                      pretty_print_stmt(body)),
        Stmt::Next => "next;".to_string(),
        Stmt::Break => "break;".to_string(),
        Stmt::Let { ref ident, ref expr } => format!("let {} be {};",
                                                     pretty_print_expr(ident),
                                                     pretty_print_expr(expr)),
        Stmt::Assign { ref ident, ref expr } => format!("assign {} to {};",
                                                        pretty_print_expr(expr),
                                                        pretty_print_expr(ident)),
        Stmt::Print { ref expr } => format!("print {};", pretty_print_expr(expr)),
        Stmt::Err { ref expr } => format!("err {};", pretty_print_expr(expr)),
        Stmt::Scan { ref ident } => format!("scan {};", pretty_print_expr(ident)),
        Stmt::Return { ref expr } => format!("return {};", pretty_print_expr(expr)),
        Stmt::FnDecl { ref ident, ref params, ref body } => {
            let mut buffer = String::new();
            write!(&mut buffer, "declare function {} with params ",
                   pretty_print_expr(ident)).unwrap();
            let mut iter = params.iter();
            for expr in iter {
                write!(&mut buffer, "{}, ", pretty_print_expr(expr)).unwrap();
            }
            write!(&mut buffer, "and body {}", pretty_print_stmt(body)).unwrap();
            buffer
        }
        Stmt::Expr { ref expr } => format!("{};", pretty_print_expr(expr))
    }
}

pub fn pretty_print_expr(expr: &Expr) -> String {
    match *expr {
        Expr::Binary { ref left, ref operator, ref right } => format!("({} {} {})",
                                                                      operator.clone(),
                                                                      pretty_print_expr(left),
                                                                      pretty_print_expr(right)),
        Expr::Unary { ref operator, ref right } => format!("({} {})",
                                                           operator.clone(),
                                                           pretty_print_expr(right)),
        Expr::Literal { ref value } => format!("(literal {}", value.clone()),
        Expr::Identifier { ref ident } => format!("(ident {})", ident.clone()),
        Expr::Grouping { ref expr } => format!("({})", pretty_print_expr(expr)),
        Expr::FnCall { ref ident, ref args } => {
            let mut buffer = String::new();
            write!(&mut buffer, "(call function {} with args ",
                   pretty_print_expr(ident)).unwrap();
            let mut iter = args.iter();
            for expr in iter {
                write!(&mut buffer, "{}, ", pretty_print_expr(expr)).unwrap();
            }
            write!(&mut buffer, ")").unwrap();
            buffer
        }
    }
}
