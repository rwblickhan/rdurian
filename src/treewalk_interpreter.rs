use ast::{Expr, Stmt};
use std::collections::HashMap;
use std::fmt;
use std::io::{stderr, stdin, stdout, BufRead, Error, Write};
use std::ops::Deref;
use token::Token;

pub struct Interpreter {
    curr_scope: Environment
}

#[derive(Clone, PartialEq)]
enum RuntimeObject {
    String(String),
    Integer(i32),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for RuntimeObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeObject::String(s) => write!(f, "{}", s),
            RuntimeObject::Integer(i) => write!(f, "{}", i),
            RuntimeObject::Float(fl) => write!(f, "{}", fl),
            RuntimeObject::Bool(b) => write!(f, "{}", b)
        }
    }
}

struct RuntimeException {
    msg: String
}

impl From<Error> for RuntimeException {
    fn from(error: Error) -> Self {
        RuntimeException { msg: error.to_string() }
    }
}

type RuntimeIdentifier = String;

#[derive(Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    env: HashMap<RuntimeIdentifier, RuntimeObject>,
}

impl Environment {
    fn new(parent: Option<Box<Environment>>) -> Environment {
        Environment { parent, env: HashMap::new() }
    }

    fn get(&self, ident: &RuntimeIdentifier) -> Result<RuntimeObject, RuntimeException> {
        match self.env.get(ident) {
            Some(obj) => Ok(obj.deref().clone()),
            None => match self.parent {
                Some(ref enclosing_scope) => enclosing_scope.get(ident),
                None => Err(RuntimeException { msg: format!("Undeclared identifier {}", ident) })
            }
        }
    }

    fn declare(&mut self, ident: &RuntimeIdentifier, val: &RuntimeObject) -> Result<(), RuntimeException> {
        match self.get(ident) {
            Ok(..) => Err(RuntimeException { msg: format!("Identifier {} already declared", ident) }),
            Err(..) => {
                self.env.insert(ident.clone(), val.clone());
                Ok(())
            }
        }
    }

    fn assign(&mut self, ident: &RuntimeIdentifier, val: &RuntimeObject) -> Result<(), RuntimeException> {
        match self.env.get(ident) {
            Some(..) => {
                self.env.insert(ident.clone(), val.clone());
                Ok(())
            }
            None => match self.parent {
                Some(ref mut enclosing_scope) => enclosing_scope.assign(ident, val),
                None => Err(RuntimeException { msg: format!("Undeclared identifier {}", ident) })
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { curr_scope: Environment::new(None) }
    }

    pub fn interp(&mut self, stmt: &Stmt) {
        match self.interp_stmt(stmt) {
            Ok(msg) => println!("{}", msg),
            Err(re) => println!("Runtime error: {}", re.msg)
        }
    }

    fn interp_stmt(&mut self, stmt: &Stmt) -> Result<String, RuntimeException> {
        match stmt {
            Stmt::Block { stmts } => {
                let old_scope = self.curr_scope.clone(); // lol so inefficient
                self.curr_scope = Environment::new(Some(Box::new(old_scope)));
                let mut buffer = String::new();
                for stmt in stmts {
                    let output_msg = match self.interp_stmt(stmt) {
                        Err(e) => {
                            let new_scope = self.curr_scope.clone();
                            self.curr_scope = new_scope.parent.unwrap().deref().clone(); // bye bye new scope
                            return Err(e);
                        }
                        Ok(obj) => obj
                    };
                    buffer.push_str(&format!("{}\n", output_msg));
                }
                let new_scope = self.curr_scope.clone();
                self.curr_scope = new_scope.parent.unwrap().deref().clone(); // bye bye new scope
                Ok(buffer)
            }
            Stmt::If { cond, true_body, false_body } => {
                let cond_obj = self.interp_expr(cond)?;
                if self.is_truthy(&cond_obj) {
                    self.interp_stmt(true_body)?;
                    return Ok("if done".to_string());
                }
                match false_body {
                    None => Ok("if done".to_string()),
                    Some(body) => {
                        self.interp_stmt(body)?;
                        Ok("if done".to_string())
                    }
                }
            }
            Stmt::While {cond, body} => {
                while self.is_truthy(&self.interp_expr(cond)?) {
                    match self.interp_stmt(body) {
                        // TODO yes this is hacky and yes i'm too lazy to fix the types right now
                        Err(ref e) if e.msg.eq("break") => return Ok("while done".to_string()),
                        Err(ref e) if e.msg.eq("next") => continue,
                        _ => ()
                    };
                }
                Ok("while done".to_string())
            }
            Stmt::Next => Err(RuntimeException {msg: "next".to_string()}),
            Stmt::Break => Err(RuntimeException {msg: "break".to_string()}),
            Stmt::Let { ident, expr } => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                self.curr_scope.declare(&lval, &assign_val)?;
                Ok(format!("{} = {}", lval, assign_val))
            }
            Stmt::Assign {ident, expr} => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                self.curr_scope.assign(&lval, &assign_val)?;
                Ok(format!("{} = {}", lval, assign_val))
            }
            Stmt::Print { expr } => {
                let expr_obj = self.interp_expr(expr)?;
                let stdout = stdout();
                let mut handle = stdout.lock();
                handle.write(format!("{}\n", expr_obj).as_bytes())?;
                Ok("stdout done".to_string())
            }
            Stmt::Err { expr } => {
                let expr_obj = self.interp_expr(expr)?;
                let stderr = stderr();
                let mut handle = stderr.lock();
                handle.write(format!("{}\n", expr_obj).as_bytes())?;
                Ok("stderr done".to_string())
            }
            Stmt::Scan { ident } => {
                let lval = self.interp_lval(ident)?;
                let stdin = stdin();
                let mut handle = stdin.lock();
                let input = handle.lines().next().unwrap()?;
                self.curr_scope.assign(&lval, &RuntimeObject::String(input))?;
                Ok("stdin done".to_string())
            }
            Stmt::Expr { expr } => Ok(format!("{}", self.interp_expr(expr)?)),
            _ => Err(RuntimeException { msg: "Unimplemented.".to_string() }) // TODO
        }
    }

    fn interp_lval(&self, expr: &Expr) -> Result<RuntimeIdentifier, RuntimeException> {
        match expr {
            Expr::Identifier { ident } => match ident {
                Token::Identifier {literal, ..} => Ok(literal.clone()),
                _ => Err(RuntimeException { msg: "Unexpected identifier token.".to_string()})
            }
            _ => Err(RuntimeException { msg: "Expression was not a valid assignment target.".to_string() })
        }
    }

    fn interp_expr(&self, expr: &Expr) -> Result<RuntimeObject, RuntimeException> {
        match expr {
            Expr::Binary { left, operator, right } => {
                let left_obj = self.interp_expr(left)?;
                let right_obj = self.interp_expr(right)?;
                match *operator {
                    Token::And(_line) => Ok(RuntimeObject::Bool(self.is_truthy(&left_obj) && self.is_truthy(&right_obj))),
                    Token::Or(_line) => Ok(RuntimeObject::Bool(self.is_truthy(&left_obj) || self.is_truthy(&right_obj))),
                    Token::Ampersand(_line) => Ok(self.interp_concat(&left_obj, &right_obj)),
                    Token::Plus(_line) => Ok(self.interp_add(&left_obj, &right_obj)?),
                    Token::Minus(_line) => Ok(self.interp_sub(&left_obj, &right_obj)?),
                    Token::Star(_line) => Ok(self.interp_mul(&left_obj, &right_obj)?),
                    Token::Slash(_line) => Ok(self.interp_fdiv(&left_obj, &right_obj)?),
                    Token::BangEqual(_line) => Err(RuntimeException { msg: "Unimplemented.".to_string() }), // TODO
                    Token::EqualEqual(_line) => Err(RuntimeException { msg: "Unimplemented.".to_string() }), // TODO
                    Token::GreaterEqual(_line) => Ok(self.interp_ge(&left_obj, &right_obj)?),
                    Token::Greater(_line) => Ok(self.interp_gt(&left_obj, &right_obj)?),
                    Token::LesserEqual(_line) => Ok(self.interp_le(&left_obj, &right_obj)?),
                    Token::Lesser(_line) => Ok(self.interp_lt(&left_obj, &right_obj)?),
                    _ => Err(RuntimeException { msg: "Unexpected token found for binary operator.".to_string() })
                }
            }
            Expr::Unary { operator, right } => {
                let right_obj = self.interp_expr(right)?;
                match *operator {
                    Token::Plus(_line) => self.interp_numberify(&right_obj),
                    Token::Minus(_line) => self.interp_negate(&right_obj),
                    Token::Ampersand(_line) => Ok(self.interp_stringify(&right_obj)),
                    Token::Bang(_line) => Ok(RuntimeObject::Bool(!self.is_truthy(&right_obj))),
                    _ => Err(RuntimeException { msg: "Unexpected token found for unary operator.".to_string() })
                }
            }
            Expr::Literal { value } => {
                match *value {
                    Token::String { ref literal, .. } => Ok(RuntimeObject::String(literal.clone())),
                    Token::Integer { literal, .. } => Ok(RuntimeObject::Integer(literal)),
                    Token::Float { literal, .. } => Ok(RuntimeObject::Float(literal)),
                    Token::True(_line) => Ok(RuntimeObject::Bool(true)),
                    Token::False(_line) => Ok(RuntimeObject::Bool(false)),
                    _ => Err(RuntimeException { msg: "Unexpected literal found.".to_string() })
                }
            }
            Expr::Identifier { ident } => match ident {
                Token::Identifier {literal, ..} => self.curr_scope.get(literal),
                _ => Err(RuntimeException { msg: "Unexpected token for identifier.".to_string()})
            }
            Expr::Grouping { expr } => self.interp_expr(expr),
            Expr::FnCall { ident, args } => Err(RuntimeException { msg: "Unimplemented.".to_string() }) // TODO
        }
    }

    fn is_truthy(&self, obj: &RuntimeObject) -> bool {
        match *obj {
            RuntimeObject::Bool(bool) if !bool => false,
            _ => true
        }
    }

    fn interp_concat(&self, left: &RuntimeObject, right: &RuntimeObject) -> RuntimeObject {
        let mut left_str = match *left {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string()
        };
        let right_str = match *right {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string()
        };
        left_str.push_str(&right_str);
        RuntimeObject::String(left_str)
    }

    fn interp_add(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int + right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float + right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_sub(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int - right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float - right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_mul(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int * right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float * right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_fdiv(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(left_int as f64 / right_int as f64))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float / right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_ge(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int >= right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float >= right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_gt(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int > right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float > right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_le(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int <= right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float <= right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_lt(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int < right_int))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float < right_float))
                    }
                    _ => Err(RuntimeException { msg: "Right operand had incompatible type.".to_string() })
                }
            }
            _ => Err(RuntimeException { msg: "Left operand had incompatible type.".to_string() })
        }
    }

    fn interp_stringify(&self, right: &RuntimeObject) -> RuntimeObject {
        match *right {
            RuntimeObject::String(ref s) => RuntimeObject::String(s.clone()),
            RuntimeObject::Integer(i) => RuntimeObject::String(i.to_string()),
            RuntimeObject::Float(f) => RuntimeObject::String(f.to_string()),
            RuntimeObject::Bool(b) => RuntimeObject::String(b.to_string())
        }
    }

    fn interp_numberify(&self, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::Integer(i) => Ok(RuntimeObject::Integer(i)),
            RuntimeObject::Float(f) => Ok(RuntimeObject::Float(f)),
            RuntimeObject::String(ref s) => {
                match s.parse::<i32>() {
                    Ok(i) => Ok(RuntimeObject::Integer(i)),
                    Err(_e) => match s.parse::<f64>() {
                        Ok(f) => Ok(RuntimeObject::Float(f)),
                        Err(_e) => Err(RuntimeException { msg: "Right operand was not convertible to numeric type.".to_string() })
                    }
                }
            }
            _ => Err(RuntimeException { msg: "Right operand was not convertible to numeric type.".to_string() })
        }
    }

    fn interp_negate(&self, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::Integer(i) => Ok(RuntimeObject::Integer(-i)),
            RuntimeObject::Float(f) => Ok(RuntimeObject::Float(-f)),
            RuntimeObject::String(ref s) => {
                match s.parse::<i32>() {
                    Ok(i) => Ok(RuntimeObject::Integer(-i)),
                    Err(_e) => match s.parse::<f64>() {
                        Ok(f) => Ok(RuntimeObject::Float(-f)),
                        Err(_e) => Err(RuntimeException { msg: "Right operand was not convertible to numeric type.".to_string() })
                    }
                }
            }
            _ => Err(RuntimeException { msg: "Right operand was not convertible to numeric type.".to_string() })
        }
    }
}