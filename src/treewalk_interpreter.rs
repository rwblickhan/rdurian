use ast::{Expr, Stmt};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::f64;
use std::io::{stderr, stdin, stdout, BufRead, Error, Write};
use std::ops::Deref;
use std::rc::Rc;
use token::Token;

pub struct Interpreter {
    curr_scope: Rc<RefCell<Environment>>
}

#[derive(Clone, PartialEq)]
enum RuntimeObject {
    String(String),
    Integer(i32),
    Float(f64),
    Bool(bool),
    Function { params: Vec<String>, closure: Rc<RefCell<Environment>>, body: Box<Stmt> },
    Nil,
}

impl fmt::Display for RuntimeObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeObject::String(s) => write!(f, "{}", s),
            RuntimeObject::Integer(i) => write!(f, "{}", i),
            RuntimeObject::Float(fl) => write!(f, "{}", fl),
            RuntimeObject::Bool(b) => write!(f, "{}", b),
            RuntimeObject::Function { .. } => write!(f, "function"), // TODO
            RuntimeObject::Nil => write!(f, "nil")
        }
    }
}

enum RuntimeException {
    RuntimeError(String),
    Next,
    Break,
    Return(RuntimeObject),
}

impl From<Error> for RuntimeException {
    fn from(error: Error) -> Self {
        RuntimeException::RuntimeError(error.to_string())
    }
}

type RuntimeIdentifier = String;

#[derive(Clone, PartialEq)]
struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    env: HashMap<RuntimeIdentifier, RuntimeObject>,
}

impl Environment {
    fn new(parent: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment { parent, env: HashMap::new() }
    }

    fn get(&self, ident: &RuntimeIdentifier) -> Result<RuntimeObject, RuntimeException> {
        match self.env.get(ident) {
            Some(obj) => Ok(obj.deref().clone()),
            None => match self.parent {
                Some(ref enclosing_scope) => enclosing_scope.borrow().get(ident),
                None => Err(RuntimeException::RuntimeError(format!("Undeclared identifier {}", ident)))
            }
        }
    }

    fn declare(&mut self, ident: &RuntimeIdentifier, val: &RuntimeObject) -> Result<(), RuntimeException> {
        match self.get(ident) {
            Ok(..) => Err(RuntimeException::RuntimeError(format!("Identifier {} already declared", ident))),
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
                Some(ref mut enclosing_scope) => enclosing_scope.borrow_mut().assign(ident, val),
                None => Err(RuntimeException::RuntimeError(format!("Undeclared identifier {}", ident)))
            }
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter { curr_scope: Rc::new(RefCell::new(Environment::new(None))) }
    }
}

impl Interpreter {
    pub fn interp(&mut self, stmt: &Stmt) {
        match self.interp_stmt(stmt) {
            Ok(msg) => println!("{}", msg),
            Err(exception) => match exception {
                RuntimeException::RuntimeError(e) => println!("Runtime error: {}", e),
                RuntimeException::Break => println!("Runtime error: break with no enclosing loop"),
                RuntimeException::Next => println!("Runtime error: next with no enclosing loop"),
                RuntimeException::Return(..) => println!("Runtime error: return with no enclosing function")
            }
        }
    }

    fn interp_stmt(&mut self, stmt: &Stmt) -> Result<String, RuntimeException> {
        match stmt {
            Stmt::Block { stmts } => {
                let old_scope = self.curr_scope.clone();
                self.curr_scope = Rc::new(RefCell::new(Environment::new(Some(old_scope.clone()))));
                let mut buffer = String::new();
                for stmt in stmts {
                    let output_msg = match self.interp_stmt(stmt) {
                        Err(e) => {
                            self.curr_scope = old_scope;
                            return Err(e);
                        }
                        Ok(obj) => obj
                    };
                    buffer.push_str(&format!("{}\n", output_msg));
                }
                self.curr_scope = old_scope;
                Ok(buffer)
            }
            Stmt::If { cond, true_body, false_body } => {
                let cond_obj = self.interp_expr(cond)?;
                if is_truthy(&cond_obj) {
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
            Stmt::While { cond, body } => {
                while is_truthy(&self.interp_expr(cond)?) {
                    if let Err(exception) = self.interp_stmt(body) {
                        match exception {
                            RuntimeException::Break => return Ok("while done".to_string()),
                            RuntimeException::Next => continue,
                            _ => return Err(exception)
                        }
                    };
                }
                Ok("while done".to_string())
            }
            Stmt::Next => Err(RuntimeException::Next),
            Stmt::Break => Err(RuntimeException::Break),
            Stmt::Let { ident, expr } => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                let mut new_scope = Environment::new(Some(self.curr_scope.clone()));
                new_scope.declare(&lval, &assign_val)?;
                self.curr_scope = Rc::new(RefCell::new(new_scope));
                Ok(format!("{} = {}", lval, assign_val))
            }
            Stmt::Assign { ident, expr } => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                self.curr_scope.borrow_mut().assign(&lval, &assign_val)?;
                Ok(format!("{} = {}", lval, assign_val))
            }
            Stmt::Print { expr } => {
                let expr_obj = self.interp_expr(expr)?;
                let stdout = stdout();
                let mut handle = stdout.lock();
                handle.write_all(format!("{}\n", expr_obj).as_bytes())?;
                Ok("stdout done".to_string())
            }
            Stmt::Err { expr } => {
                let expr_obj = self.interp_expr(expr)?;
                let stderr = stderr();
                let mut handle = stderr.lock();
                handle.write_all(format!("{}\n", expr_obj).as_bytes())?;
                Ok("stderr done".to_string())
            }
            Stmt::Scan { ident } => {
                let lval = self.interp_lval(ident)?;
                let stdin = stdin();
                let mut handle = stdin.lock();
                let input = handle.lines().next().unwrap()?;
                self.curr_scope.borrow_mut().assign(&lval, &RuntimeObject::String(input))?;
                Ok("stdin done".to_string())
            }
            Stmt::Return { expr } => {
                let ret_val = self.interp_expr(expr)?;
                Err(RuntimeException::Return(ret_val))
            }
            Stmt::FnDecl { ident, params, body } => {
                let ident_str = self.interp_lval(ident)?;
                let mut param_strs = Vec::new();
                for param in params {
                    let param_str = self.interp_lval(param)?;
                    param_strs.push(param_str);
                }
                let mut param_scope = Environment::new(Some(self.curr_scope.clone()));
                for param_str in &param_strs {
                    param_scope.declare(param_str, &RuntimeObject::Nil)?;
                }
                self.curr_scope.borrow_mut().declare(&ident_str,
                                                     &RuntimeObject::Function {
                                                         params: param_strs,
                                                         closure: Rc::new(RefCell::new(param_scope)),
                                                         body: Box::new(body.deref().clone()),
                                                     })?;
                Ok(format!("Function {} declared", ident_str))
            }
            Stmt::Expr { expr } => Ok(format!("{}", self.interp_expr(expr)?)),
        }
    }

    fn interp_lval(&self, expr: &Expr) -> Result<RuntimeIdentifier, RuntimeException> {
        match expr {
            Expr::Identifier { ident } => match ident {
                Token::Identifier { literal, .. } => Ok(literal.clone()),
                _ => Err(RuntimeException::RuntimeError("Unexpected identifier token.".to_string()))
            }
            _ => Err(RuntimeException::RuntimeError("Expression was not a valid assignment target.".to_string()))
        }
    }

    fn interp_expr(&mut self, expr: &Expr) -> Result<RuntimeObject, RuntimeException> {
        match expr {
            Expr::Binary { left, operator, right } => {
                let left_obj = self.interp_expr(left)?;
                let right_obj = self.interp_expr(right)?;
                match *operator {
                    Token::And(_line) => Ok(RuntimeObject::Bool(is_truthy(&left_obj) && is_truthy(&right_obj))),
                    Token::Or(_line) => Ok(RuntimeObject::Bool(is_truthy(&left_obj) || is_truthy(&right_obj))),
                    Token::Ampersand(_line) => Ok(self.interp_concat(&left_obj, &right_obj)?),
                    Token::Plus(_line) => Ok(self.interp_add(&left_obj, &right_obj)?),
                    Token::Minus(_line) => Ok(self.interp_sub(&left_obj, &right_obj)?),
                    Token::Star(_line) => Ok(self.interp_mul(&left_obj, &right_obj)?),
                    Token::Slash(_line) => Ok(self.interp_fdiv(&left_obj, &right_obj)?),
                    Token::BangEqual(_line) => Err(RuntimeException::RuntimeError("Unimplemented.".to_string())), // TODO
                    Token::EqualEqual(_line) => Err(RuntimeException::RuntimeError("Unimplemented.".to_string())), // TODO
                    Token::GreaterEqual(_line) => Ok(self.interp_ge(&left_obj, &right_obj)?),
                    Token::Greater(_line) => Ok(self.interp_gt(&left_obj, &right_obj)?),
                    Token::LesserEqual(_line) => Ok(self.interp_le(&left_obj, &right_obj)?),
                    Token::Lesser(_line) => Ok(self.interp_lt(&left_obj, &right_obj)?),
                    _ => Err(RuntimeException::RuntimeError("Unexpected token found for binary operator.".to_string()))
                }
            }
            Expr::Unary { operator, right } => {
                let right_obj = self.interp_expr(right)?;
                match *operator {
                    Token::Plus(_line) => self.interp_numberify(&right_obj),
                    Token::Minus(_line) => self.interp_negate(&right_obj),
                    Token::Ampersand(_line) => Ok(self.interp_stringify(&right_obj)?),
                    Token::Bang(_line) => Ok(RuntimeObject::Bool(!is_truthy(&right_obj))),
                    _ => Err(RuntimeException::RuntimeError("Unexpected token found for unary operator.".to_string()))
                }
            }
            Expr::Literal { value } => {
                match *value {
                    Token::String { ref literal, .. } => Ok(RuntimeObject::String(literal.clone())),
                    Token::Integer { literal, .. } => Ok(RuntimeObject::Integer(literal)),
                    Token::Float { literal, .. } => Ok(RuntimeObject::Float(literal)),
                    Token::True(_line) => Ok(RuntimeObject::Bool(true)),
                    Token::False(_line) => Ok(RuntimeObject::Bool(false)),
                    _ => Err(RuntimeException::RuntimeError("Unexpected literal found.".to_string()))
                }
            }
            Expr::Identifier { ident } => match ident {
                Token::Identifier { literal, .. } => self.curr_scope.borrow().get(literal),
                _ => Err(RuntimeException::RuntimeError("Unexpected token for identifier.".to_string()))
            }
            Expr::Grouping { expr } => self.interp_expr(expr),
            Expr::FnCall { ident, args } => {
                let ident_str = self.interp_lval(ident)?;
                let mut func_obj = self.curr_scope.borrow().get(&ident_str)?;
                let mut arg_queue = VecDeque::new();
                for arg in args {
                    arg_queue.push_front(self.interp_expr(arg)?);
                }

                match func_obj {
                    RuntimeObject::Function { ref params, ref closure, ref body } => {
                        let old_scope = self.curr_scope.clone();
                        self.curr_scope = closure.clone();
                        for param in params {
                            match arg_queue.pop_front() {
                                Some(arg) => self.curr_scope.borrow_mut().assign(&param, &arg)?,
                                None => return Err(RuntimeException::RuntimeError("Incorrect number of parameters".to_string()))
                            };
                        }
                        match self.interp_stmt(&body) {
                            Err(RuntimeException::Return(obj)) => {
                                self.curr_scope = old_scope;
                                Ok(obj)
                            }
                            Err(e) => {
                                self.curr_scope = old_scope;
                                Err(e)
                            }
                            _ => {
                                self.curr_scope = old_scope;
                                // implicitly return nil
                                Ok(RuntimeObject::Nil)
                            }
                        }
                    }
                    _ => Err(RuntimeException::RuntimeError("Not a callable value.".to_string()))
                }
            }
        }
    }

    fn interp_concat(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        let mut left_str = match *left {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string(),
            _ => return Err(RuntimeException::RuntimeError(format!("Cannot stringify {}", left)))
        };
        let right_str = match *right {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string(),
            _ => return Err(RuntimeException::RuntimeError(format!("Cannot stringify {}", right)))
        };
        left_str.push_str(&right_str);
        Ok(RuntimeObject::String(left_str))
    }

    fn interp_add(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int + right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float + right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_sub(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int - right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float - right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_mul(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int * right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float * right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_fdiv(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) / f64::from(right_int)))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float / right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_ge(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int >= right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float >= right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_gt(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int > right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float > right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_le(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int <= right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float <= right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_lt(&self, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int < right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float < right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError("Right operand had incompatible type.".to_string()))
                }
            }
            _ => Err(RuntimeException::RuntimeError("Left operand had incompatible type.".to_string()))
        }
    }

    fn interp_stringify(&self, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::String(ref s) => Ok(RuntimeObject::String(s.clone())),
            RuntimeObject::Integer(i) => Ok(RuntimeObject::String(i.to_string())),
            RuntimeObject::Float(f) => Ok(RuntimeObject::String(f.to_string())),
            RuntimeObject::Bool(b) => Ok(RuntimeObject::String(b.to_string())),
            _ => Err(RuntimeException::RuntimeError(format!("Cannot stringify {}", right)))
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
                        Err(_e) => Err(RuntimeException::RuntimeError("Right operand was not convertible to numeric type.".to_string()))
                    }
                }
            }
            _ => Err(RuntimeException::RuntimeError("Right operand was not convertible to numeric type.".to_string()))
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
                        Err(_e) => Err(RuntimeException::RuntimeError("Right operand was not convertible to numeric type.".to_string()))
                    }
                }
            }
            _ => Err(RuntimeException::RuntimeError("Right operand was not convertible to numeric type.".to_string()))
        }
    }
}

fn is_truthy(obj: &RuntimeObject) -> bool {
    match *obj {
        RuntimeObject::Bool(bool) if !bool => false,
        _ => true
    }
}