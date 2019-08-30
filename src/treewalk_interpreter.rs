extern crate subprocess;

use ast::{Expr, Stmt};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fmt;
use std::f64;
use std::io::{stderr, stdin, stdout, BufRead, Error, Write};
use std::ops::Deref;
use std::rc::Rc;
use token::Token;

pub struct Interpreter {
    curr_scope: Rc<RefCell<Environment>>,
    had_error: bool,
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
            RuntimeObject::Bool(b) => if *b {
                write!(f, "True")
            } else {
                write!(f, "False")
            },
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

impl From<subprocess::PopenError> for RuntimeException {
    fn from(error: subprocess::PopenError) -> Self {
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

    fn get(&self, ident: RuntimeIdentifier) -> Result<RuntimeObject, RuntimeException> {
        match self.env.get(&ident) {
            Some(obj) => Ok(obj.deref().clone()),
            None => match self.parent {
                Some(ref enclosing_scope) => enclosing_scope.borrow().get(ident),
                None => Err(RuntimeException::RuntimeError(format!("Undeclared identifier {}", ident)))
            }
        }
    }

    fn declare(&mut self, ident: RuntimeIdentifier, val: &RuntimeObject) -> Result<(), RuntimeException> {
        match self.get(ident.clone()) {
            Ok(..) => Err(RuntimeException::RuntimeError(format!("Identifier {} already declared", ident))),
            Err(..) => {
                self.env.insert(ident, val.clone());
                Ok(())
            }
        }
    }

    fn assign(&mut self, ident: RuntimeIdentifier, val: &RuntimeObject) -> Result<(), RuntimeException> {
        match self.env.get(&ident) {
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
        Interpreter {
            curr_scope: Rc::new(RefCell::new(Environment::new(None))),
            had_error: false,
        }
    }
}

impl Interpreter {
    pub fn had_error(&self) -> bool {
        self.had_error
    }

    pub fn interp(&mut self, stmt: &Stmt) -> Option<String> {
        match self.interp_stmt(stmt) {
            Ok(msg) => msg,
            Err(exception) => match exception {
                RuntimeException::RuntimeError(e) => {
                    self.had_error = true;
                    Some(format!("Runtime error: {}", e))
                }
                RuntimeException::Break => {
                    self.had_error = true;
                    Some("Runtime error: break with no enclosing loop".to_string())
                }
                RuntimeException::Next => {
                    self.had_error = true;
                    Some("Runtime error: next with no enclosing loop".to_string())
                }
                RuntimeException::Return(..) => {
                    self.had_error = true;
                    Some("Runtime error: return with no enclosing function".to_string())
                }
            }
        }
    }

    fn interp_stmt(&mut self, stmt: &Stmt) -> Result<Option<String>, RuntimeException> {
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
                    match output_msg {
                        None => (),
                        Some(msg) => buffer.push_str(&format!("{}\n", msg))
                    };
                }
                self.curr_scope = old_scope;
                Ok(Some(buffer))
            }
            Stmt::If { cond, true_body, false_body, .. } => {
                let cond_obj = self.interp_expr(cond)?;
                if is_truthy(&cond_obj) {
                    return Ok(self.interp_stmt(true_body)?);
                }
                match false_body {
                    None => Ok(None),
                    Some(body) => {
                        Ok(self.interp_stmt(body)?)
                    }
                }
            }
            Stmt::While { cond, body, .. } => {
                let mut buffer = String::new();
                while is_truthy(&self.interp_expr(cond)?) {
                    match self.interp_stmt(body) {
                        Err(exception) => match exception {
                            RuntimeException::Break => return Ok(Some(buffer)),
                            RuntimeException::Next => continue,
                            _ => return Err(exception)
                        }
                        Ok(opt_msg) => if let Some(msg) = opt_msg {
                            buffer.push_str(&msg);
                        }
                    };
                }
                Ok(Some(buffer))
            }
            Stmt::Next(_) => Err(RuntimeException::Next),
            Stmt::Break(_) => Err(RuntimeException::Break),
            Stmt::Let { ident, expr, .. } => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                let mut new_scope = Environment::new(Some(self.curr_scope.clone()));
                new_scope.declare(lval.clone(), &assign_val)?;
                self.curr_scope = Rc::new(RefCell::new(new_scope));
                Ok(Some(format!("{} = {}", lval, assign_val)))
            }
            Stmt::Assign { ident, expr } => {
                let lval = self.interp_lval(ident)?;
                let assign_val = self.interp_expr(expr)?;
                self.curr_scope.borrow_mut().assign(lval.clone(), &assign_val)?;
                Ok(Some(format!("{} = {}", lval, assign_val)))
            }
            Stmt::Print { expr, .. } => {
                let expr_obj = self.interp_expr(expr)?;
                let stdout = stdout();
                let mut handle = stdout.lock();
                handle.write_all(format!("{}\n", expr_obj).as_bytes())?;
                Ok(None)
            }
            Stmt::Err { expr, .. } => {
                let expr_obj = self.interp_expr(expr)?;
                let stderr = stderr();
                let mut handle = stderr.lock();
                handle.write_all(format!("{}\n", expr_obj).as_bytes())?;
                Ok(None)
            }
            Stmt::Scan { ident, .. } => {
                let lval = self.interp_lval(ident)?;
                let stdin = stdin();
                let handle = stdin.lock();
                let input = handle.lines().next().unwrap()?;
                self.curr_scope.borrow_mut().assign(lval, &RuntimeObject::String(input))?;
                Ok(None)
            }
            Stmt::Return { expr, .. } => {
                let ret_val = self.interp_expr(expr)?;
                Err(RuntimeException::Return(ret_val))
            }
            Stmt::FnDecl { ident, params, body, .. } => {
                let ident_str = self.interp_lval(ident)?;
                let mut param_strs = Vec::new();
                for param in params {
                    let param_str = self.interp_lval(param)?;
                    param_strs.push(param_str);
                }
                let mut param_scope = Environment::new(Some(self.curr_scope.clone()));
                for param_str in &param_strs {
                    param_scope.declare(param_str.clone(), &RuntimeObject::Nil)?;
                }
                self.curr_scope.borrow_mut().declare(ident_str,
                                                     &RuntimeObject::Function {
                                                         params: param_strs,
                                                         closure: Rc::new(RefCell::new(param_scope)),
                                                         body: Box::new(body.deref().clone()),
                                                     })?;
                Ok(None)
            }
            Stmt::Expr { expr } => Ok(Some(format!("{}", self.interp_expr(expr)?))),
        }
    }

    fn interp_lval(&self, expr: &Expr) -> Result<RuntimeIdentifier, RuntimeException> {
        match expr {
            Expr::Identifier { ident } => match ident {
                Token::Identifier { literal, .. } => Ok(literal.clone()),
                _ => Err(RuntimeException::RuntimeError(format!("Unexpected identifier token: {}", ident)))
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
                    Token::Ampersand(_line) => Ok(self.interp_concat(operator, &left_obj, &right_obj)?),
                    Token::Plus(_line) => Ok(self.interp_add(operator, &left_obj, &right_obj)?),
                    Token::Minus(_line) => Ok(self.interp_sub(operator, &left_obj, &right_obj)?),
                    Token::Star(_line) => Ok(self.interp_mul(operator, &left_obj, &right_obj)?),
                    Token::Slash(_line) => Ok(self.interp_fdiv(operator, &left_obj, &right_obj)?),
                    Token::Modulo(_line) => Ok(self.interp_mod(operator, &left_obj, &right_obj)?),
                    Token::Caret(_line) => Ok(self.interp_exp(operator, &left_obj, &right_obj)?),
                    Token::BangEqual(_line) => Ok(RuntimeObject::Bool(!left_obj.eq(&right_obj))),
                    Token::EqualEqual(_line) => Ok(RuntimeObject::Bool(left_obj.eq(&right_obj))),
                    Token::GreaterEqual(_line) => Ok(self.interp_ge(operator, &left_obj, &right_obj)?),
                    Token::Greater(_line) => Ok(self.interp_gt(operator, &left_obj, &right_obj)?),
                    Token::LesserEqual(_line) => Ok(self.interp_le(operator, &left_obj, &right_obj)?),
                    Token::Lesser(_line) => Ok(self.interp_lt(operator, &left_obj, &right_obj)?),
                    _ => Err(RuntimeException::RuntimeError(format!("Expected binary operator, found: {}", operator)))
                }
            }
            Expr::Unary { operator, right } => {
                let right_obj = self.interp_expr(right)?;
                match *operator {
                    Token::Plus(_line) => self.interp_numberify(operator, &right_obj),
                    Token::Minus(_line) => self.interp_negate(operator, &right_obj),
                    Token::Ampersand(_line) => Ok(self.interp_stringify(operator, &right_obj)?),
                    Token::Bang(_line) => Ok(RuntimeObject::Bool(!is_truthy(&right_obj))),
                    Token::Slash(_line) => self.interp_sqrt(operator, &right_obj),
                    Token::Dollar(_line) => self.interp_env(operator, &right_obj),
                    _ => Err(RuntimeException::RuntimeError(format!("Expected unary operator, found: {} ", operator)))
                }
            }
            Expr::Literal { value } => {
                match *value {
                    Token::String { ref literal, .. } => Ok(RuntimeObject::String(literal.clone())),
                    Token::Integer { literal, .. } => Ok(RuntimeObject::Integer(literal)),
                    Token::Float { literal, .. } => Ok(RuntimeObject::Float(literal)),
                    Token::True(_line) => Ok(RuntimeObject::Bool(true)),
                    Token::False(_line) => Ok(RuntimeObject::Bool(false)),
                    _ => Err(RuntimeException::RuntimeError(format!("Expected literal, found: {}", value)))
                }
            }
            Expr::Identifier { ident } => match ident {
                Token::Identifier { literal, .. } => self.curr_scope.borrow().get(literal.to_string()),
                _ => Err(RuntimeException::RuntimeError(format!("Expected identifier, found: {}", ident)))
            }
            Expr::Grouping { expr } => self.interp_expr(expr),
            Expr::FnCall { ident, args } => {
                let ident_str = self.interp_lval(ident)?;
                let func_obj = self.curr_scope.borrow().get(ident_str)?;
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
                                Some(arg) => self.curr_scope.borrow_mut().assign(param.to_string(), &arg)?,
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
                    _ => Err(RuntimeException::RuntimeError("Expected a callable".to_string()))
                }
            }
            Expr::Exec { .. } => {
                let exec = self.gen_exec(expr)?;
                Ok(RuntimeObject::String(
                    exec
                        .stdout(subprocess::Redirection::Pipe)
                        .capture()?
                        .stdout_str()))
            }
            Expr::Pipeline { commands } => {
                let pipeline = self.gen_pipeline(commands)?;
                Ok(RuntimeObject::String(pipeline.capture()?.stdout_str()))
            }
        }
    }

    fn interp_concat(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        let mut left_str = match *left {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string(),
            _ => return Err(RuntimeException::RuntimeError(format!("Operand to {} not stringifiable", operator)))
        };
        let right_str = match *right {
            RuntimeObject::String(ref s) => s.clone(),
            RuntimeObject::Integer(i) => i.to_string(),
            RuntimeObject::Float(f) => f.to_string(),
            RuntimeObject::Bool(b) => b.to_string(),
            _ => return Err(RuntimeException::RuntimeError(format!("Operand to {} not stringifiable", operator)))
        };
        left_str.push_str(&right_str);
        Ok(RuntimeObject::String(left_str))
    }

    fn interp_add(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int + right_int))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) + right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(left_float + f64::from(right_int)))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float + right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_sub(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int - right_int))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) - right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(left_float - f64::from(right_int)))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float - right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_mul(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer(left_int * right_int))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) * right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(left_float * f64::from(right_int)))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float * right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_fdiv(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) / f64::from(right_int)))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(f64::from(left_int) / right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Float(left_float / f64::from(right_int)))
                    }
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Float(left_float / right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric", operator)))
        }
    }

    fn interp_mod(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Integer((left_int % right_int + right_int) % right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not integer", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not integer", operator)))
        }
    }

    fn interp_exp(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(base) => {
                match *right {
                    RuntimeObject::Integer(pow) => {
                        if pow >= 0 {
                            Ok(RuntimeObject::Integer(base.pow(pow as u32)))
                        } else {
                            Ok(RuntimeObject::Float(f64::from(base).powf(f64::from(pow))))
                        }
                    }
                    RuntimeObject::Float(pow) => {
                        Ok(RuntimeObject::Float(f64::from(base).powf(pow)))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            RuntimeObject::Float(base) => {
                match *right {
                    RuntimeObject::Integer(pow) => {
                        Ok(RuntimeObject::Float(base.powi(pow)))
                    }
                    RuntimeObject::Float(pow) => {
                        Ok(RuntimeObject::Float(base.powf(pow)))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} not numeric type", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_ge(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int >= right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected int)", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float >= right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected float)", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_gt(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int > right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected int)", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float > right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected float)", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_le(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int <= right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatche type (expected int)", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float <= right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected float)", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_lt(&self, operator: &Token, left: &RuntimeObject, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *left {
            RuntimeObject::Integer(left_int) => {
                match *right {
                    RuntimeObject::Integer(right_int) => {
                        Ok(RuntimeObject::Bool(left_int < right_int))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected int)", operator)))
                }
            }
            RuntimeObject::Float(left_float) => {
                match *right {
                    RuntimeObject::Float(right_float) => {
                        Ok(RuntimeObject::Bool(left_float < right_float))
                    }
                    _ => Err(RuntimeException::RuntimeError(format!("Right operand to {} had mismatched type (expected float)", operator)))
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Left operand to {} not numeric type", operator)))
        }
    }

    fn interp_stringify(&self, operator: &Token, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::String(ref s) => Ok(RuntimeObject::String(s.clone())),
            RuntimeObject::Integer(i) => Ok(RuntimeObject::String(i.to_string())),
            RuntimeObject::Float(f) => Ok(RuntimeObject::String(f.to_string())),
            RuntimeObject::Bool(b) => Ok(RuntimeObject::String(b.to_string())),
            _ => Err(RuntimeException::RuntimeError(format!("Operand to {} not stringifiable", operator)))
        }
    }

    fn interp_sqrt(&self, operator: &Token, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::Integer(i) => Ok(RuntimeObject::Float(f64::from(i).sqrt())),
            RuntimeObject::Float(f) => Ok(RuntimeObject::Float(f.sqrt())),
            _ => Err(RuntimeException::RuntimeError(format!("Operand to {} not numeric type", operator)))
        }
    }

    fn interp_env(&self, operator: &Token, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::String(ref s) => {
                match env::var(s) {
                    Err(_) => Ok(RuntimeObject::Nil),
                    Ok(val) => Ok(RuntimeObject::String(val))
                }
            }
            RuntimeObject::Integer(i) => {
                if i < 0 {
                    return Err(RuntimeException::RuntimeError(
                        format!("Operand to {} cannot be negative integer", operator)));
                }
                match env::args().nth(i as usize) {
                    Some(arg) => Ok(RuntimeObject::String(arg)),
                    None => Ok(RuntimeObject::Nil)
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Operand to {} invalid type", operator)))
        }
    }

    fn interp_numberify(&self, operator: &Token, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::Integer(i) => Ok(RuntimeObject::Integer(i)),
            RuntimeObject::Float(f) => Ok(RuntimeObject::Float(f)),
            RuntimeObject::String(ref s) => {
                match s.parse::<i32>() {
                    Ok(i) => Ok(RuntimeObject::Integer(i)),
                    Err(_e) => match s.parse::<f64>() {
                        Ok(f) => Ok(RuntimeObject::Float(f)),
                        Err(_e) => Err(RuntimeException::RuntimeError(format!("Operand to {} not convertible to numeric type", operator)))
                    }
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Operand to {} not convertible to numeric type", operator)))
        }
    }

    fn interp_negate(&self, operator: &Token, right: &RuntimeObject) -> Result<RuntimeObject, RuntimeException> {
        match *right {
            RuntimeObject::Integer(i) => Ok(RuntimeObject::Integer(-i)),
            RuntimeObject::Float(f) => Ok(RuntimeObject::Float(-f)),
            RuntimeObject::String(ref s) => {
                match s.parse::<i32>() {
                    Ok(i) => Ok(RuntimeObject::Integer(-i)),
                    Err(_e) => match s.parse::<f64>() {
                        Ok(f) => Ok(RuntimeObject::Float(-f)),
                        Err(_e) => Err(RuntimeException::RuntimeError(format!("Operand to {} not convertible to numeric type", operator)))
                    }
                }
            }
            _ => Err(RuntimeException::RuntimeError(format!("Operand to {} not convertible to numeric type", operator)))
        }
    }

    fn gen_exec(&mut self, command: &Expr) -> Result<subprocess::Exec, RuntimeException> {
        match *command {
            Expr::Exec { ref command, ref args } => {
                match *args {
                    None => Ok(subprocess::Exec::cmd(command)),
                    Some(ref args) => {
                        let mut out_args = Vec::new();
                        for arg in args {
                            out_args.push(self.interp_expr(&arg)?);
                        }
                        let mut out_args_string = Vec::new();
                        for arg in out_args {
                            out_args_string.push(
                                match arg {
                                    RuntimeObject::String(ref s) => s.clone(),
                                    RuntimeObject::Integer(i) => i.to_string(),
                                    RuntimeObject::Float(f) => f.to_string(),
                                    RuntimeObject::Bool(b) => b.to_string(),
                                    _ => return Err(RuntimeException::RuntimeError(format!("Parameter to exec command call {} not stringifiable",
                                                                                           command.clone())))
                                }
                            )
                        }
                        if command.eq(&"cd".to_string()) {
                            // TODO better error handling
                            env::set_current_dir(out_args_string.first().unwrap())?;
                        }
                        Ok(subprocess::Exec::cmd(command).args(&out_args_string))
                    }
                }
            }
            _ => Err(RuntimeException::RuntimeError("Expected exec command".to_string()))
        }
    }

    fn gen_pipeline(&mut self, commands: &[Box<Expr>]) -> Result<subprocess::Pipeline, RuntimeException> {
        let mut prepared_commands = Vec::new();
        for command in commands {
            prepared_commands.push(self.gen_exec(&command)?);
        }
        let mut iter = prepared_commands.iter();
        let first = iter.next().unwrap().clone();
        let second = iter.next().unwrap().clone();
        let mut pipeline = first | second;
        while let Some(exec) = iter.next() {
            pipeline = pipeline | exec.clone();
        }
        Ok(pipeline)
    }
}

fn is_truthy(obj: &RuntimeObject) -> bool {
    match *obj {
        RuntimeObject::Bool(bool) if !bool => false,
        RuntimeObject::Nil => false,
        _ => true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Parser;
    use lexer::Lexer;

    #[test]
    fn test_interp_if_stmt() {
        let mut parser = Parser::new(Lexer::new("if 2 > 1 {\n    \"Good\"\n} else {\n    \"Bad\"\n}\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Good\n");
    }

    #[test]
    fn test_interp_else_stmt() {
        let mut parser = Parser::new(Lexer::new("if 1 > 2 {\n    \"Good\"\n} else {\n    \"Bad\"\n}\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Bad\n");
    }

    #[test]
    fn test_interp_elif_stmt() {
        let mut parser = Parser::new(Lexer::new("if 1 > 2 {\n    \"Good\"\n} elif 1 == 1 {\n    \"Okay\"\n}\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Okay\n");
    }

    #[test]
    fn test_interp_while_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = 0\nwhile a < 3 {\na = a + 1\n}\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 0");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 1\na = 2\na = 3\n");
    }

    #[test]
    fn test_interp_next_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = 0\nwhile a < 3 {\n a = a + 1\nif a == 1 { next }\na\n}\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 0");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 2\n2\na = 3\n3\n");
    }

    #[test]
    fn test_interp_next_stmt_no_loop() {
        let mut parser = Parser::new(Lexer::new("next\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Runtime error: next with no enclosing loop");
    }

    #[test]
    fn test_interp_break_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = 0\nwhile a < 3 {\n a = a + 1\nif a == 2 { break }\n}\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 0");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 1\n");
    }

    #[test]
    fn test_interp_break_stmt_no_loop() {
        let mut parser = Parser::new(Lexer::new("break\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Runtime error: break with no enclosing loop");
    }

    #[test]
    fn test_interp_let_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = \"hi\"\na\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = hi");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "hi");
    }

    #[test]
    fn test_interp_assign_stmt() {
        let mut parser = Parser::new(Lexer::new("let a = \"hi\"\na = 1\na\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = hi");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "a = 1");
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "1");
    }

    #[test]
    fn test_interp_group_expr() {
        let mut parser = Parser::new(Lexer::new("(1 + 2)\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "3");
    }

    #[test]
    fn test_interp_func() {
        let mut parser = Parser::new(Lexer::new("def f(a) {\nreturn a\n}\nf(1)\n"));
        let mut interpreter = Interpreter::default();
        let out = interpreter.interp(&parser.next().unwrap());
        assert_eq!(out, None);
        let out = interpreter.interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "1");
    }

    #[test]
    fn test_interp_return_stmt_no_fn() {
        let mut parser = Parser::new(Lexer::new("return 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Runtime error: return with no enclosing function");
    }

    #[test]
    fn test_interp_and_expr() {
        let mut parser = Parser::new(Lexer::new("True and False\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "False");
    }

    #[test]
    fn test_interp_or_expr() {
        let mut parser = Parser::new(Lexer::new("True or False\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "True");
    }

    #[test]
    fn test_interp_concat_expr() {
        let mut parser = Parser::new(Lexer::new("1 & 2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "12");
    }

    #[test]
    fn test_interp_add_expr() {
        let mut parser = Parser::new(Lexer::new("1 + 2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "3");
    }

    #[test]
    fn test_interp_sub_expr() {
        let mut parser = Parser::new(Lexer::new("2.0 - 1.5\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "0.5");
    }

    #[test]
    fn test_interp_mul_expr() {
        let mut parser = Parser::new(Lexer::new("2 * 3\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "6");
    }

    #[test]
    fn test_interp_fdiv_expr() {
        let mut parser = Parser::new(Lexer::new("1 / 2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "0.5");
    }

    #[test]
    fn test_interp_mod_expr() {
        let mut parser = Parser::new(Lexer::new("21 % 4\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "1");
    }

    #[test]
    fn test_interp_mod_neg_expr() {
        let mut parser = Parser::new(Lexer::new("-21 % 4\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "3");
    }

    #[test]
    fn test_interp_exp_expr() {
        let mut parser = Parser::new(Lexer::new("2 ^ 2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "4");
    }

    #[test]
    fn test_interp_exp_neg_pow_expr() {
        let mut parser = Parser::new(Lexer::new("2 ^ -2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "0.25");
    }

    #[test]
    fn test_interp_exp_float_base_expr() {
        let mut parser = Parser::new(Lexer::new("2.0 ^ 2\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "4");
    }

    #[test]
    fn test_interp_exp_float_base_float_pow_expr() {
        let mut parser = Parser::new(Lexer::new("4.0 ^ 0.5\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "2");
    }

    #[test]
    fn test_interp_eq_expr() {
        let mut parser = Parser::new(Lexer::new("1 == 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "True");
    }

    #[test]
    fn test_interp_neq_expr() {
        let mut parser = Parser::new(Lexer::new("1 != 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "False");
    }

    #[test]
    fn test_interp_ge_expr() {
        let mut parser = Parser::new(Lexer::new("1 >= 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "True");
    }

    #[test]
    fn test_interp_gt_expr() {
        let mut parser = Parser::new(Lexer::new("1 > 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "False");
    }

    #[test]
    fn test_interp_le_expr() {
        let mut parser = Parser::new(Lexer::new("1 <= 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "True");
    }

    #[test]
    fn test_interp_lt_expr() {
        let mut parser = Parser::new(Lexer::new("1 < 1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "False");
    }

    #[test]
    fn test_interp_stringify_expr() {
        let mut parser = Parser::new(Lexer::new("1 + &1\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "Runtime error: Right operand to + (line 0) not numeric type");
    }

    #[test]
    fn test_interp_numberify_expr() {
        let mut parser = Parser::new(Lexer::new("1 + +\"1\"\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "2");
    }

    #[test]
    fn test_interp_negate_expr() {
        let mut parser = Parser::new(Lexer::new("-\"1.0\"\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "-1");
    }

    #[test]
    fn test_interp_not_expr() {
        let mut parser = Parser::new(Lexer::new("!True\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "False");
    }

    #[test]
    fn test_interp_sqrt_int_expr() {
        let mut parser = Parser::new(Lexer::new("/4\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "2");
    }

    #[test]
    fn test_interp_sqrt_float_expr() {
        let mut parser = Parser::new(Lexer::new("/5.0\n"));
        let out = Interpreter::default().interp(&parser.next().unwrap()).unwrap();
        assert_eq!(out, "2.23606797749979");
    }
}