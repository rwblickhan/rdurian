extern crate byteorder;

use self::byteorder::{BigEndian, ReadBytesExt};
use bytecode::{Bytecode, ConstantPoolIdx, Opcode, Tag};
use std::collections::VecDeque;
use std::fmt;
use std::fs;
use std::io::{stderr, stdout, Cursor, Error, Write};

#[derive(Clone, PartialEq)]
enum RuntimeObject {
    Integer(i32),
    Float(f64),
    Bool(bool),
    Nil,
}

impl fmt::Display for RuntimeObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeObject::Integer(i) => write!(f, "{}", i),
            RuntimeObject::Float(fl) => write!(f, "{}", fl),
            RuntimeObject::Bool(b) => if *b {
                write!(f, "True")
            } else {
                write!(f, "False")
            },
            RuntimeObject::Nil => write!(f, "nil")
        }
    }
}

#[derive(Debug)]
pub enum StartupError {
    MissingMagicNumber,
    InvalidMagicNumber(u8),
    InvalidConstantPoolSize,
    FileError(Error),
}

impl From<Error> for StartupError {
    fn from(error: Error) -> Self {
        StartupError::FileError(error)
    }
}

pub enum RuntimeError {
    InvalidConstant(u8),
    StackError,
    FileError(Error),
    InvalidOperand(String),
}

impl From<Error> for RuntimeError {
    fn from(error: Error) -> Self {
        RuntimeError::FileError(error)
    }
}

#[derive(Default)]
pub struct VM {
    had_exec_error: bool,
    constant_pool_size: ConstantPoolIdx,
    constant_pool: Vec<Bytecode>,
    instructions: Vec<Bytecode>,
    pc: usize,
    stack: Vec<RuntimeObject>,
}

impl VM {
    pub fn init(input: &str) -> Result<VM, StartupError> {
        let mut input_queue = VecDeque::from(fs::read(input)?);
        let magic_number = input_queue.pop_front();
        match magic_number {
            Some(num) if num == 0x2A => (),
            Some(num) => return Err(StartupError::InvalidMagicNumber(num)),
            _ => return Err(StartupError::MissingMagicNumber),
        };
        let constant_pool_size = Cursor::new({
            let mut tmp = Vec::new();
            for _i in 0..2 {
                let out = input_queue.pop_front();
                match out {
                    Some(byte) => tmp.push(byte),
                    _ => return Err(StartupError::InvalidConstantPoolSize)
                }
            }
            tmp
        }).read_u16::<BigEndian>()?;
        let mut constant_pool = Vec::with_capacity(usize::from(constant_pool_size));
        for _i in 0..constant_pool_size {
            constant_pool.push(input_queue.pop_front().unwrap());
        }
        let instructions = Vec::from(input_queue);
        Ok(VM {
            had_exec_error: false,
            constant_pool_size,
            constant_pool,
            instructions,
            pc: 0,
            stack: Vec::with_capacity(2 ^ 16),
        })
    }

    pub fn run(&mut self) {
        loop {
            match Opcode::from(self.instructions[self.pc]) {
                Opcode::Nop => self.nop(),
                Opcode::Halt => return,
                Opcode::Pop => self.pop(),
                Opcode::Constant => match self.constant() {
                    _ => () // TODO
                }
                Opcode::Add => match self.add() {
                    _ => () // TODO
                }
                Opcode::Sub => match self.sub() {
                    _ => () // TODO
                }
                Opcode::Print => match self.print() {
                    _ => () // TODO
                }
                Opcode::Err => match self.err() {
                    _ => () // TODO
                }
            };
        }
    }

    fn nop(&mut self) {
        self.pc = self.pc + 1;
    }

    fn pop(&mut self) {
        self.pc = self.pc + 1;
        self.stack.pop();
    }

    fn constant(&mut self) -> Result<(), RuntimeError> {
        self.pc = self.pc + 1;
        let idx = Cursor::new({
            let mut tmp = Vec::new();
            for _i in 0..2 {
                tmp.push(self.instructions[self.pc]);
                self.pc = self.pc + 1;
            }
            tmp
        }).read_u16::<BigEndian>()?;
        let tag = Tag::from(self.constant_pool[usize::from(idx)]);
        match tag {
            Tag::Nil => Ok(self.stack.push(RuntimeObject::Nil)),
            Tag::Integer => Ok(self.stack.push(RuntimeObject::Integer(
                Cursor::new({
                    let mut tmp = Vec::with_capacity(4);
                    for i in 1..5 {
                        tmp.push(self.constant_pool[usize::from(idx + i)]);
                    }
                    tmp
                }).read_i32::<BigEndian>()?
            ))),
            _ => { Ok(()) } // TODO
        }
    }

    fn add(&mut self) -> Result<(), RuntimeError> {
        self.pc = self.pc + 1;
        let right = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        let left = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        match left {
            RuntimeObject::Integer(left_int) => {
                match right {
                    RuntimeObject::Integer(right_int) => Ok(self.stack.push(RuntimeObject::Integer(left_int + right_int))),
                    RuntimeObject::Float(right_float) => Ok(self.stack.push(RuntimeObject::Float(f64::from(left_int) + right_float))),
                    _ => Err(RuntimeError::InvalidOperand("Right operand to addition not numeric type".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match right {
                    RuntimeObject::Integer(right_int) => Ok(self.stack.push(RuntimeObject::Float(left_float + f64::from(right_int)))),
                    RuntimeObject::Float(right_float) => Ok(self.stack.push(RuntimeObject::Float(left_float + right_float))),
                    _ => Err(RuntimeError::InvalidOperand("Right operand to addition not numeric type".to_string()))
                }
            }
            _ => Err(RuntimeError::InvalidOperand("Left operand to addition not numeric type".to_string())),
        }
    }

    fn sub(&mut self) -> Result<(), RuntimeError> {
        self.pc = self.pc + 1;
        let right = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        let left = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        match left {
            RuntimeObject::Integer(left_int) => {
                match right {
                    RuntimeObject::Integer(right_int) => Ok(self.stack.push(RuntimeObject::Integer(left_int - right_int))),
                    RuntimeObject::Float(right_float) => Ok(self.stack.push(RuntimeObject::Float(f64::from(left_int) - right_float))),
                    _ => Err(RuntimeError::InvalidOperand("Right operand to addition not numeric type".to_string()))
                }
            }
            RuntimeObject::Float(left_float) => {
                match right {
                    RuntimeObject::Integer(right_int) => Ok(self.stack.push(RuntimeObject::Float(left_float - f64::from(right_int)))),
                    RuntimeObject::Float(right_float) => Ok(self.stack.push(RuntimeObject::Float(left_float - right_float))),
                    _ => Err(RuntimeError::InvalidOperand("Right operand to addition not numeric type".to_string()))
                }
            }
            _ => Err(RuntimeError::InvalidOperand("Left operand to addition not numeric type".to_string())),
        }
    }

    fn print(&mut self) -> Result<(), RuntimeError> {
        self.pc = self.pc + 1;
        let obj = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        let stdout = stdout();
        let mut handle = stdout.lock();
        handle.write_all(format!("{}\n", obj).as_bytes())?;
        Ok(())
    }

    fn err(&mut self) -> Result<(), RuntimeError> {
        self.pc = self.pc + 1;
        let obj = match self.stack.pop() {
            Some(val) => val,
            None => return Err(RuntimeError::StackError),
        };
        let stderr = stderr();
        let mut handle = stderr.lock();
        handle.write_all(format!("{}\n", obj).as_bytes())?;
        Ok(())
    }

    pub fn had_error(&self) -> bool {
        self.had_exec_error
    }
}