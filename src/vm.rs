extern crate byteorder;

use self::byteorder::{BigEndian, ReadBytesExt};
use bytecode::ConstantPoolIdx;
use std::collections::VecDeque;
use std::fs;
use std::io::{Cursor, Error};

#[derive(Debug)]
pub enum StartupError {
    MissingMagicNumber,
    InvalidMagicNumber(u8),
    InvalidConstantPoolSize,
    InvalidConstant(u8),
    FileError(Error),
}

impl From<Error> for StartupError {
    fn from(error: Error) -> Self {
        StartupError::FileError(error)
    }
}

#[derive(Default)]
pub struct VM {
    had_exec_error: bool,
    constant_pool_size: ConstantPoolIdx,
    constant_pool: Vec<u8>,
    instructions: Vec<u8>,
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
        let mut instructions = Vec::from(input_queue);
        Ok(VM { had_exec_error: false, constant_pool_size, constant_pool, instructions })
    }

    pub fn run(&mut self) {}

    pub fn had_error(&self) -> bool {
        self.had_exec_error
    }
}