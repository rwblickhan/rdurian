extern crate rdurian;
extern crate byteorder;
extern crate clap;

use byteorder::{BigEndian, ReadBytesExt};
use clap::{App, Arg};
use rdurian::bytecode::{Opcode, Tag};
use std::collections::VecDeque;
use std::fs;
use std::io::{Cursor, stdout, Write};

fn main() {
    let matches = App::new("rdurian disassembler")
        .version("0.1")
        .author("R.W. Blickhan <rwblickhan@gmail.com>")
        .about("Disassembler for rdurian bytecode")
        .arg(Arg::with_name("input")
            .help("Sets the Durian bytecode file to read.")
            .required(true)
            .index(1))
        .get_matches();

    let stdout = stdout();
    match matches.value_of("input") {
        None => panic!(),
        Some(input) => disassemble(fs::read(input).unwrap(),
                                   &mut stdout.lock()),
    }
}

fn disassemble<W: Write>(input: Vec<u8>, mut write_buf: W) {
    let mut input_queue = VecDeque::from(input);
    let magic_number = input_queue.pop_front().unwrap();
    writeln!(&mut write_buf, "Magic number: {}", magic_number).unwrap();
    let constant_pool_size = Cursor::new({
        let mut tmp = Vec::new();
        for _i in 0..2 {
            tmp.push(input_queue.pop_front().unwrap());
        }
        tmp
    }).read_u16::<BigEndian>().unwrap();
    writeln!(&mut write_buf, "Constant pool size (bytes): {}", constant_pool_size).unwrap();
    write!(&mut write_buf, "Constant pool:").unwrap();
    let mut constant_pool = VecDeque::new();
    for _i in 0..constant_pool_size {
        constant_pool.push_back(input_queue.pop_front().unwrap());
    }
    while let Some(byte) = constant_pool.pop_front() {
        let tag = Tag::from(byte);
        match tag {
            Tag::Nil => write!(&mut write_buf, "Nil ").unwrap(),
            Tag::Integer => {
                let int_constant = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..4 {
                        tmp.push(constant_pool.pop_front().unwrap());
                    }
                    tmp
                }).read_i32::<BigEndian>().unwrap();
                write!(&mut write_buf, " {} ", int_constant).unwrap();
            }
            _ => {} // TODO
        }
    }
    writeln!(&mut write_buf).unwrap();
    writeln!(&mut write_buf, "Instructions: ").unwrap();
    while let Some(byte) = input_queue.pop_front() {
        let opcode = Opcode::from(byte);
        match opcode {
            Opcode::Nop => writeln!(&mut write_buf, "NOP").unwrap(),
            Opcode::Pop => writeln!(&mut write_buf, "POP").unwrap(),
            Opcode::Constant => {
                let idx = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..2 {
                        tmp.push(input_queue.pop_front().unwrap());
                    }
                    tmp
                }).read_u16::<BigEndian>().unwrap();
                writeln!(&mut write_buf, "CONST {}", idx).unwrap();
            }
            Opcode::Add => writeln!(&mut write_buf, "ADD").unwrap(),
            Opcode::Sub => writeln!(&mut write_buf, "SUB").unwrap(),
            Opcode::Print => writeln!(&mut write_buf, "PRINT").unwrap(),
            Opcode::Err => writeln!(&mut write_buf, "ERR").unwrap(),
            Opcode::Halt => writeln!(&mut write_buf, "HALT").unwrap()
        }
    }
}
