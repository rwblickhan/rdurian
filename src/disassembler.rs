extern crate rdurian;
extern crate byteorder;
extern crate clap;

use byteorder::{BigEndian, ReadBytesExt};
use clap::{App, Arg};
use rdurian::bytecode::{ConstantPoolIdx, Opcode, Tag};
use std::collections::VecDeque;
use std::fs;
use std::io::Cursor;

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

    match matches.value_of("input") {
        None => panic!(),
        Some(input) => disassemble(fs::read(input).unwrap()),
    }
}

fn disassemble(input: Vec<u8>) {
    let mut input_queue = VecDeque::from(input);
    let magic_number = input_queue.pop_front().unwrap();
    println!("Magic number: {}", magic_number);
    let constant_pool_size = Cursor::new({
        let mut tmp = Vec::new();
        for _i in 0..2 {
            tmp.push(input_queue.pop_front().unwrap());
        }
        tmp
    }).read_u16::<BigEndian>().unwrap();
    println!("Constant pool size (bytes): {}", constant_pool_size);
    print!("Constant pool:");
    let mut constant_pool = VecDeque::new();
    for _i in 0..constant_pool_size {
        constant_pool.push_back(input_queue.pop_front().unwrap());
    }
    while let Some(byte) = constant_pool.pop_front() {
        let tag = Tag::from(byte);
        match tag {
            Tag::Nil => print!("Nil "),
            Tag::Integer => {
                let int_constant = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..4 {
                        tmp.push(constant_pool.pop_front().unwrap());
                    }
                    tmp
                }).read_i32::<BigEndian>().unwrap();
                print!(" {} ", int_constant);
            }
            _ => {} // TODO
        }
    }
    println!();
    println!("Instructions: ");
    while let Some(byte) = input_queue.pop_front() {
        let opcode = Opcode::from(byte);
        match opcode {
            Opcode::Nop => println!("NOP"),
            Opcode::Pop => println!("POP"),
            Opcode::Constant => {
                let idx = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..2 {
                        tmp.push(input_queue.pop_front().unwrap());
                    }
                    tmp
                }).read_u16::<BigEndian>().unwrap();
                println!("CONST {}", idx);
            }
            Opcode::Add => println!("ADD"),
            Opcode::Sub => println!("SUB"),
            Opcode::Halt => println!("HALT")
        }
    }
}
