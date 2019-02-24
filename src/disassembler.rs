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

fn disassemble<W: Write>(input: Vec<u8>, write_buf: &mut W) {
    let mut input_queue = VecDeque::from(input);
    let magic_number = input_queue.pop_front().unwrap();
    writeln!(write_buf, "Magic number: {}", magic_number).unwrap();
    let constant_pool_size = Cursor::new({
        let mut tmp = Vec::new();
        for _i in 0..2 {
            tmp.push(input_queue.pop_front().unwrap());
        }
        tmp
    }).read_u16::<BigEndian>().unwrap();
    writeln!(write_buf, "Constant pool size (bytes): {}", constant_pool_size).unwrap();
    write!(write_buf, "Constant pool:").unwrap();
    let mut constant_pool = VecDeque::new();
    for _i in 0..constant_pool_size {
        constant_pool.push_back(input_queue.pop_front().unwrap());
    }
    while let Some(byte) = constant_pool.pop_front() {
        let tag = Tag::from(byte);
        match tag {
            Tag::Nil => write!(write_buf, " Nil ").unwrap(),
            Tag::Integer => {
                let int_constant = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..4 {
                        tmp.push(constant_pool.pop_front().unwrap());
                    }
                    tmp
                }).read_i32::<BigEndian>().unwrap();
                write!(write_buf, " {} ", int_constant).unwrap();
            }
            _ => {} // TODO
        }
    }
    writeln!(write_buf).unwrap();
    writeln!(write_buf, "Instructions:").unwrap();
    while let Some(byte) = input_queue.pop_front() {
        let opcode = Opcode::from(byte);
        match opcode {
            Opcode::Nop => writeln!(write_buf, "NOP").unwrap(),
            Opcode::Pop => writeln!(write_buf, "POP").unwrap(),
            Opcode::Constant => {
                let idx = Cursor::new({
                    let mut tmp = Vec::new();
                    for _i in 0..2 {
                        tmp.push(input_queue.pop_front().unwrap());
                    }
                    tmp
                }).read_u16::<BigEndian>().unwrap();
                writeln!(write_buf, "CONST {}", idx).unwrap();
            }
            Opcode::Add => writeln!(write_buf, "ADD").unwrap(),
            Opcode::Sub => writeln!(write_buf, "SUB").unwrap(),
            Opcode::Print => writeln!(write_buf, "PRINT").unwrap(),
            Opcode::Err => writeln!(write_buf, "ERR").unwrap(),
            Opcode::Halt => writeln!(write_buf, "HALT").unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use disassemble;
    use std::io::Write;
    use rdurian::bytecode::Opcode;

    #[test]
    fn test_disasm_nop() {
        let mut mock_stdout = Vec::new();
        let mut input = Vec::new();
        let mut expected_output = Vec::new();
        // Magic number
        input.push(0x2A);
        writeln!(&mut expected_output, "Magic number: 42");
        // Constant pool size
        input.push(0x00);
        input.push(0x00);
        writeln!(&mut expected_output, "Constant pool size (bytes): 0");
        writeln!(&mut expected_output, "Constant pool:");
        // NOP
        input.push(Opcode::Nop as u8);
        writeln!(&mut expected_output, "Instructions:");
        writeln!(&mut expected_output, "NOP");
        disassemble(input, &mut mock_stdout);
        assert_eq!(String::from_utf8(mock_stdout).unwrap(),
                   String::from_utf8(expected_output).unwrap());
    }
}
