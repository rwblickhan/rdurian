extern crate rdurian;
extern crate clap;

use clap::{App, Arg};
use rdurian::ast::Stmt;
use rdurian::code_generator::CodeGenerator;
use rdurian::lexer::Lexer;
use rdurian::parser::Parser;
use rdurian::pretty_printer::*;
use rdurian::treewalk_interpreter::Interpreter;
use rdurian::vm::VM;
use std::io::{stdin, stdout, stderr, Write};
use std::fs;
use std::fs::File;

fn main() {
    let matches = App::new("rdurian")
        .version("0.1")
        .author("R.W. Blickhan <rwblickhan@gmail.com>")
        .about("Rust implementation of Durian compiler")
        .arg(Arg::with_name("verbose")
            .help("Print verbose logs")
            .short("v")
            .long("verbose"))
        .arg(Arg::with_name("pp")
            .help("Pretty print Durian source while parsing")
            .short("p")
            .long("prettyprint"))
        .arg(Arg::with_name("input")
            .help("Sets the Durian source file to read.")
            .required(false)
            .index(1))
        .get_matches();

    let pretty_print = matches.is_present("pp");
    let verbose = matches.is_present("verbose");
    if pretty_print && verbose {
        println!("Pretty printing enabled");
    } else if verbose {
        println!("Pretty printing disabled")
    }

    match matches.value_of("input") {
        None => exec_repl(pretty_print),
        Some(input) => exec_input(verbose, pretty_print, input)
    }
}

fn exec_repl(pretty_print: bool) {
    println!("Starting REPL...");
    println!("Welcome to the rdurian REPL! Type 'fin' when you're finished.");
    print!("> ");
    stdout().flush().unwrap();
    let mut interpreter = Interpreter::default();
    loop {
        let mut buffer = String::new();
        stdin().read_line(&mut buffer).unwrap();
        if buffer.eq(&"fin\n".to_string()) {
            println!("Goodbye!");
            break;
        }
        if buffer.ends_with("{\n") {
            while !buffer.ends_with("}\n") {
                stdin().read_line(&mut buffer).unwrap();
            }
        }
        let mut parser = Parser::new(Lexer::new(&buffer));
        while let Some(stmt) = parser.next() {
            if pretty_print {
                println!("{}", pretty_print_stmt(&stmt));
            }
            match interpreter.interp(&stmt) {
                None => (),
                Some(msg) => println!("{}", msg)
            };
        }
        print!("> ");
        stdout().flush().unwrap();
    }
    std::process::exit(0);
}

fn exec_input(verbose: bool, pretty_print: bool, input: &str) {
    if verbose {
        println!("Executing input from file {}", input);
    }
    let source = fs::read_to_string(input)
        .unwrap_or_else(|_| panic!("Unable to read input file {}", input));
    let mut parser = Parser::new(Lexer::new(&source));
    let mut stmts: Vec<Stmt> = Vec::new();
    while let Some(stmt) = parser.next() {
        if parser.had_error() {
            continue;
        }
        if pretty_print {
            println!("{}", pretty_print_stmt(&stmt));
        }
        stmts.push(stmt);
    }
    if parser.had_error() {
        std::process::exit(1);
    }
    let mut code_gen = CodeGenerator::default();
    let mut iter = stmts.iter();
    while let Some(stmt) = iter.next() {
        code_gen.compile(stmt);
    }
    let constants = code_gen.retrieve_constant_pool();
    let out = code_gen.retrieve_bytecode();
    // TODO determine the actual filename
    let mut bytecode_file = File::create("tmp.durb").unwrap();
    bytecode_file.write(&[0x2A]).unwrap();
    bytecode_file.write(code_gen.retrieve_constant_pool_size().as_slice()).unwrap();
    bytecode_file.write_all(constants.as_slice()).unwrap();
    bytecode_file.write_all(out.as_slice()).unwrap();
    // TODO determine the actual filename
    let stdout = stdout();
    let stderr = stderr();
    let mut stdout_locked = stdout.lock();
    let mut stderr_locked = stderr.lock();
    let bytecode = fs::read("tmp.durb").unwrap();
    let mut vm = match VM::init(bytecode,
                                &mut stdout_locked,
                                &mut stderr_locked) {
        Ok(vm) => vm,
        Err(e) => {
            println!("{:?}", e);
            std::process::exit(1)
        } // TODO
    };
    vm.run();
    if vm.had_error() {
        std::process::exit(1);
    }
    std::process::exit(0);
}
