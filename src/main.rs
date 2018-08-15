extern crate rdurian;
extern crate clap;

use clap::{App, Arg};
use rdurian::lexer::Lexer;
use rdurian::parser::Parser;
use rdurian::pretty_printer::*;
use rdurian::treewalk_interpreter::Interpreter;
use std::io::{stdin, stdout, Write};
use std::fs;

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
        if buffer.eq(&"fin\n") {
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
    let mut interpreter = Interpreter::default();
    let mut exit_code = 0;
    while let Some(stmt) = parser.next() {
        if parser.had_error() {
            exit_code = 1;
            continue;
        }
        if pretty_print {
            println!("{}", pretty_print_stmt(&stmt));
        }
        let out = interpreter.interp(&stmt);
        if interpreter.had_error() {
            exit_code = 1;
            if verbose {
                println!("{}", out.unwrap_or_else(|| "Unknown runtime error".to_string()));
            }
            break;
        }
    }
    std::process::exit(exit_code);
}
