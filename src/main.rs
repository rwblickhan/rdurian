extern crate rdurian;
extern crate clap;

use clap::{App, Arg};
use rdurian::lexer::Lexer;
use rdurian::parser::Parser;
use rdurian::pretty_printer::*;
use std::io::{stdin, stdout, Write};

fn main() {
    let matches = App::new("rdurian")
        .version("0.1")
        .author("R.W. Blickhan <rwblickhan@gmail.com>")
        .about("Rust implementation of Durian compiler")
        .arg(Arg::with_name("pp")
            .help("Pretty print Durian source after parsing")
            .short("p")
            .long("prettyprint"))
        .arg(Arg::with_name("input")
            .help("Sets the Durian source file to read.")
            .required(false)
            .index(1))
        .get_matches();

    let pretty_print = matches.is_present("pp");
    if pretty_print {
        println!("Pretty printing enabled");
    } else {
        println!("Pretty printing disabled")
    }

    match matches.value_of("input") {
        None => exec_repl(pretty_print),
        Some(input) => exec_input(pretty_print, input.to_string())
    }
}

fn exec_repl(pretty_print: bool) {
    println!("Starting REPL...");
    println!("Welcome to the rdurian REPL! Type 'fin' when you're finished.");
    print!("> ");
    stdout().flush().unwrap();
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
        }
        print!("> ");
        stdout().flush().unwrap();
    }
    std::process::exit(0);
}

fn exec_input(pretty_print: bool, input: String) {
    println!("Executing input from file {}", input);
    println!("Not yet implemented!");
    std::process::exit(1);
}
