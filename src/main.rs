use std::io::stdin;

use parser::Parser;

mod parser;
fn main() {
    let stdin = stdin();
    println!("Write a polynomial expression: ");
    let mut buffer = String::new();
    stdin.read_line(&mut buffer).unwrap();

    let mut parser = Parser::new(&buffer);
    println!("Result {:?}", parser.parse());
}
