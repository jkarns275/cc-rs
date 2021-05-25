#[macro_use] extern crate lalrpop_util;

mod parse_tree;
mod lexer;
mod interner;


use crate::interner::*;
use crate::lexer::*;
use crate::parse_tree::*;

lalrpop_mod!(pub gram);
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = match line {
            Ok(x) => x,
            _ => break,
        };
        let lexer = Lexer::new(&input[..]);
        let mut str_interner = Interner::<String>::new();
        let mut byte_interner = Interner::<Box<[u8]>>::new();
        let parsed = gram::translation_unit_programParser::new().parse(&input[..], &mut str_interner, &mut byte_interner, lexer);
        match parsed {
            Ok(x) => {
                let mut s = String::new();
                for a in x.iter() {
                    a.pretty_print(&mut s, &str_interner);
                }
                println!("{}", s);
            },
            Err(err) => println!("{:?}", err),
        };
    }
}
