#[macro_use] extern crate lalrpop_util;

pub mod parse_tree;
pub mod interner;
pub mod lexer;
pub mod util;
pub mod ast;

use crate::parse_tree::*;
use crate::interner::*;
use crate::lexer::*;
use crate::util::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::io::{self, BufRead};

lalrpop_mod!(pub gram);

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = match line {
            Ok(x) => x,
            _ => break,
        };
        let mut tde = Rc::new(RefCell::new(TypeDefEnv::new()));
        let lexer = Lexer::new(&input[..], tde.clone());
        let mut str_interner = Interner::<String>::new();
        let mut byte_interner = Interner::<Box<[u8]>>::new();
        let mut parsed = gram::translation_unit_programParser::new().parse(&input[..], &mut str_interner, &mut byte_interner, &mut tde, lexer);
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
