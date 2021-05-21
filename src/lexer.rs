use std::str::{CharIndices, FromStr, self};
use std::borrow::Borrow;
use crate::lexer::LexErrorKind::{UnexpectedEof, InvalidCharLiteral};

#[derive(Debug, Clone)]
pub enum IntType { Unsigned, Long, }
#[derive(Debug, Clone)]
pub enum FloatType { Float, Double, }

#[derive(Debug, Clone)]
pub enum Tok<'t> {
    KWAuto,
    KWBreak,
    KWCase,
    KWChar,
    KWConst,
    KWContinue,
    KWDefault,
    KWDo,
    KWDouble,
    KWElse,
    KWEnum,
    KWExtern,
    KWFloat,
    KWFor,
    KWGoto,
    KWIf,
    KWInt,
    KWLong,
    KWRegister,
    KWReturn,
    KWShort,
    KWSigned,
    KWSizeof,
    KWStatic,
    KWStruct,
    KWSwitch,
    KWTypedef,
    KWUnion,
    KWUnsigned,
    KWVoid,
    KWVolatile,
    KWWhile,

    Int((i128, IntType)), Float((f64, FloatType)), String(Box<[u8]>), Char(u8),
    Id(&'t str),

    Ellipsis,
    RightAssign, LeftAssign,
    AddAssign, SubAssign,
    MulAssign, DivAssign, ModAssign,
    AndAssign, XorAssign, OrAssign,
    RightSh, LeftSh,
    Inc, Dec,
    Arrow,
    LAnd, LOr,
    Lt, Lte, Gt, Gte, Eq, Neq,
    Semicolon,
    BraceOpen, BraceClose,
    BrackOpen, BrackClose,
    ParOpen, ParClose,
    Comma,
    Colon,
    Assign,
    Dot,
    LNot, BNot,
    Sub, Add,
    Mul, Div, Mod,
    Xor, BOr, BAnd,
    Qmark,

    EOF,
}

type SpannedToken<'input> = (usize, Tok<'input>, usize);

#[derive(Debug)]
pub enum LexErrorKind {
    BadString,
    UntermnatedString,
    BadStringEscape,
    UnexpectedEof,
    BadFloat,
    BadHexLiteral,
    BadOctalLiteral,
    ExpectedEllipsis,
    InvalidCharLiteral,
    UnexpectedChar(char),
    ICE,
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub start: usize,
    pub end: usize,
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    input: &'input str,
    peek: Option<(usize, char)>,
    eof_emitted: bool,
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_octal_digit(ch: char) -> bool {
    match ch {
        '0' ..= '7' => true,
        _ => false,
    }
}

fn is_ident_start(ch: char) -> bool {
    // TODO: Unicode?
    match ch {
        '_' | 'a'..= 'z' | 'A'..= 'Z' => true,
        _ => false,
    }
}

fn is_ident_continue(ch: char) -> bool {
    // TODO: Unicode?
    match ch {
        '0' ..= '9' | '\'' => true,
        ch => is_ident_start(ch),
    }
}

fn is_operator_char(ch: char) -> bool {
    match ch {
        '.' | '>' | '<' | '=' | '!' | '%' | '^' | '&' | '*' | '-' | '=' | '+' | ':' | ';' |
        '/' | '|' | '~' | '?' => true,
        _ => false,
    }
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices();
        let peek = chars.next();
        Lexer {
            chars,
            peek,
            input,
            eof_emitted: false,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        match self.peek {
            Some((loc, c)) => {
                self.peek = self.chars.next();
                Some((loc, c))
            },
            None => None
        }
    }

    fn next_char_is(&self, ch: char) -> bool {
        match self.peek {
            Some((_, c)) => c == ch,
            _ => false,
        }
    }

    fn test_lookahead<F>(&self, mut predicate: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        match self.peek {
            Some((_, ch)) => predicate(ch),
            _ => false,
        }
    }

    fn take_while_<F>(&mut self, start: usize, mut predicate: F) -> (usize, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.peek {
            if predicate(ch) {
                self.bump();
            } else {
                return (end, self.slice(start, end))
            }
        }
        let end = self.get_current_index();
        (end, self.slice(start, end))
    }

    fn take_until<F>(&mut self, start: usize, mut predicate: F) -> (usize, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_while_(start, |x| !predicate(x))
    }

    fn slice(&mut self, start: usize, end: usize) -> &'input str {
        &self.input[start..end]
    }

    fn error<T>(&self, start: usize, kind: LexErrorKind) -> Result<T, LexError> {
        Err(LexError { kind, start, end: self.get_current_index() + 1 })
    }

    fn block_comment(&mut self, start: usize) -> Result<(), LexError> {
        self.bump(); // Skip first '*'

        loop {
            let _ = self.take_until(start, |ch| ch == '*');
            self.bump(); // Skip next '*'
            match self.peek {
                Some((_, '/')) => {
                    self.bump();
                    return Ok(());
                }
                Some((_, _)) => continue,
                None => return self.error(start, LexErrorKind::UnexpectedEof),
            }
        }
    }

    fn identifier(&mut self, start: usize) -> SpannedToken<'input> {
        let (end, ident) = self.take_while_(start, is_ident_continue);
        let token = match ident {
            "auto" => Tok::KWAuto,
            "break" => Tok::KWBreak,
            "case" => Tok::KWCase,
            "const" => Tok::KWConst,
            "char" => Tok::KWChar,
            "continue" => Tok::KWContinue,
            "default" => Tok::KWDefault,
            "do" => Tok::KWDo,
            "double" => Tok::KWDouble,
            "else" => Tok::KWElse,
            "enum" => Tok::KWEnum,
            "extern" => Tok::KWExtern,
            "float" => Tok::KWFloat,
            "for" => Tok::KWFor,
            "goto" => Tok::KWGoto,
            "if" => Tok::KWIf,
            "int" => Tok::KWInt,
            "long" => Tok::KWLong,
            "register" => Tok::KWRegister,
            "return" => Tok::KWReturn,
            "short" => Tok::KWShort,
            "signed" => Tok::KWSigned,
            "sizeof" => Tok::KWSizeof,
            "static" => Tok::KWStatic,
            "struct" => Tok::KWStruct,
            "switch" => Tok::KWSwitch,
            "typedef" => Tok::KWTypedef,
            "union" => Tok::KWUnion,
            "unsigned" => Tok::KWUnsigned,
            "void" => Tok::KWVoid,
            "volatile" => Tok::KWVolatile,
            "while" => Tok::KWWhile,
            src => Tok::Id(src),
        };

        (start, token, end)
    }

    fn operator(&mut self, start: usize, first: char) -> Result<SpannedToken<'input>, LexError> {
        match first {
            // . or ... or error
            '.' => {
                match self.peek {
                    Some((_, '.')) => {
                        self.bump();
                        match self.bump() {
                            Some((end, '.')) => Ok((start, Tok::Ellipsis, end)),
                            _ => self.error(start, LexErrorKind::ExpectedEllipsis),
                        }
                    },
                    _ => Ok((start, Tok::Dot, start + 1)),
                }
            },
            // >>= or >> or >= or >
            '>' => {
                match self.peek {
                    Some((_, '>')) => {
                        self.bump();
                        match self.peek {
                            Some((end, '=')) => {
                                self.bump();
                                Ok((start, Tok::RightAssign, end))
                            },
                            _ => Ok((start, Tok::RightSh, start + 2)),
                        }
                    },
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::Gte, start + 2))
                    },
                    _ => Ok((start, Tok::Gt, start + 1))
                }
            },
            // <<= or << or <% or <= or <: or <
            '<' => {
                match self.peek {
                    Some((_, '<')) =>  {
                        self.bump();
                        match self.peek {
                            Some((end, '=')) => {
                                self.bump();
                                Ok((start, Tok::LeftAssign, end))
                            },
                            _ => {
                                Ok((start, Tok::LeftSh, start + 2))
                            },
                        }
                    },
                    Some((_, '%')) => {
                        self.bump();
                        Ok((start, Tok::BraceOpen, start + 2))
                    },
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::Lte, start + 2))
                    },
                    Some((_, ':')) => {
                        self.bump();
                        Ok((start, Tok::BrackOpen, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Lt, start + 1))
                    },
                }
            },
            // += or ++ or +
            '+' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::AddAssign, start + 2))
                    },
                    Some((_, '+')) => {
                        self.bump();
                        Ok((start, Tok::Inc, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Add, start + 1))
                    }
                }
            },
            // -= or -- or -> or -
            '-' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::SubAssign, start + 2))
                    },
                    Some((_, '-')) => {
                        self.bump();
                        Ok((start, Tok::Dec, start + 2))
                    },
                    Some((_, '>')) => {
                        self.bump();
                        Ok((start, Tok::Arrow, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Sub, start + 1))
                    }
                }
            },
            // *= or *
            '*' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::MulAssign, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Mul, start + 1))
                    }
                }
            },
            // /= or /
            '/' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::DivAssign, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Div, start + 1))
                    }
                }
            },
            // %= or %> or %
            '%' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::ModAssign, start + 2))
                    },
                    Some((_, '>')) => {
                        self.bump();
                        Ok((start, Tok::BraceClose, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Mod, start + 1))
                    }
                }
            },
            // &= or && or &
            '&' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::AndAssign, start + 2))
                    },
                    Some((_, '&')) => {
                        self.bump();
                        Ok((start, Tok::LAnd, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::BAnd, start + 1))
                    }
                }
            },
            // ^= or ^
            '^' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::XorAssign, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Xor, start + 1))
                    }
                }
            },
            // |= or || or |
            '|' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::OrAssign, start + 2))
                    },
                    Some((_, '|')) => {
                        self.bump();
                        Ok((start, Tok::LOr, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::BOr, start + 1))
                    }
                }
            },
            // == or =
            '=' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::Eq, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Assign, start + 1))
                    }
                }
            },
            // != or !
            '!' => {
                match self.peek {
                    Some((_, '=')) => {
                        self.bump();
                        Ok((start, Tok::Neq, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::LNot, start + 1))
                    }
                }
            },
            // :> or :
            ':' => {
                match self.peek {
                    Some((_, '>')) => {
                        self.bump();
                        Ok((start, Tok::BrackClose, start + 2))
                    },
                    _ => {
                        Ok((start, Tok::Colon, start + 1))
                    }
                }
            },
            // ?
            '?' => Ok((start, Tok::Qmark, start + 1)),
            ';' => Ok((start, Tok::Semicolon, start + 1)),
            _ => self.error(start, LexErrorKind::ICE),
        }
    }

    fn get_hex_digits_u8(&mut self, buf: &mut [u8]) -> Result<(), LexError> {
        for i in 0..buf.len() {
            match self.bump() {
                Some((p, c)) => {
                    if !c.is_ascii_hexdigit() {
                        return self.error(p, LexErrorKind::BadStringEscape);
                    }
                    buf[i] = c as u8;
                }
                None => {
                    return self.error(self.get_current_index(), LexErrorKind::UnexpectedEof)
                },
            }
        }

        Ok(())
    }

    /**
     * Will fetch at least 1 octal digit
     **/
    fn try_get_octal_digits(&mut self, buf: &mut [u8]) -> Result<usize, LexError> {
        for i in 0..buf.len() {
            match self.peek {
                Some((p, c)) => {
                    match c {
                        ch if is_octal_digit(ch) => buf[i] = c as u8,
                        _ => return if i == 0 { self.error(p, LexErrorKind::BadStringEscape) } else { Ok(i) }
                    }
                }
                None => {
                    return self.error(self.get_current_index(), LexErrorKind::UnexpectedEof)
                },
            }
            self.bump();
        }

        Ok(buf.len())
    }

    fn escape_code(&mut self) -> Result<char, LexError> {
        match self.bump() {
            Some((_, '\'')) => Ok(0x27 as char),
            Some((_, '"')) => Ok(0x22 as char),
            Some((_, '\\')) => Ok(0x5c as char),
            Some((_, 'n')) => Ok(0x0A as char),
            Some((_, 'r')) => Ok(0x0D as char),
            Some((_, 't')) => Ok(0x09 as char),
            Some((_, 'v')) => Ok(0x0B as char),
            Some((_, '?')) => Ok(0x3F as char),
            Some((_, 'u')) => {
                let mut digits = [0u8; 4];
                self.get_hex_digits_u8(&mut digits)?;
                let s = unsafe { str::from_utf8_unchecked(digits.borrow()) };
                let u = u32::from_str(s).unwrap();
                debug_assert!(char::from_u32(u as u32).is_some());
                Ok(char::from_u32(u as u32).unwrap())
            },
            Some((_, 'U')) => {
                let mut digits = [0u8; 8];
                self.get_hex_digits_u8(&mut digits)?;
                let s = unsafe { str::from_utf8_unchecked(digits.borrow()) };
                let u = u32::from_str(s).unwrap();
                debug_assert!(char::from_u32(u).is_some());
                Ok(char::from_u32(u).unwrap())
            },
            Some((_, 'x')) => {
                let mut digits = [0u8; 2];
                self.get_hex_digits_u8(&mut digits)?;
                let s = unsafe { str::from_utf8_unchecked(digits.borrow()) };
                let u = u8::from_str(s).unwrap();
                Ok(u as char)
            },
            Some((_, ch)) if is_octal_digit(ch) => {
                let mut digits = [0u8; 3];
                digits[0] = ch as u8;
                let n = self.try_get_octal_digits(&mut digits)? + 1;
                let s = unsafe { str::from_utf8_unchecked(&digits[..n]) };
                let u = u8::from_str_radix(s, 8).unwrap();
                Ok(u as char)
            },
            Some((start, _)) => {
                self.error(start, LexErrorKind::BadStringEscape)
            },
            None => self.error(self.get_current_index(), LexErrorKind::UnexpectedEof),
        }
    }

    fn char_escape_code(&mut self) -> Result<u8, LexError> {
        match self.bump() {
            Some((_, '\'')) => Ok(0x27),
            Some((_, '"')) => Ok(0x22),
            Some((_, '\\')) => Ok(0x5c),
            Some((_, 'n')) => Ok(0x0A),
            Some((_, 'r')) => Ok(0x0D),
            Some((_, 't')) => Ok(0x09),
            Some((_, 'v')) => Ok(0x0B),
            Some((_, '?')) => Ok(0x3F),
            Some((_, 'x')) => {
                let mut digits = [0u8; 2];
                self.get_hex_digits_u8(&mut digits)?;
                let s = unsafe { str::from_utf8_unchecked(digits.borrow()) };
                let u = u8::from_str(s).unwrap();
                Ok(u)
            },
            Some((_, ch)) if is_octal_digit(ch) => {
                let mut digits = [0u8; 3];
                digits[0] = ch as u8;
                let n = self.try_get_octal_digits(&mut digits)? + 1;
                let s = unsafe { str::from_utf8_unchecked(&digits[..n]) };
                let u = u8::from_str_radix(s, 8).unwrap();
                Ok(u)
            },
            Some((start, _)) => {
                self.error(start, LexErrorKind::BadStringEscape)
            },
            None => self.error(self.get_current_index(), LexErrorKind::UnexpectedEof),
        }
    }

    fn char_literal(&mut self, start: usize) -> Result<SpannedToken<'input>, LexError> {
        let result =
            match self.bump() {
                Some((_, '\\')) => {
                    let byte = self.char_escape_code()?;
                    Ok((start, Tok::Char(byte), self.get_current_index()))
                },
                Some((_, ch)) => {
                    if ch.len_utf8() > 1 {
                        self.error(start, LexErrorKind::InvalidCharLiteral)
                    } else {
                        let byte = ch as u8;
                        Ok((start, Tok::Char(byte), start + 1))
                    }
                },
                _ => self.error(start, LexErrorKind::UnexpectedEof)
            };
        match self.bump() {
            Some((_, '\'')) => result,
            Some(_) => self.error(start, LexErrorKind::InvalidCharLiteral),
            _ => self.error(start, LexErrorKind::UnexpectedEof)
        }
    }

    fn string_literal(&mut self, start: usize) -> Result<SpannedToken<'input>, LexError> {
        let mut string = String::new();

        while let Some((next, ch)) = self.bump() {
            match ch {
                '\\' => string.push(self.escape_code()?),
                '"' => {
                    let end = next + ch.len_utf8();
                    let token = Tok::String(string.into_bytes().into_boxed_slice());
                    return Ok((start, token, end));
                },
                '\r' => break,
                '\n' => break,
                ch => string.push(ch),
            }
        }

        self.error(start, LexErrorKind::UntermnatedString)
    }

    fn try_get_int_suffix(&mut self) -> Option<IntType> {
        if let Some((_, ch)) = self.peek {
            let suffix =
                match ch {
                    'u' | 'U' => IntType::Unsigned,
                    'l' | 'L' => IntType::Long,
                    _ => return None
                };
            self.bump();
            Some(suffix)
        } else {
            None
        }
    }

    fn try_get_float_suffix(&mut self) -> Option<FloatType> {
        if let Some((_, ch)) = self.peek {
            let suffix =
                match ch {
                    'f' | 'F' => FloatType::Float,
                    'l' | 'L' => FloatType::Double,
                    _ => return None
                };
            self.bump();
            Some(suffix)
        } else {
            None
        }
    }

    fn try_get_float_exp(&mut self, buf: &mut String) -> Result<(), LexError> {
        // If we match an E we are definitely dealing with a float.
        // [eE]
        match self.peek {
            Some((_, ch)) if ch == 'e' || ch == 'E' => {},
            _ => return Ok(())
        };
        self.bump();
        buf.push('E');

        // [+-]?
        match self.peek {
            Some((_, ch)) if ch == '-' || ch == '+' => {
                self.bump();
                buf.push(ch);
            },
            _ => {}
        }

        // [0-9]+
        self.get_digits(buf)?;

        Ok(())
    }

    fn get_digits(&mut self, buf: &mut String) -> Result<(), LexError> {
        // let mut n = 0;
        while let Some((_, ch)) = self.peek {
            if is_digit(ch) {
                self.bump();
                // n += 1;
                buf.push(ch);
            } else {
                return Ok(()) // Ok(n);
            }
        }
        Ok(())
        // Ok(n)
    }

    fn get_hex_digits(&mut self, buf: &mut String) -> Result<(), LexError> {
        // let mut n = 0;
        while let Some((_, ch)) = self.peek {
            if ch.is_ascii_hexdigit() {
                self.bump();
                // n += 1;
                buf.push(ch);
            } else {
                return Ok(()) // Ok(n);
            }
        }
        Ok(())
        // Ok(n)
    }

    fn get_current_index(&self) -> usize {
        if let Some((end, _)) = self.peek {
            end
        } else {
            self.input.len()
        }
    }

    fn hex_literal(&mut self) -> Result<SpannedToken<'input>, LexError> {
        let mut digits = String::with_capacity(32);
        match self.peek {
            Some((_, ch)) if ch.is_ascii_hexdigit() => {},
            Some((start, _)) => return self.error(start, LexErrorKind::BadHexLiteral),
            _ => return self.error(self.get_current_index(), LexErrorKind::UnexpectedEof),
        };

        let (start, ch) = self.bump().unwrap();
        digits.push(ch);

        // [0-9a-fA-F]*
        self.get_hex_digits(&mut digits);

        let suffix =
            match self.try_get_int_suffix() {
                Some(suffix) => suffix,
                _ => IntType::Long,
            };

        let value = i128::from_str_radix(digits.borrow(), 16).unwrap();
        Ok((start, Tok::Int((value, suffix)), self.get_current_index()))
    }

    fn octal_literal(&mut self) -> Result<SpannedToken<'input>, LexError> {
        let mut digits = String::with_capacity(32);
        match self.peek {
            Some((_, ch)) if is_octal_digit(ch) => {},
            Some((start, _)) => return self.error(start, LexErrorKind::BadOctalLiteral),
            _ => return self.error(self.get_current_index(), LexErrorKind::UnexpectedEof),
        };

        let (start, ch) = self.bump().unwrap();
        digits.push(ch);

        // [0-9a-fA-F]*
        self.get_hex_digits(&mut digits)?;

        let suffix =
            match self.try_get_int_suffix() {
                Some(suffix) => suffix,
                _ => IntType::Long,
            };

        let value = i128::from_str_radix(digits.borrow(), 16).unwrap();
        Ok((start, Tok::Int((value, suffix)), self.get_current_index()))
    }

    fn numeric_literal(&mut self, start: usize, first_char: char) -> Result<SpannedToken<'input>, LexError> {
        match first_char {
            '0' => {
                match self.peek {
                    Some((_next, ch)) if ch == 'x' || ch == 'X' => {
                        self.bump();
                        self.hex_literal()
                    },
                    Some(_) => {
                        self.octal_literal()
                    },
                    _ => self.error(self.get_current_index(), LexErrorKind::UnexpectedEof)
                }
            },
            '1'..= '9' => {
                let mut value = String::with_capacity(64);
                value.push(first_char);

                // [0-9]*
                self.get_digits(&mut value)?;

                if let Some((_next, '.')) = self.peek {
                    // This is going to be a float
                    value.push('.');
                    self.get_digits(&mut value)?;
                    self.try_get_float_exp(&mut value)?;
                    let suffix =
                        match self.try_get_float_suffix() {
                            Some(x) => x,
                            None => FloatType::Double,
                        };
                    let value = f64::from_str(value.borrow()).unwrap();
                    let end = self.get_current_index();
                    Ok((start, Tok::Float((value, suffix)), end))
                } else {
                    // integer
                    let suffix =
                        match self.try_get_int_suffix() {
                            Some(x) => x,
                            None => IntType::Long,
                        };
                    let value = i128::from_str(value.borrow()).unwrap();
                    let end = self.get_current_index();
                    Ok((start, Tok::Int((value, suffix)), end))
                }
            },
            '.' => {
                let mut value = String::with_capacity(64);
                value.push('.');
                match self.peek {
                    Some((_, c)) if c >= '0' && c <= '9' => {
                        self.bump();
                        value.push(c);
                    },
                    _ => return Ok((start, Tok::Dot, self.get_current_index())),
                };

                // [0-9]*
                self.get_digits(&mut value)?;
                self.try_get_float_exp(&mut value)?;
                let suffix =
                    match self.try_get_float_suffix() {
                        Some(x) => x,
                        None => FloatType::Double,
                    };
                let value = f64::from_str(value.borrow()).unwrap();
                let end = self.get_current_index();
                Ok((start, Tok::Float((value, suffix)), end))
            },
            _ => self.error(start, LexErrorKind::ICE)
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((start, ch)) = self.peek {
            let end = start + ch.len_utf8();
            return match ch {
                ',' => { self.bump(); Some(Ok((start, Tok::Comma, end))) },
                '{' => { self.bump(); Some(Ok((start, Tok::BraceOpen, end))) },
                '[' => { self.bump(); Some(Ok((start, Tok::BrackOpen, end))) },
                '(' => { self.bump(); Some(Ok((start, Tok::ParOpen, end))) },
                ')' => { self.bump(); Some(Ok((start, Tok::ParClose, end))) },
                ']' => { self.bump(); Some(Ok((start, Tok::BrackClose, end))) },
                '}' => { self.bump(); Some(Ok((start, Tok::BraceClose, end))) },
                '?' => { self.bump(); Some(Ok((start, Tok::Qmark, end))) },
                '~' => { self.bump(); Some(Ok((start, Tok::BNot, end))) },
                '"' => { self.bump(); Some(self.string_literal(start)) },
                '\'' => { self.bump(); Some(self.char_literal(start)) },
                '/' if self.next_char_is('*') => {
                    self.bump(); 
                    match self.block_comment(start) {
                        Ok(()) => continue,
                        Err(err) => Some(Err(err)),
                    }
                },
                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),
                ch if is_digit(ch) || (ch == '.' && self.test_lookahead(is_digit)) => {
                    self.bump();
                    Some(self.numeric_literal(start, ch))
                }
                ch if is_operator_char(ch) => { self.bump(); Some(self.operator(start, ch)) },
                ch if ch.is_whitespace() => { self.bump(); continue },
                ch => Some(self.error(start, LexErrorKind::UnexpectedChar(ch))),
            };
        }
        if self.eof_emitted {
            None
        } else {
            self.eof_emitted = true;
            // Return EOF instead of None so that the layout algorithm receives the eof location
            Some(Ok((self.get_current_index(), Tok::EOF, self.get_current_index())))
        }
    }
}