//! Parser and Generator for Whitespace.

use std::collections::HashMap;
use std::io::{self, BufRead, ErrorKind, Write};

use crate::bytecode::{ByteCodeReader, ByteCodeWriter};
use crate::io::BufReadExt;
use crate::ir;
use crate::ir::Instruction;
use crate::syntax::{Compiler, Decompiler};

macro_rules! write_num (
    ($w:expr, $cmd:expr, $n:expr) => (
        write!(
            $w,
            "{}{}",
            $cmd,
            if $n < 0 {
                format!("\t{:b}\n", -$n)
            } else {
                format!(" {:b}\n", $n)
            }
            .replace("0", " ")
            .replace("1", "\t")
        )
    )
);

fn unknown_instruction(inst: &'static str) -> io::Error {
    io::Error::new(
        ErrorKind::InvalidInput,
        format!("syntax error: \"{}\" is unknown instruction", inst),
    )
}

/// An iterator that convert to IR from whitespace tokens on each iteration.
pub struct Instructions<T> {
    tokens: T,
    labels: HashMap<String, i64>,
    count: i64,
}

impl<I: Iterator<Item = io::Result<Token>>> Instructions<I> {
    /// Create an iterator that convert to IR from tokens on each iteration.
    pub fn new(iter: I) -> Instructions<I> {
        Instructions {
            tokens: iter,
            labels: HashMap::new(),
            count: 1,
        }
    }

    fn parse_value(&mut self) -> io::Result<String> {
        let mut value = String::new();
        loop {
            match self.tokens.next() {
                Some(Ok(Space)) => value.push('0'),
                Some(Ok(Tab)) => value.push('1'),
                Some(Ok(LF)) => break,
                Some(Err(e)) => return Err(e),
                None => {
                    return Err(io::Error::new(
                        ErrorKind::InvalidInput,
                        "syntax error: no value terminator",
                    ))
                }
            }
        }
        Ok(value)
    }

    fn parse_sign(&mut self) -> io::Result<bool> {
        match self.tokens.next() {
            Some(Ok(Space)) => Ok(true),
            Some(Ok(Tab)) => Ok(false),
            Some(Ok(LF)) | None => Err(io::Error::new(
                ErrorKind::InvalidInput,
                "invalid value format: no sign",
            )),
            Some(Err(e)) => Err(e),
        }
    }

    fn parse_number(&mut self) -> io::Result<i64> {
        let positive = self.parse_sign()?;
        let value = self.parse_value()?;
        match i64::from_str_radix(&value, 2) {
            Ok(n) => Ok(if positive { n } else { -n }),
            Err(err) => Err(io::Error::new(ErrorKind::InvalidInput, err)),
        }
    }

    fn parse_label(&mut self) -> io::Result<i64> {
        let label = self.parse_value()?;
        match self.labels.get(&label) {
            Some(&val) => Ok(val),
            None => {
                let val = self.count;
                self.count += 1;
                self.labels.insert(label, val);
                Ok(val)
            }
        }
    }

    fn parse_stack(&mut self) -> io::Result<Instruction> {
        match self.tokens.next() {
            Some(Ok(Space)) => Ok(ir::StackPush(self.parse_number()?)),
            Some(Ok(LF)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::StackDuplicate),
                Some(Ok(Tab)) => Ok(ir::StackSwap),
                Some(Ok(LF)) => Ok(ir::StackDiscard),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("SN")),
            },
            Some(Ok(Tab)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::StackCopy(self.parse_number()?)),
                Some(Ok(LF)) => Ok(ir::StackSlide(self.parse_number()?)),
                Some(Ok(Tab)) => Err(unknown_instruction("STT")),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("ST")),
            },
            Some(Err(e)) => Err(e),
            None => Err(unknown_instruction("S")),
        }
    }

    fn parse_arithmetic(&mut self) -> io::Result<Instruction> {
        match self.tokens.next() {
            Some(Ok(Space)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::Addition),
                Some(Ok(Tab)) => Ok(ir::Subtraction),
                Some(Ok(LF)) => Ok(ir::Multiplication),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("TSS")),
            },
            Some(Ok(Tab)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::Division),
                Some(Ok(Tab)) => Ok(ir::Modulo),
                Some(Ok(LF)) => Err(unknown_instruction("TSTN")),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("TST")),
            },
            Some(Ok(LF)) => Err(unknown_instruction("TSN")),
            Some(Err(e)) => Err(e),
            None => Err(unknown_instruction("TS")),
        }
    }

    fn parse_heap(&mut self) -> io::Result<Instruction> {
        match self.tokens.next() {
            Some(Ok(Space)) => Ok(ir::HeapStore),
            Some(Ok(Tab)) => Ok(ir::HeapRetrieve),
            Some(Err(e)) => Err(e),
            Some(Ok(LF)) => Err(unknown_instruction("TTN")),
            None => Err(unknown_instruction("TT")),
        }
    }

    fn parse_flow(&mut self) -> io::Result<Instruction> {
        match self.tokens.next() {
            Some(Ok(Space)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::Mark(self.parse_label()?)),
                Some(Ok(Tab)) => Ok(ir::Call(self.parse_label()?)),
                Some(Ok(LF)) => Ok(ir::Jump(self.parse_label()?)),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("NS")),
            },
            Some(Ok(Tab)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::JumpIfZero(self.parse_label()?)),
                Some(Ok(Tab)) => Ok(ir::JumpIfNegative(self.parse_label()?)),
                Some(Ok(LF)) => Ok(ir::Return),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("NT")),
            },
            Some(Ok(LF)) => match self.tokens.next() {
                Some(Ok(LF)) => Ok(ir::Exit),
                Some(Ok(Space)) => Err(unknown_instruction("NNS")),
                Some(Ok(Tab)) => Err(unknown_instruction("NNT")),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("NN")),
            },
            Some(Err(e)) => Err(e),
            None => Err(unknown_instruction("N")),
        }
    }

    fn parse_io(&mut self) -> io::Result<Instruction> {
        match self.tokens.next() {
            Some(Ok(Space)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::PutCharactor),
                Some(Ok(Tab)) => Ok(ir::PutNumber),
                Some(Ok(LF)) => Err(unknown_instruction("TNSN")),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("TNS")),
            },
            Some(Ok(Tab)) => match self.tokens.next() {
                Some(Ok(Space)) => Ok(ir::GetCharactor),
                Some(Ok(Tab)) => Ok(ir::GetNumber),
                Some(Ok(LF)) => Err(unknown_instruction("TNTN")),
                Some(Err(e)) => Err(e),
                None => Err(unknown_instruction("TNT")),
            },
            Some(Ok(LF)) => Err(unknown_instruction("TNN")),
            Some(Err(e)) => Err(e),
            None => Err(unknown_instruction("TN")),
        }
    }
}

impl<I: Iterator<Item = io::Result<Token>>> Iterator for Instructions<I> {
    type Item = io::Result<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.next() {
            Some(Ok(Space)) => Some(self.parse_stack()),
            Some(Ok(Tab)) => match self.tokens.next() {
                Some(Ok(Space)) => Some(self.parse_arithmetic()),
                Some(Ok(Tab)) => Some(self.parse_heap()),
                Some(Ok(LF)) => Some(self.parse_io()),
                _ => Some(Err(ErrorKind::InvalidInput.into())),
            },
            Some(Ok(LF)) => Some(self.parse_flow()),
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

#[allow(missing_docs)]
#[derive(PartialEq, Debug)]
pub enum Token {
    Space,
    Tab,
    LF,
}

pub use self::Token::*;

struct Tokens<T> {
    lexemes: T,
}

impl<I: Iterator<Item = io::Result<char>>> Tokens<I> {
    pub fn parse(self) -> Instructions<Tokens<I>> {
        Instructions::new(self)
    }
}

impl<I: Iterator<Item = io::Result<char>>> Iterator for Tokens<I> {
    type Item = io::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.lexemes.next()? {
            Ok(' ') => Ok(Space),
            Ok('\t') => Ok(Tab),
            Ok('\n') => Ok(LF),
            Ok(_) => Err(ErrorKind::InvalidInput.into()),
            Err(e) => Err(e),
        })
    }
}

struct Scan<'r, T> {
    buffer: &'r mut T,
}

impl<'r, B: BufRead> Scan<'r, B> {
    pub fn tokenize(self) -> Tokens<Scan<'r, B>> {
        Tokens { lexemes: self }
    }
}

impl<B: BufRead> Iterator for Scan<'_, B> {
    type Item = io::Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let ret = match self.buffer.read_char() {
                Ok(' ') => ' ',
                Ok('\t') => '\t',
                Ok('\n') => '\n',
                Ok(_) => continue,
                Err(err) if err.kind() == ErrorKind::UnexpectedEof => return None,
                Err(e) => return Some(Err(e)),
            };
            return Some(Ok(ret));
        }
    }
}

fn scan<B: BufRead>(buffer: &mut B) -> Scan<'_, B> {
    Scan { buffer }
}

/// Compiler and Decompiler for Whitespace.
pub struct Whitespace;

impl Whitespace {
    /// Create a new `Whitespace`.
    pub fn new() -> Whitespace {
        Whitespace
    }
}

impl Compiler for Whitespace {
    fn compile<B: BufRead, W: ByteCodeWriter>(
        &self,
        input: &mut B,
        output: &mut W,
    ) -> io::Result<()> {
        let mut it = scan(input).tokenize().parse();
        output.assemble(&mut it)
    }
}

impl Decompiler for Whitespace {
    fn decompile<R: ByteCodeReader, W: Write>(
        &self,
        input: &mut R,
        output: &mut W,
    ) -> io::Result<()> {
        for inst in input.disassemble() {
            match inst {
                Ok(ir::StackPush(n)) => write_num!(output, "  ", n),
                Ok(ir::StackDuplicate) => write!(output, " \n "),
                Ok(ir::StackCopy(n)) => write_num!(output, " \t ", n),
                Ok(ir::StackSwap) => write!(output, " \n\t"),
                Ok(ir::StackDiscard) => write!(output, " \n\n"),
                Ok(ir::StackSlide(n)) => write_num!(output, " \t\n", n),
                Ok(ir::Addition) => write!(output, "\t   "),
                Ok(ir::Subtraction) => write!(output, "\t  \t"),
                Ok(ir::Multiplication) => write!(output, "\t  \n"),
                Ok(ir::Division) => write!(output, "\t \t "),
                Ok(ir::Modulo) => write!(output, "\t \t\t"),
                Ok(ir::HeapStore) => write!(output, "\t\t "),
                Ok(ir::HeapRetrieve) => write!(output, "\t\t\t"),
                Ok(ir::Mark(n)) => write_num!(output, "\n  ", n),
                Ok(ir::Call(n)) => write_num!(output, "\n \t", n),
                Ok(ir::Jump(n)) => write_num!(output, "\n \n", n),
                Ok(ir::JumpIfZero(n)) => write_num!(output, "\n\t ", n),
                Ok(ir::JumpIfNegative(n)) => write_num!(output, "\n\t\t", n),
                Ok(ir::Return) => write!(output, "\n\t\n"),
                Ok(ir::Exit) => write!(output, "\n\n\n"),
                Ok(ir::PutCharactor) => write!(output, "\t\n  "),
                Ok(ir::PutNumber) => write!(output, "\t\n \t"),
                Ok(ir::GetCharactor) => write!(output, "\t\n\t "),
                Ok(ir::GetNumber) => write!(output, "\t\n\t\t"),
                Err(e) => Err(e),
            }?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::io::{self, Cursor, Seek, SeekFrom};
    use std::str::from_utf8;

    use crate::bytecode::ByteCodeWriter;
    use crate::ir::*;
    use crate::syntax::Decompiler;

    #[test]
    fn test_scan() {
        let mut buffer = Cursor::new(" [\t饂飩]\n".as_bytes());
        let it = super::scan(&mut buffer);
        let expected = &[' ', '\t', '\n'];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_tokenize() {
        let mut buffer = Cursor::new(" [\t饂飩]\n".as_bytes());
        let it = super::scan(&mut buffer).tokenize();
        let expected = &[super::Space, super::Tab, super::LF];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_parse() {
        let source = vec![
            "   \t\n",     // PUSH 1
            " \n ",        // DUP
            " \t  \t\n",   // COPY 1
            " \n\t",       // SWAP
            " \n\n",       // DISCARD
            " \t\n \t\n",  // SLIDE 1
            "\t   ",       // ADD
            "\t  \t",      // SUB
            "\t  \n",      // MUL
            "\t \t ",      // DIV
            "\t \t\t",     // MOD
            "\t\t ",       // STORE
            "\t\t\t",      // RETRIEVE
            "\n   \t\n",   // MARK 01
            "\n \t\t \n",  // CALL 10
            "\n \n \t\n",  // JUMP 01
            "\n\t \t \n",  // JUMPZ 10
            "\n\t\t \t\n", // JUMPN 01
            "\n\t\n",      // RETURN
            "\n\n\n",      // EXIT
            "\t\n  ",      // PUTC
            "\t\n \t",     // PUTN
            "\t\n\t ",     // GETC
            "\t\n\t\t",    // GETN
        ]
        .concat();
        let mut buffer = Cursor::new(source.as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(1),
            StackDuplicate,
            StackCopy(1),
            StackSwap,
            StackDiscard,
            StackSlide(1),
            Addition,
            Subtraction,
            Multiplication,
            Division,
            Modulo,
            HeapStore,
            HeapRetrieve,
            Mark(1),
            Call(2),
            Jump(1),
            JumpIfZero(2),
            JumpIfNegative(1),
            Return,
            Exit,
            PutCharactor,
            PutNumber,
            GetCharactor,
            GetNumber,
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_generate() {
        let mut bc2 = Cursor::new(Vec::new());
        {
            let mut bc = Cursor::new(Vec::new());
            bc.write_push(1).unwrap();
            bc.write_dup().unwrap();
            bc.write_copy(2).unwrap();
            bc.write_swap().unwrap();
            bc.write_discard().unwrap();
            bc.write_slide(3).unwrap();
            bc.write_add().unwrap();
            bc.write_sub().unwrap();
            bc.write_mul().unwrap();
            bc.write_div().unwrap();
            bc.write_mod().unwrap();
            bc.write_store().unwrap();
            bc.write_retrieve().unwrap();
            bc.write_mark(1).unwrap();
            bc.write_call(1).unwrap();
            bc.write_jump(1).unwrap();
            bc.write_jumpz(1).unwrap();
            bc.write_jumpn(1).unwrap();
            bc.write_return().unwrap();
            bc.write_exit().unwrap();
            bc.write_putc().unwrap();
            bc.write_putn().unwrap();
            bc.write_getc().unwrap();
            bc.write_getn().unwrap();

            bc.seek(SeekFrom::Start(0)).unwrap();
            let syntax = super::Whitespace::new();
            syntax.decompile(&mut bc, &mut bc2).unwrap();
        }
        let result = from_utf8(bc2.get_ref())
            .unwrap()
            .replace(" ", "S")
            .replace("\t", "T")
            .replace("\n", "N");
        let expected = vec![
            "   \t\n",
            " \n ",
            " \t  \t \n",
            " \n\t",
            " \n\n",
            " \t\n \t\t\n",
            "\t   ",
            "\t  \t",
            "\t  \n",
            "\t \t ",
            "\t \t\t",
            "\t\t ",
            "\t\t\t",
            "\n   \t\n",
            "\n \t \t\n",
            "\n \n \t\n",
            "\n\t  \t\n",
            "\n\t\t \t\n",
            "\n\t\n",
            "\n\n\n",
            "\t\n  ",
            "\t\n \t",
            "\t\n\t ",
            "\t\n\t\t",
        ]
        .concat()
        .replace(" ", "S")
        .replace("\t", "T")
        .replace("\n", "N");
        assert_eq!(result, expected);
    }
}
