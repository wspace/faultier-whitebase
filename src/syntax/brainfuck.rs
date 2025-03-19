//! Parser for Brainfuck.

use std::collections::{HashMap, VecDeque};
use std::io::{self, BufRead, ErrorKind};

use bytecode::ByteCodeWriter;
use io::BufReadExt;
use ir;
use ir::Instruction;
use syntax::Compiler;

pub const BF_FAIL_MARKER: i64 = -1;
pub const BF_PTR_ADDR: i64 = -1;

/// An iterator that convert to IR from brainfuck tokens on each iteration.
pub struct Instructions<T> {
    tokens: T,
    stack: Vec<i64>,
    scount: i64,
    labels: HashMap<String, i64>,
    lcount: i64,
    buffer: VecDeque<io::Result<Instruction>>,
    parsed: bool,
}

impl<I: Iterator<Item = io::Result<Token>>> Instructions<I> {
    /// Create an iterator that convert to IR from tokens on each iteration.
    pub fn new(iter: I) -> Instructions<I> {
        Instructions {
            tokens: iter,
            stack: Vec::new(),
            scount: 1,
            labels: HashMap::new(),
            lcount: 1,
            buffer: VecDeque::new(),
            parsed: false,
        }
    }

    fn marker(&mut self, label: String) -> i64 {
        match self.labels.get(&label) {
            Some(&val) => val,
            None => {
                let val = self.lcount;
                self.lcount += 1;
                self.labels.insert(label, val);
                val
            }
        }
    }
}

impl<I: Iterator<Item = io::Result<Token>>> Iterator for Instructions<I> {
    type Item = io::Result<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.buffer.pop_front() {
            Some(i) => Some(i),
            None => {
                let ret = match self.tokens.next() {
                    Some(Ok(MoveRight)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::StackDuplicate),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackPush(1)),
                        Ok(ir::Addition),
                        Ok(ir::HeapStore),
                    ],
                    Some(Ok(MoveLeft)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::StackDuplicate),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackPush(1)),
                        Ok(ir::Subtraction),
                        Ok(ir::StackDuplicate),
                        Ok(ir::JumpIfNegative(BF_FAIL_MARKER)),
                        Ok(ir::HeapStore),
                    ],
                    Some(Ok(Increment)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackDuplicate),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackPush(1)),
                        Ok(ir::Addition),
                        Ok(ir::HeapStore),
                    ],
                    Some(Ok(Decrement)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackDuplicate),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::StackPush(1)),
                        Ok(ir::Subtraction),
                        Ok(ir::HeapStore),
                    ],
                    Some(Ok(Get)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::GetCharactor),
                    ],
                    Some(Ok(Put)) => vec![
                        Ok(ir::StackPush(BF_PTR_ADDR)),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::HeapRetrieve),
                        Ok(ir::PutCharactor),
                    ],
                    Some(Ok(LoopStart)) => {
                        let l: i64 = self.scount;
                        self.scount += 1;
                        self.stack.push(l);
                        vec![
                            Ok(ir::Mark(self.marker(format!("{}#", l)))),
                            Ok(ir::StackPush(BF_PTR_ADDR)),
                            Ok(ir::HeapRetrieve),
                            Ok(ir::HeapRetrieve),
                            Ok(ir::JumpIfZero(self.marker(format!("#{}", l)))),
                        ]
                    }
                    Some(Ok(LoopEnd)) => match self.stack.pop() {
                        Some(l) => vec![
                            Ok(ir::Jump(self.marker(format!("{}#", l)))),
                            Ok(ir::Mark(self.marker(format!("#{}", l)))),
                        ],
                        None => vec![Err(io::Error::new(
                            ErrorKind::InvalidInput,
                            "syntax error: broken loop",
                        ))],
                    },
                    Some(Err(e)) => vec![Err(e)],
                    None => {
                        if self.parsed {
                            return None;
                        }
                        self.parsed = true;
                        vec![Ok(ir::Exit), Ok(ir::Mark(BF_FAIL_MARKER))]
                    }
                };
                self.buffer.extend(ret);
                self.buffer.pop_front()
            }
        }
    }
}

#[allow(missing_docs)]
#[derive(PartialEq, Debug)]
pub enum Token {
    MoveRight,
    MoveLeft,
    Increment,
    Decrement,
    Put,
    Get,
    LoopStart,
    LoopEnd,
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
        let c = self.lexemes.next();
        if c.is_none() {
            return None;
        }

        Some(match c.unwrap() {
            Ok('>') => Ok(MoveRight),
            Ok('<') => Ok(MoveLeft),
            Ok('+') => Ok(Increment),
            Ok('-') => Ok(Decrement),
            Ok(',') => Ok(Get),
            Ok('.') => Ok(Put),
            Ok('[') => Ok(LoopStart),
            Ok(']') => Ok(LoopEnd),
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

impl<'r, B: BufRead> Iterator for Scan<'r, B> {
    type Item = io::Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let ret = match self.buffer.read_char() {
                Ok('>') => '>',
                Ok('<') => '<',
                Ok('+') => '+',
                Ok('-') => '-',
                Ok(',') => ',',
                Ok('.') => '.',
                Ok('[') => '[',
                Ok(']') => ']',
                Ok(_) => continue,
                Err(err) if err.kind() == ErrorKind::UnexpectedEof => return None,
                Err(e) => return Some(Err(e)),
            };
            return Some(Ok(ret));
        }
    }
}

fn scan<'r, B: BufRead>(buffer: &'r mut B) -> Scan<'r, B> {
    Scan { buffer: buffer }
}

/// Compiler for Brainfuck.
pub struct Brainfuck;

impl Brainfuck {
    /// Create a new `Brainfuck`.
    pub fn new() -> Brainfuck {
        Brainfuck
    }
}

impl Compiler for Brainfuck {
    fn compile<B: BufRead, W: ByteCodeWriter>(
        &self,
        input: &mut B,
        output: &mut W,
    ) -> io::Result<()> {
        let mut it = scan(input).tokenize().parse();
        output.assemble(&mut it)
    }
}

#[cfg(test)]
mod test {
    use std::io::{self, Cursor};

    use ir::*;

    #[test]
    fn test_scan() {
        let mut buffer = Cursor::new("><+- ,.\n[饂飩]".as_bytes());
        let it = super::scan(&mut buffer);
        let expected = &['>', '<', '+', '-', ',', '.', '[', ']'];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_tokenize() {
        let mut buffer = Cursor::new("><+- ,.\n[饂飩]".as_bytes());
        let it = super::scan(&mut buffer).tokenize();
        let expected = &[
            super::MoveRight,
            super::MoveLeft,
            super::Increment,
            super::Decrement,
            super::Get,
            super::Put,
            super::LoopStart,
            super::LoopEnd,
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_parse() {
        let mut buffer = Cursor::new(">".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            StackDuplicate,
            HeapRetrieve,
            StackPush(1),
            Addition,
            HeapStore,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new("<".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            StackDuplicate,
            HeapRetrieve,
            StackPush(1),
            Subtraction,
            StackDuplicate,
            JumpIfNegative(super::BF_FAIL_MARKER),
            HeapStore,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new("+".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            StackDuplicate,
            HeapRetrieve,
            StackPush(1),
            Addition,
            HeapStore,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new("-".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            StackDuplicate,
            HeapRetrieve,
            StackPush(1),
            Subtraction,
            HeapStore,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new(",".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            HeapRetrieve,
            GetCharactor,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new(".".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            HeapRetrieve,
            PutCharactor,
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);

        let mut buffer = Cursor::new("[[]]".as_bytes());
        let it = super::scan(&mut buffer).tokenize().parse();
        let expected = &[
            // outer loop
            Mark(1),
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            HeapRetrieve,
            JumpIfZero(2),
            // inner loop
            Mark(3),
            StackPush(super::BF_PTR_ADDR),
            HeapRetrieve,
            HeapRetrieve,
            JumpIfZero(4),
            Jump(3),
            Mark(4),
            // outer loop
            Jump(1),
            Mark(2),
            Exit,
            Mark(super::BF_FAIL_MARKER),
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }
}
