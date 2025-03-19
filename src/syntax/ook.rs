//! Parser for Ook!

use std::io::{self, BufRead, ErrorKind};
use std::str::from_utf8;

use bytecode::ByteCodeWriter;
use syntax::brainfuck::{
    Decrement, Get, Increment, Instructions, LoopEnd, LoopStart, MoveLeft, MoveRight, Put, Token,
};
use syntax::Compiler;

struct Tokens<T> {
    lexemes: T,
}

impl<I: Iterator<Item = io::Result<String>>> Tokens<I> {
    pub fn parse(self) -> Instructions<Tokens<I>> {
        Instructions::new(self)
    }
}

impl<I: Iterator<Item = io::Result<String>>> Iterator for Tokens<I> {
    type Item = io::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let op = self.lexemes.next();
        if op.is_none() {
            return None;
        }

        let res = op.unwrap();
        match res {
            Err(e) => return Some(Err(e)),
            Ok(_) => (),
        }

        Some(match res.unwrap().as_str() {
            "Ook. Ook?" => Ok(MoveRight),
            "Ook? Ook." => Ok(MoveLeft),
            "Ook. Ook." => Ok(Increment),
            "Ook! Ook!" => Ok(Decrement),
            "Ook. Ook!" => Ok(Get),
            "Ook! Ook." => Ok(Put),
            "Ook! Ook?" => Ok(LoopStart),
            "Ook? Ook!" => Ok(LoopEnd),
            _ => Err(ErrorKind::InvalidInput.into()),
        })
    }
}

fn is_whitespace(c: &char) -> bool {
    *c == ' ' || is_linebreak(c)
}

fn is_linebreak(c: &char) -> bool {
    *c == '\n' || *c == '\r'
}

struct Scan<'r, T> {
    buffer: &'r mut T,
    is_start: bool,
}

impl<'r, B: BufRead> Scan<'r, B> {
    pub fn tokenize(self) -> Tokens<Scan<'r, B>> {
        Tokens { lexemes: self }
    }
}

impl<'r, B: BufRead> Iterator for Scan<'r, B> {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0u8, ..9];

        if !self.is_start {
            // skip separator
            match self.buffer.read_char() {
                Ok(ref c) if is_whitespace(c) => (),
                Ok(_) => return Some(Err(ErrorKind::InvalidInput.into())),
                Err(io::Error {
                    kind: EndOfFile, ..
                }) => return None,
                Err(e) => return Some(Err(e)),
            }
            // skip linebreak
            loop {
                match self.buffer.read_char() {
                    Ok(ref c) if is_linebreak(c) => continue,
                    Ok(c) => {
                        buf[0] = c as u8;
                        break;
                    }
                    Err(io::Error {
                        kind: EndOfFile, ..
                    }) => return None,
                    Err(e) => return Some(Err(e)),
                }
            }
            match self.buffer.read(buf.mut_slice_from(1)) {
                Ok(8) => {}
                Ok(0) => return None,
                Ok(_) => return Some(Err(ErrorKind::InvalidInput.into())),
                Err(e) => return Some(Err(e)),
            }
        } else {
            match self.buffer.read(&mut buf) {
                Ok(9) => {}
                Ok(0) => return None,
                Ok(_) => return Some(Err(ErrorKind::InvalidInput.into())),
                Err(e) => return Some(Err(e)),
            }
            self.is_start = false;
        }

        match from_utf8(&buf) {
            Ok(string) => Some(Ok(string.into())),
            Err(err) => Some(Err(io::Error::new(ErrorKind::InvalidInput, err))),
        }
    }
}

fn scan<'r, B: BufRead>(buffer: &'r mut B) -> Scan<'r, B> {
    Scan {
        buffer: buffer,
        is_start: true,
    }
}

/// Compiler for Ook!.
pub struct Ook;

impl Ook {
    /// Create a new `Ook`.
    pub fn new() -> Ook {
        Ook
    }
}

impl Compiler for Ook {
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

    use syntax::brainfuck::{
        Decrement, Get, Increment, LoopEnd, LoopStart, MoveLeft, MoveRight, Put,
    };

    #[test]
    fn test_scan() {
        let mut buffer = Cursor::new("Ook? Ook. Ook! Ook.\nOok. Ook? Ook.".as_bytes());
        let mut it = super::scan(&mut buffer);
        assert_eq!(it.next().unwrap().unwrap(), "Ook? Ook.");
        assert_eq!(it.next().unwrap().unwrap(), "Ook! Ook.");
        assert_eq!(it.next().unwrap().unwrap(), "Ook. Ook?");
        assert!(it.next().unwrap().is_err());
    }

    #[test]
    fn test_tokenize() {
        let source = vec![
            "Ook. Ook?",
            "Ook? Ook.",
            "Ook. Ook.",
            "Ook! Ook!",
            "Ook. Ook!",
            "Ook! Ook.",
            "Ook! Ook?",
            "Ook? Ook!",
        ]
        .connect(" ");
        let mut buffer = Cursor::new(source.as_bytes());
        let it = super::scan(&mut buffer).tokenize();
        let expected = &[
            MoveRight, MoveLeft, Increment, Decrement, Get, Put, LoopStart, LoopEnd,
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }
}
