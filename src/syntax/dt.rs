//! Parser and Generator for DT.

use std::io::{self, BufRead, ErrorKind, Write};

use crate::bytecode::{ByteCodeReader, ByteCodeWriter};
use crate::io::BufReadExt;
use crate::ir;
use crate::syntax::whitespace::{Instructions, Space, Tab, Token, LF};
use crate::syntax::{Compiler, Decompiler};

const S: &'static str = "ど";
const T: &'static str = "童貞ちゃうわっ！";
const N: &'static str = "…";

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
            S => Ok(Space),
            T => Ok(Tab),
            N => Ok(LF),
            _ => Err(ErrorKind::InvalidInput.into()),
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
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        'outer: loop {
            match self.buffer.read_char() {
                Ok(c) if c == S.chars().next().unwrap() => return Some(Ok(S.to_string())),
                Ok(c) if c == N.chars().next().unwrap() => return Some(Ok(N.to_string())),
                Ok(c) if c == T.chars().next().unwrap() => {
                    for tc in T.chars().skip(1) {
                        match self.buffer.read_char() {
                            Ok(c) => {
                                if c != tc {
                                    continue 'outer;
                                }
                            }
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok(T.to_string()));
                }
                Ok(_) => continue,
                Err(err) if err.kind() == ErrorKind::UnexpectedEof => return None,
                Err(e) => return Some(Err(e)),
            }
        }
    }
}

fn scan<'r, B: BufRead>(buffer: &'r mut B) -> Scan<'r, B> {
    Scan { buffer: buffer }
}

/// Compiler and Decompiler for DT.
pub struct DT;

impl DT {
    /// Create a new `DT`.
    pub fn new() -> DT {
        DT
    }

    #[inline]
    fn write<W: Write>(&self, output: &mut W, inst: &[&'static str]) -> io::Result<()> {
        write!(output, "{}", inst.concat())
    }

    #[inline]
    fn write_num<W: Write>(&self, output: &mut W, cmd: &[&'static str], n: i64) -> io::Result<()> {
        let (flag, value) = if n < 0 { (T, n * -1) } else { (S, n) };
        write!(
            output,
            "{}{}{}{}",
            cmd.concat(),
            flag,
            format!("{:b}", value).replace("0", S).replace("1", T),
            N
        )
    }
}

impl Compiler for DT {
    fn compile<B: BufRead, W: ByteCodeWriter>(
        &self,
        input: &mut B,
        output: &mut W,
    ) -> io::Result<()> {
        let mut it = scan(input).tokenize().parse();
        output.assemble(&mut it)
    }
}

impl Decompiler for DT {
    fn decompile<R: ByteCodeReader, W: Write>(
        &self,
        input: &mut R,
        output: &mut W,
    ) -> io::Result<()> {
        for inst in input.disassemble() {
            match inst {
                Ok(ir::StackPush(n)) => self.write_num(output, &[S, S], n),
                Ok(ir::StackDuplicate) => self.write(output, &[S, N, S]),
                Ok(ir::StackCopy(n)) => self.write_num(output, &[S, T, S], n),
                Ok(ir::StackSwap) => self.write(output, &[S, N, T]),
                Ok(ir::StackDiscard) => self.write(output, &[S, N, N]),
                Ok(ir::StackSlide(n)) => self.write_num(output, &[S, T, N], n),
                Ok(ir::Addition) => self.write(output, &[T, S, S, S]),
                Ok(ir::Subtraction) => self.write(output, &[T, S, S, T]),
                Ok(ir::Multiplication) => self.write(output, &[T, S, S, N]),
                Ok(ir::Division) => self.write(output, &[T, S, T, S]),
                Ok(ir::Modulo) => self.write(output, &[T, S, T, T]),
                Ok(ir::HeapStore) => self.write(output, &[T, T, S]),
                Ok(ir::HeapRetrieve) => self.write(output, &[T, T, T]),
                Ok(ir::Mark(n)) => self.write_num(output, &[N, S, S], n),
                Ok(ir::Call(n)) => self.write_num(output, &[N, S, T], n),
                Ok(ir::Jump(n)) => self.write_num(output, &[N, S, N], n),
                Ok(ir::JumpIfZero(n)) => self.write_num(output, &[N, T, S], n),
                Ok(ir::JumpIfNegative(n)) => self.write_num(output, &[N, T, T], n),
                Ok(ir::Return) => self.write(output, &[N, T, N]),
                Ok(ir::Exit) => self.write(output, &[N, N, N]),
                Ok(ir::PutCharactor) => self.write(output, &[T, N, S, S]),
                Ok(ir::PutNumber) => self.write(output, &[T, N, S, T]),
                Ok(ir::GetCharactor) => self.write(output, &[T, N, T, S]),
                Ok(ir::GetNumber) => self.write(output, &[T, N, T, T]),
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
    use crate::syntax::whitespace::{Space, Tab, LF};
    use crate::syntax::Decompiler;

    const S: &'static str = "ど";
    const T: &'static str = "童貞ちゃうわっ！";
    const N: &'static str = "…";

    #[test]
    fn test_scan() {
        let source = vec![S, "童貞饂飩ちゃうわっ！", T, "\n", N].concat();
        let mut buffer = Cursor::new(source.as_bytes());
        let it = super::scan(&mut buffer);
        let expected = &[S, T, N];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_tokenize() {
        let source = vec![S, "童貞饂飩ちゃうわっ！", T, "\n", N].concat();
        let mut buffer = Cursor::new(source.as_bytes());
        let it = super::scan(&mut buffer).tokenize();
        let expected = &[Space, Tab, LF];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }

    #[test]
    fn test_generate() {
        let mut bc2 = Cursor::new(Vec::new());
        {
            let mut bc = Cursor::new(Vec::new());
            bc.write_push(-1).unwrap();
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
            let syntax = super::DT::new();
            syntax.decompile(&mut bc, &mut bc2).unwrap();
        }
        let result = from_utf8(bc2.get_ref()).unwrap();
        let expected = vec![
            "どど童貞ちゃうわっ！童貞ちゃうわっ！…",
            "ど…ど",
            "ど童貞ちゃうわっ！どど童貞ちゃうわっ！ど…",
            "ど…童貞ちゃうわっ！",
            "ど……",
            "ど童貞ちゃうわっ！…ど童貞ちゃうわっ！童貞ちゃうわっ！…",
            "童貞ちゃうわっ！どどど",
            "童貞ちゃうわっ！どど童貞ちゃうわっ！",
            "童貞ちゃうわっ！どど…",
            "童貞ちゃうわっ！ど童貞ちゃうわっ！ど",
            "童貞ちゃうわっ！ど童貞ちゃうわっ！童貞ちゃうわっ！",
            "童貞ちゃうわっ！童貞ちゃうわっ！ど",
            "童貞ちゃうわっ！童貞ちゃうわっ！童貞ちゃうわっ！",
            "…どどど童貞ちゃうわっ！…",
            "…ど童貞ちゃうわっ！ど童貞ちゃうわっ！…",
            "…ど…ど童貞ちゃうわっ！…",
            "…童貞ちゃうわっ！どど童貞ちゃうわっ！…",
            "…童貞ちゃうわっ！童貞ちゃうわっ！ど童貞ちゃうわっ！…",
            "…童貞ちゃうわっ！…",
            "………",
            "童貞ちゃうわっ！…どど",
            "童貞ちゃうわっ！…ど童貞ちゃうわっ！",
            "童貞ちゃうわっ！…童貞ちゃうわっ！ど",
            "童貞ちゃうわっ！…童貞ちゃうわっ！童貞ちゃうわっ！",
        ]
        .concat();
        assert_eq!(result, expected);
    }
}
