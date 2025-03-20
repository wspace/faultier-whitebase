//! Bytecode utilities.

use std::io::{self, ErrorKind, Read, Seek, Write};

use crate::ir;
use crate::ir::Instruction;

pub const IMP_STACK: u8 = 0b0011 << 4;
pub const IMP_ARITHMETIC: u8 = 0b1000 << 4;
pub const IMP_HEAP: u8 = 0b1010 << 4;
pub const IMP_FLOW: u8 = 0b0111 << 4;
pub const IMP_IO: u8 = 0b1001 << 4;

pub const CMD_PUSH: u8 = IMP_STACK + 0b0011;
pub const CMD_DUP: u8 = IMP_STACK + 0b0100;
pub const CMD_COPY: u8 = IMP_STACK + 0b1000;
pub const CMD_SWAP: u8 = IMP_STACK + 0b0110;
pub const CMD_DISCARD: u8 = IMP_STACK + 0b0101;
pub const CMD_SLIDE: u8 = IMP_STACK + 0b1001;
pub const CMD_ADD: u8 = IMP_ARITHMETIC + 0b0000;
pub const CMD_SUB: u8 = IMP_ARITHMETIC + 0b0010;
pub const CMD_MUL: u8 = IMP_ARITHMETIC + 0b0001;
pub const CMD_DIV: u8 = IMP_ARITHMETIC + 0b1000;
pub const CMD_MOD: u8 = IMP_ARITHMETIC + 0b1010;
pub const CMD_STORE: u8 = IMP_HEAP + 0b0011;
pub const CMD_RETRIEVE: u8 = IMP_HEAP + 0b1011;
pub const CMD_MARK: u8 = IMP_FLOW + 0b0000;
pub const CMD_CALL: u8 = IMP_FLOW + 0b0010;
pub const CMD_JUMP: u8 = IMP_FLOW + 0b0001;
pub const CMD_JUMPZ: u8 = IMP_FLOW + 0b1000;
pub const CMD_JUMPN: u8 = IMP_FLOW + 0b1010;
pub const CMD_RETURN: u8 = IMP_FLOW + 0b1001;
pub const CMD_EXIT: u8 = IMP_FLOW + 0b0101;
pub const CMD_PUTC: u8 = IMP_IO + 0b0000;
pub const CMD_PUTN: u8 = IMP_IO + 0b0010;
pub const CMD_GETC: u8 = IMP_IO + 0b1000;
pub const CMD_GETN: u8 = IMP_IO + 0b1010;

/// Bytecodes writer.
pub trait ByteCodeWriter {
    /// Compile a instruction to bytecodes.
    fn assemble<I: Iterator<Item = io::Result<Instruction>>>(
        &mut self,
        iter: &mut I,
    ) -> io::Result<()>;
    /// Writes a push instruction.
    fn write_push(&mut self, n: i64) -> io::Result<()>;
    /// Writes a duplicate instruction.
    fn write_dup(&mut self) -> io::Result<()>;
    /// Writes a copy instruction.
    fn write_copy(&mut self, n: i64) -> io::Result<()>;
    /// Writes a swap instruction.
    fn write_swap(&mut self) -> io::Result<()>;
    /// Writes a discard instruction.
    fn write_discard(&mut self) -> io::Result<()>;
    /// Writes a slide instruction.
    fn write_slide(&mut self, n: i64) -> io::Result<()>;
    /// Writes a addition instruction.
    fn write_add(&mut self) -> io::Result<()>;
    /// Writes a subtraction instruction.
    fn write_sub(&mut self) -> io::Result<()>;
    /// Writes a multiplication instruction.
    fn write_mul(&mut self) -> io::Result<()>;
    /// Writes a division instruction.
    fn write_div(&mut self) -> io::Result<()>;
    /// Writes a modulo instruction.
    fn write_mod(&mut self) -> io::Result<()>;
    /// Writes a store instruction.
    fn write_store(&mut self) -> io::Result<()>;
    /// Writes a retrieve instruction.
    fn write_retrieve(&mut self) -> io::Result<()>;
    /// Writes a mark instruction.
    fn write_mark(&mut self, n: i64) -> io::Result<()>;
    /// Writes a call instruction.
    fn write_call(&mut self, n: i64) -> io::Result<()>;
    /// Writes a jump instruction.
    fn write_jump(&mut self, n: i64) -> io::Result<()>;
    /// Writes a conditional jump instruction.
    fn write_jumpz(&mut self, n: i64) -> io::Result<()>;
    /// Writes a conditional jump instruction.
    fn write_jumpn(&mut self, n: i64) -> io::Result<()>;
    /// Writes a return instruction.
    fn write_return(&mut self) -> io::Result<()>;
    /// Writes a exit instruction.
    fn write_exit(&mut self) -> io::Result<()>;
    /// Writes a character put instruction.
    fn write_putc(&mut self) -> io::Result<()>;
    /// Writes a number put instruction.
    fn write_putn(&mut self) -> io::Result<()>;
    /// Writes a character get instruction.
    fn write_getc(&mut self) -> io::Result<()>;
    /// Writes a number get instruction.
    fn write_getn(&mut self) -> io::Result<()>;
}

impl<W: Write> ByteCodeWriter for W {
    fn assemble<I: Iterator<Item = io::Result<Instruction>>>(
        &mut self,
        iter: &mut I,
    ) -> io::Result<()> {
        for inst in iter {
            match inst {
                Ok(ir::StackPush(n)) => self.write_push(n),
                Ok(ir::StackDuplicate) => self.write_dup(),
                Ok(ir::StackCopy(n)) => self.write_copy(n),
                Ok(ir::StackSwap) => self.write_swap(),
                Ok(ir::StackDiscard) => self.write_discard(),
                Ok(ir::StackSlide(n)) => self.write_slide(n),
                Ok(ir::Addition) => self.write_add(),
                Ok(ir::Subtraction) => self.write_sub(),
                Ok(ir::Multiplication) => self.write_mul(),
                Ok(ir::Division) => self.write_div(),
                Ok(ir::Modulo) => self.write_mod(),
                Ok(ir::HeapStore) => self.write_store(),
                Ok(ir::HeapRetrieve) => self.write_retrieve(),
                Ok(ir::Mark(n)) => self.write_mark(n),
                Ok(ir::Call(n)) => self.write_call(n),
                Ok(ir::Jump(n)) => self.write_jump(n),
                Ok(ir::JumpIfZero(n)) => self.write_jumpz(n),
                Ok(ir::JumpIfNegative(n)) => self.write_jumpn(n),
                Ok(ir::Return) => self.write_return(),
                Ok(ir::Exit) => self.write_exit(),
                Ok(ir::PutCharactor) => self.write_putc(),
                Ok(ir::PutNumber) => self.write_putn(),
                Ok(ir::GetCharactor) => self.write_getc(),
                Ok(ir::GetNumber) => self.write_getn(),
                Err(e) => Err(e),
            }?;
        }
        Ok(())
    }

    fn write_push(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_PUSH, n)
    }

    fn write_dup(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_DUP)
    }

    fn write_copy(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_COPY, n)
    }

    fn write_swap(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_SWAP)
    }

    fn write_discard(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_DISCARD)
    }

    fn write_slide(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_SLIDE, n)
    }

    fn write_add(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_ADD)
    }

    fn write_sub(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_SUB)
    }

    fn write_mul(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_MUL)
    }

    fn write_div(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_DIV)
    }

    fn write_mod(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_MOD)
    }

    fn write_store(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_STORE)
    }

    fn write_retrieve(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_RETRIEVE)
    }

    fn write_mark(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_MARK, n)
    }

    fn write_call(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_CALL, n)
    }

    fn write_jump(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_JUMP, n)
    }

    fn write_jumpz(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_JUMPZ, n)
    }

    fn write_jumpn(&mut self, n: i64) -> io::Result<()> {
        write_cmd_arg(self, CMD_JUMPN, n)
    }

    fn write_return(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_RETURN)
    }

    fn write_exit(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_EXIT)
    }

    fn write_putn(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_PUTN)
    }

    fn write_putc(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_PUTC)
    }

    fn write_getc(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_GETC)
    }

    fn write_getn(&mut self) -> io::Result<()> {
        write_cmd(self, CMD_GETN)
    }
}

fn write_cmd<W: Write>(w: &mut W, cmd: u8) -> io::Result<()> {
    w.write_all(&[cmd])
}

fn write_cmd_arg<W: Write>(w: &mut W, cmd: u8, arg: i64) -> io::Result<()> {
    w.write_all(&[cmd])?;
    w.write_all(&arg.to_be_bytes())
}

/// An iterator that convert to IR from bytes on each iteration, until `read_inst()` encounters EOF.
pub struct Instructions<'r, T: ?Sized> {
    reader: &'r mut T,
}

impl<'r, B: ByteCodeReader> Iterator for Instructions<'r, B> {
    type Item = io::Result<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read_inst()? {
            Ok((CMD_PUSH, n)) => Some(Ok(ir::StackPush(n))),
            Ok((CMD_DUP, _)) => Some(Ok(ir::StackDuplicate)),
            Ok((CMD_COPY, n)) => Some(Ok(ir::StackCopy(n))),
            Ok((CMD_SWAP, _)) => Some(Ok(ir::StackSwap)),
            Ok((CMD_DISCARD, _)) => Some(Ok(ir::StackDiscard)),
            Ok((CMD_SLIDE, n)) => Some(Ok(ir::StackSlide(n))),
            Ok((CMD_ADD, _)) => Some(Ok(ir::Addition)),
            Ok((CMD_SUB, _)) => Some(Ok(ir::Subtraction)),
            Ok((CMD_MUL, _)) => Some(Ok(ir::Multiplication)),
            Ok((CMD_DIV, _)) => Some(Ok(ir::Division)),
            Ok((CMD_MOD, _)) => Some(Ok(ir::Modulo)),
            Ok((CMD_STORE, _)) => Some(Ok(ir::HeapStore)),
            Ok((CMD_RETRIEVE, _)) => Some(Ok(ir::HeapRetrieve)),
            Ok((CMD_MARK, n)) => Some(Ok(ir::Mark(n))),
            Ok((CMD_CALL, n)) => Some(Ok(ir::Call(n))),
            Ok((CMD_JUMP, n)) => Some(Ok(ir::Jump(n))),
            Ok((CMD_JUMPZ, n)) => Some(Ok(ir::JumpIfZero(n))),
            Ok((CMD_JUMPN, n)) => Some(Ok(ir::JumpIfNegative(n))),
            Ok((CMD_RETURN, _)) => Some(Ok(ir::Return)),
            Ok((CMD_EXIT, _)) => Some(Ok(ir::Exit)),
            Ok((CMD_PUTC, _)) => Some(Ok(ir::PutCharactor)),
            Ok((CMD_PUTN, _)) => Some(Ok(ir::PutNumber)),
            Ok((CMD_GETC, _)) => Some(Ok(ir::GetCharactor)),
            Ok((CMD_GETN, _)) => Some(Ok(ir::GetNumber)),
            Err(e) => Some(Err(e)),
            _ => Some(Err(ErrorKind::InvalidInput.into())),
        }
    }
}

/// Bytecodes reader.
pub trait ByteCodeReader: Read + Seek {
    /// Read the next instruction bytes from the underlying stream.
    ///
    /// # Error
    ///
    /// If an I/O error occurs, then this function will return `Ok(Err(e))`.
    /// On EOF, it returns `None`.
    fn read_inst(&mut self) -> Option<io::Result<(u8, i64)>>;

    /// Create an iterator that convert to IR from bytes on each iteration
    /// until EOF.
    ///
    /// # Error
    ///
    /// Any error that is produced by the underlying `Read`er is returned by the
    /// iterator and should be handled by the caller.
    fn disassemble<'r>(&'r mut self) -> Instructions<'r, Self> {
        Instructions { reader: self }
    }
}

impl<R: Read + Seek> ByteCodeReader for R {
    fn read_inst(&mut self) -> Option<io::Result<(u8, i64)>> {
        let mut buf = [0; 8];
        match self.read(&mut buf[..1]) {
            Ok(0) => return None,
            Err(e) => return Some(Err(e)),
            _ => {}
        }
        let n = buf[0];
        let arg = match n {
            CMD_PUSH | CMD_COPY | CMD_SLIDE | CMD_MARK | CMD_CALL | CMD_JUMP | CMD_JUMPZ
            | CMD_JUMPN => {
                if let Err(e) = self.read_exact(&mut buf) {
                    return Some(Err(e));
                }
                i64::from_be_bytes(buf)
            }
            _ => 0,
        };
        Some(Ok((n, arg)))
    }
}

#[cfg(test)]
mod test {
    use std::io::{self, Cursor, Seek, SeekFrom};

    use super::{ByteCodeReader, ByteCodeWriter};
    use crate::ir;

    #[test]
    fn test_readwrite() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_push(-1).unwrap();
        bc.write_dup().unwrap();
        bc.write_copy(1).unwrap();
        bc.write_swap().unwrap();
        bc.write_discard().unwrap();
        bc.write_slide(2).unwrap();
        bc.write_add().unwrap();
        bc.write_sub().unwrap();
        bc.write_mul().unwrap();
        bc.write_div().unwrap();
        bc.write_mod().unwrap();
        bc.write_store().unwrap();
        bc.write_retrieve().unwrap();
        bc.write_mark(-1).unwrap();
        bc.write_call(1).unwrap();
        bc.write_jump(-1).unwrap();
        bc.write_jumpz(1).unwrap();
        bc.write_jumpn(-1).unwrap();
        bc.write_return().unwrap();
        bc.write_exit().unwrap();
        bc.write_putc().unwrap();
        bc.write_putn().unwrap();
        bc.write_getc().unwrap();
        bc.write_getn().unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUSH, -1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DUP, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_COPY, 1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SWAP, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DISCARD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SLIDE, 2));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_ADD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SUB, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MUL, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DIV, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MOD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_STORE, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_RETRIEVE, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MARK, -1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_CALL, 1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMP, -1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMPZ, 1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMPN, -1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_RETURN, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_EXIT, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUTC, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUTN, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_GETC, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_GETN, 0));
        assert!(bc.read_inst().is_none());
    }

    #[test]
    fn test_assemble() {
        let mut bc = Cursor::new(Vec::new());
        {
            let vec: Vec<io::Result<ir::Instruction>> = vec![
                Ok(ir::StackPush(1)),
                Ok(ir::StackDuplicate),
                Ok(ir::StackCopy(2)),
                Ok(ir::StackSwap),
                Ok(ir::StackDiscard),
                Ok(ir::StackSlide(3)),
                Ok(ir::Addition),
                Ok(ir::Subtraction),
                Ok(ir::Multiplication),
                Ok(ir::Division),
                Ok(ir::Modulo),
                Ok(ir::HeapStore),
                Ok(ir::HeapRetrieve),
                Ok(ir::Mark(4)),
                Ok(ir::Call(5)),
                Ok(ir::Jump(6)),
                Ok(ir::JumpIfZero(7)),
                Ok(ir::JumpIfNegative(8)),
                Ok(ir::Return),
                Ok(ir::Exit),
                Ok(ir::PutCharactor),
                Ok(ir::PutNumber),
                Ok(ir::GetCharactor),
                Ok(ir::GetNumber),
            ];
            let mut it = vec.into_iter();
            bc.assemble(&mut it).unwrap();
        }
        bc.seek(SeekFrom::Start(0)).unwrap();
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUSH, 1));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DUP, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_COPY, 2));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SWAP, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DISCARD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SLIDE, 3));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_ADD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_SUB, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MUL, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_DIV, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MOD, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_STORE, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_RETRIEVE, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_MARK, 4));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_CALL, 5));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMP, 6));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMPZ, 7));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_JUMPN, 8));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_RETURN, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_EXIT, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUTC, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_PUTN, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_GETC, 0));
        assert_eq!(bc.read_inst().unwrap().unwrap(), (super::CMD_GETN, 0));
        assert!(bc.read_inst().is_none());
    }

    #[test]
    fn test_disassemble() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_push(-1).unwrap();
        bc.write_dup().unwrap();
        bc.write_copy(1).unwrap();
        bc.write_swap().unwrap();
        bc.write_discard().unwrap();
        bc.write_slide(2).unwrap();
        bc.write_add().unwrap();
        bc.write_sub().unwrap();
        bc.write_mul().unwrap();
        bc.write_div().unwrap();
        bc.write_mod().unwrap();
        bc.write_store().unwrap();
        bc.write_retrieve().unwrap();
        bc.write_mark(-1).unwrap();
        bc.write_call(1).unwrap();
        bc.write_jump(-1).unwrap();
        bc.write_jumpz(1).unwrap();
        bc.write_jumpn(-1).unwrap();
        bc.write_return().unwrap();
        bc.write_exit().unwrap();
        bc.write_putc().unwrap();
        bc.write_putn().unwrap();
        bc.write_getc().unwrap();
        bc.write_getn().unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        let it = bc.disassemble();
        let expected = &[
            ir::StackPush(-1),
            ir::StackDuplicate,
            ir::StackCopy(1),
            ir::StackSwap,
            ir::StackDiscard,
            ir::StackSlide(2),
            ir::Addition,
            ir::Subtraction,
            ir::Multiplication,
            ir::Division,
            ir::Modulo,
            ir::HeapStore,
            ir::HeapRetrieve,
            ir::Mark(-1),
            ir::Call(1),
            ir::Jump(-1),
            ir::JumpIfZero(1),
            ir::JumpIfNegative(-1),
            ir::Return,
            ir::Exit,
            ir::PutCharactor,
            ir::PutNumber,
            ir::GetCharactor,
            ir::GetNumber,
        ];
        assert_eq!(it.collect::<io::Result<Vec<_>>>().unwrap(), expected);
    }
}
