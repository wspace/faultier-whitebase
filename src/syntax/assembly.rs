//! Assembler and Disassembler.

use std::io::{self, BufRead, EndOfFile, ErrorKind, Write};

use bytecode;
use bytecode::{ByteCodeReader, ByteCodeWriter};
use syntax::{Compiler, Decompiler};

macro_rules! try_number(
    ($val:expr) => (match from_str($val) {
        Some(n) => n,
        None => return Err(io::Error::new(
            ErrorKind::InvalidInput,
            format!("invalid value format: expected number, but {}", $val),
        )),
    })
);

/// Assembler and Disassembler.
pub struct Assembly;

impl Assembly {
    /// Create a new `Assembly`.
    pub fn new() -> Assembly {
        Assembly
    }
}

impl Compiler for Assembly {
    fn compile<B: BufRead, W: ByteCodeWriter>(
        &self,
        input: &mut B,
        output: &mut W,
    ) -> io::Result<()> {
        loop {
            let ret = match input.read_line() {
                Ok(line) => {
                    let inst = line.replace("\n", "");
                    if inst.len() == 0 {
                        continue;
                    }
                    if inst.char_at(0) == ';' {
                        continue;
                    }
                    let (mnemonic, val) = match inst.find(' ') {
                        Some(n) => (inst.slice_to(n), inst.slice_from(n + 1)),
                        None => (inst.as_str(), ""),
                    };
                    match mnemonic {
                        "PUSH" => output.write_push(try_number!(val)),
                        "DUP" => output.write_dup(),
                        "COPY" => output.write_copy(try_number!(val)),
                        "SWAP" => output.write_swap(),
                        "DISCARD" => output.write_discard(),
                        "SLIDE" => output.write_slide(try_number!(val)),
                        "ADD" => output.write_add(),
                        "SUB" => output.write_sub(),
                        "MUL" => output.write_mul(),
                        "DIV" => output.write_div(),
                        "MOD" => output.write_mod(),
                        "STORE" => output.write_store(),
                        "RETRIEVE" => output.write_retrieve(),
                        "MARK" => output.write_mark(try_number!(val)),
                        "CALL" => output.write_call(try_number!(val)),
                        "JUMP" => output.write_jump(try_number!(val)),
                        "JUMPZ" => output.write_jumpz(try_number!(val)),
                        "JUMPN" => output.write_jumpn(try_number!(val)),
                        "RETURN" => output.write_return(),
                        "EXIT" => output.write_exit(),
                        "PUTC" => output.write_putc(),
                        "PUTN" => output.write_putn(),
                        "GETC" => output.write_getc(),
                        "GETN" => output.write_getn(),
                        _ => Err(ErrorKind::InvalidInput.into()),
                    }
                }
                Err(e) => Err(e),
            };

            match ret {
                Ok(()) => continue,
                Err(ref e) if e.kind == EndOfFile => break,
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
}

impl Decompiler for Assembly {
    fn decompile<R: ByteCodeReader, W: Write>(
        &self,
        input: &mut R,
        output: &mut W,
    ) -> io::Result<()> {
        loop {
            let res = match input.read_inst() {
                Ok((bytecode::CMD_PUSH, n)) => writeln!(output, "PUSH {}", n),
                Ok((bytecode::CMD_DUP, _)) => writeln!(output, "DUP"),
                Ok((bytecode::CMD_COPY, n)) => writeln!(output, "COPY {}", n),
                Ok((bytecode::CMD_SWAP, _)) => writeln!(output, "SWAP"),
                Ok((bytecode::CMD_DISCARD, _)) => writeln!(output, "DISCARD"),
                Ok((bytecode::CMD_SLIDE, n)) => writeln!(output, "SLIDE {}", n),
                Ok((bytecode::CMD_ADD, _)) => writeln!(output, "ADD"),
                Ok((bytecode::CMD_SUB, _)) => writeln!(output, "SUB"),
                Ok((bytecode::CMD_MUL, _)) => writeln!(output, "MUL"),
                Ok((bytecode::CMD_DIV, _)) => writeln!(output, "DIV"),
                Ok((bytecode::CMD_MOD, _)) => writeln!(output, "MOD"),
                Ok((bytecode::CMD_STORE, _)) => writeln!(output, "STORE"),
                Ok((bytecode::CMD_RETRIEVE, _)) => writeln!(output, "RETRIEVE"),
                Ok((bytecode::CMD_MARK, n)) => writeln!(output, "MARK {}", n),
                Ok((bytecode::CMD_CALL, n)) => writeln!(output, "CALL {}", n),
                Ok((bytecode::CMD_JUMP, n)) => writeln!(output, "JUMP {}", n),
                Ok((bytecode::CMD_JUMPZ, n)) => writeln!(output, "JUMPZ {}", n),
                Ok((bytecode::CMD_JUMPN, n)) => writeln!(output, "JUMPN {}", n),
                Ok((bytecode::CMD_RETURN, _)) => writeln!(output, "RETURN"),
                Ok((bytecode::CMD_EXIT, _)) => writeln!(output, "EXIT"),
                Ok((bytecode::CMD_PUTC, _)) => writeln!(output, "PUTC"),
                Ok((bytecode::CMD_PUTN, _)) => writeln!(output, "PUTN"),
                Ok((bytecode::CMD_GETC, _)) => writeln!(output, "GETC"),
                Ok((bytecode::CMD_GETN, _)) => writeln!(output, "GETN"),
                Ok(_) => Err(ErrorKind::InvalidInput.into()),
                Err(e) => Err(e),
            };
            match res {
                Err(ref e) if e.kind == EndOfFile => break,
                Err(e) => return Err(e),
                _ => continue,
            };
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::io::{Cursor, Seek, SeekFrom};
    use std::str::from_utf8;

    use bytecode;
    use bytecode::{ByteCodeReader, ByteCodeWriter};
    use syntax::{Compiler, Decompiler};

    #[test]
    fn test_assemble() {
        let source = vec![
            "PUSH 1", "DUP", "COPY 2", "SWAP", "DISCARD", "SLIDE 3", "ADD", "SUB", "MUL", "DIV",
            "MOD", "STORE", "RETRIEVE", "MARK 4", "CALL 5", "JUMP 6", "JUMPZ 7", "JUMPN 8",
            "RETURN", "EXIT", "PUTC", "PUTN", "GETC", "GETN",
        ]
        .connect("\n");
        let mut bc = Cursor::new(Vec::new());
        {
            let syntax = super::Assembly::new();
            let mut buffer = Cursor::new(source.as_bytes());
            syntax.compile(&mut buffer, &mut bc).unwrap();
        }
        bc.seek(SeekFrom::Start(0)).unwrap();
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_PUSH, 1)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_DUP, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_COPY, 2)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_SWAP, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_DISCARD, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_SLIDE, 3)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_ADD, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_SUB, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_MUL, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_DIV, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_MOD, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_STORE, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_RETRIEVE, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_MARK, 4)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_CALL, 5)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_JUMP, 6)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_JUMPZ, 7)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_JUMPN, 8)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_RETURN, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_EXIT, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_PUTC, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_PUTN, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_GETC, 0)));
        assert_eq!(bc.read_inst(), Ok((bytecode::CMD_GETN, 0)));
        assert!(bc.read_inst().is_err());
    }

    #[test]
    fn test_disassemble() {
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
            bc.write_call(15).unwrap();
            bc.write_jump(2).unwrap();
            bc.write_jumpz(16).unwrap();
            bc.write_jumpn(32).unwrap();
            bc.write_return().unwrap();
            bc.write_exit().unwrap();
            bc.write_putc().unwrap();
            bc.write_putn().unwrap();
            bc.write_getc().unwrap();
            bc.write_getn().unwrap();
            bc.seek(SeekFrom::Start(0)).unwrap();
            let syntax = super::Assembly::new();
            syntax.decompile(&mut bc, &mut bc2).unwrap();
        }
        let result = from_utf8(bc2.get_ref()).unwrap();
        let expected = vec![
            "PUSH 1", "DUP", "COPY 2", "SWAP", "DISCARD", "SLIDE 3", "ADD", "SUB", "MUL", "DIV",
            "MOD", "STORE", "RETRIEVE", "MARK 1", "CALL 15", "JUMP 2", "JUMPZ 16", "JUMPN 32",
            "RETURN", "EXIT", "PUTC", "PUTN", "GETC", "GETN", "",
        ]
        .connect("\n");
        assert_eq!(result, expected);
    }
}
