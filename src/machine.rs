//! A virtual machine that execute Whitebase bytecode.

use std::collections::{BTreeMap, HashMap};
use std::convert::{TryFrom, TryInto};
use std::io::{self, stdin, stdout, BufRead, BufReader, ErrorKind, SeekFrom, Stdin, Stdout, Write};

use log::debug;

use crate::bytecode;
use crate::bytecode::ByteCodeReader;
use crate::io::BufReadExt;

pub type MachineResult<T> = Result<T, MachineError>;

/// A list specifying VM error.
#[derive(Debug)]
pub enum MachineError {
    /// Empty stack poped.
    IllegalStackManipulation,
    /// Tried to jump unmarked position.
    UndefinedLabel,
    /// Divide by zero.
    ZeroDivision,
    /// "RETURN" instruction was executed without "CALL".
    CallStackEmpty,
    /// Program includes no "EXIT" instruction.
    MissingExitInstruction,
    /// I/O error occurred.
    MachineIoError(io::Error),
    /// Any runtime error not part of this list.
    OtherMachineError,
}

use self::MachineError::*;

/// A virtual machine.
pub struct Machine<B, W> {
    stack: Vec<i64>,
    heap: BTreeMap<i64, i64>,
    stdin: B,
    stdout: W,
}

/// Create a new `Machine` with stdin and stdout.
pub fn with_stdio() -> Machine<BufReader<Stdin>, Stdout> {
    Machine::new(BufReader::new(stdin()), stdout())
}

impl<B: BufRead, W: Write> Machine<B, W> {
    /// Creates a new `Machine` with input and output.
    pub fn new(stdin: B, stdout: W) -> Machine<B, W> {
        Machine {
            stack: Vec::new(),
            heap: BTreeMap::new(),
            stdin,
            stdout,
        }
    }

    /// Run program.
    pub fn run<R: ByteCodeReader>(&mut self, program: &mut R) -> MachineResult<()> {
        let mut index = HashMap::new();
        let mut caller = vec![];
        loop {
            match self.step(program, &mut index, &mut caller) {
                Err(e) => return Err(e),
                Ok(false) => return Ok(()),
                Ok(true) => continue,
            }
        }
    }

    fn step<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        index: &mut HashMap<i64, u64>,
        caller: &mut Vec<u64>,
    ) -> MachineResult<bool> {
        let cmd = program.read_inst().ok_or(MissingExitInstruction)?;
        match cmd {
            Ok((bytecode::CMD_PUSH, n)) => {
                debug!("PUSH {}", n);
                self.push(n)?;
                Ok(true)
            }
            Ok((bytecode::CMD_DUP, _)) => {
                debug!("DUP");
                self.copy(0)?;
                Ok(true)
            }
            Ok((bytecode::CMD_COPY, n)) => {
                debug!("COPY {}", n);
                self.copy(n.try_into().unwrap())?;
                Ok(true)
            }
            Ok((bytecode::CMD_SWAP, _)) => {
                debug!("SWAP");
                self.swap()?;
                Ok(true)
            }
            Ok((bytecode::CMD_DISCARD, _)) => {
                debug!("DISCARD");
                self.discard()?;
                Ok(true)
            }
            Ok((bytecode::CMD_SLIDE, n)) => {
                debug!("SLIDE {}", n);
                self.slide(n.try_into().unwrap())?;
                Ok(true)
            }
            Ok((bytecode::CMD_ADD, _)) => {
                debug!("ADD");
                self.calc(|x, y| y + x)?;
                Ok(true)
            }
            Ok((bytecode::CMD_SUB, _)) => {
                debug!("SUB");
                self.calc(|x, y| y - x)?;
                Ok(true)
            }
            Ok((bytecode::CMD_MUL, _)) => {
                debug!("MUL");
                self.calc(|x, y| y * x)?;
                Ok(true)
            }
            Ok((bytecode::CMD_DIV, _)) => {
                debug!("DIV");
                self.dcalc(|x, y| y / x)?;
                Ok(true)
            }
            Ok((bytecode::CMD_MOD, _)) => {
                debug!("MOD");
                self.dcalc(|x, y| y % x)?;
                Ok(true)
            }
            Ok((bytecode::CMD_STORE, _)) => {
                debug!("STORE");
                self.store()?;
                Ok(true)
            }
            Ok((bytecode::CMD_RETRIEVE, _)) => {
                debug!("RETRIEVE");
                self.retrieve()?;
                Ok(true)
            }
            Ok((bytecode::CMD_MARK, n)) => {
                debug!("MARK {}", n);
                self.mark(program, index, n)?;
                Ok(true)
            }
            Ok((bytecode::CMD_CALL, n)) => {
                debug!("CALL {}", n);
                self.call(program, index, caller, &n)?;
                Ok(true)
            }
            Ok((bytecode::CMD_JUMP, n)) => {
                debug!("JUMP {}", n);
                self.jump(program, index, &n)?;
                Ok(true)
            }
            Ok((bytecode::CMD_JUMPZ, n)) => {
                debug!("JUMPZ {}", n);
                self.jump_if(program, index, &n, |x| x == 0)?;
                Ok(true)
            }
            Ok((bytecode::CMD_JUMPN, n)) => {
                debug!("JUMPN {}", n);
                self.jump_if(program, index, &n, |x| x < 0)?;
                Ok(true)
            }
            Ok((bytecode::CMD_RETURN, _)) => {
                debug!("RETURN");
                self.do_return(program, caller)?;
                Ok(true)
            }
            Ok((bytecode::CMD_EXIT, _)) => {
                debug!("EXIT ({:?}, {:?})", self.stack, self.heap);
                Ok(false)
            }
            Ok((bytecode::CMD_PUTC, _)) => {
                debug!("PUTC");
                self.put_char()?;
                Ok(true)
            }
            Ok((bytecode::CMD_PUTN, _)) => {
                debug!("PUTN");
                self.put_num()?;
                Ok(true)
            }
            Ok((bytecode::CMD_GETC, _)) => {
                debug!("GETC");
                self.get_char()?;
                Ok(true)
            }
            Ok((bytecode::CMD_GETN, _)) => {
                debug!("GETN");
                self.get_num()?;
                Ok(true)
            }
            Err(e) => Err(MachineIoError(e)),
            _ => Err(OtherMachineError),
        }
    }

    fn push(&mut self, n: i64) -> MachineResult<()> {
        self.stack.push(n);
        Ok(())
    }

    fn copy(&mut self, n: usize) -> MachineResult<()> {
        if self.stack.len() <= n {
            return Err(IllegalStackManipulation);
        }
        self.stack.push(self.stack[self.stack.len() - n - 1]);
        Ok(())
    }

    fn swap(&mut self) -> MachineResult<()> {
        let len = self.stack.len();
        if len < 2 {
            return Err(IllegalStackManipulation);
        }
        self.stack.swap(len - 2, len - 1);
        Ok(())
    }

    fn discard(&mut self) -> MachineResult<()> {
        self.stack.pop().ok_or(IllegalStackManipulation)?;
        Ok(())
    }

    fn slide(&mut self, n: usize) -> MachineResult<()> {
        if self.stack.len() < n {
            return Err(IllegalStackManipulation);
        }
        let top = self.stack.pop().unwrap();
        self.stack.truncate(self.stack.len() - n);
        self.stack.push(top);
        Ok(())
    }

    fn calc(&mut self, f: impl FnOnce(i64, i64) -> i64) -> MachineResult<()> {
        let x = self.stack.pop().ok_or(IllegalStackManipulation)?;
        let y = self.stack.last_mut().ok_or(IllegalStackManipulation)?;
        *y = f(x, *y);
        Ok(())
    }

    fn dcalc(&mut self, divf: impl FnOnce(i64, i64) -> i64) -> MachineResult<()> {
        let x = self.stack.pop().ok_or(IllegalStackManipulation)?;
        if x == 0 {
            return Err(ZeroDivision);
        }
        let y = self.stack.last_mut().ok_or(IllegalStackManipulation)?;
        *y = divf(x, *y);
        Ok(())
    }

    fn store(&mut self) -> MachineResult<()> {
        let val = self.stack.pop().ok_or(IllegalStackManipulation)?;
        let addr = self.stack.pop().ok_or(IllegalStackManipulation)?;
        self.heap.insert(addr, val);
        Ok(())
    }

    fn retrieve(&mut self) -> MachineResult<()> {
        let addr = self.stack.pop().ok_or(IllegalStackManipulation)?;
        self.stack.push(match self.heap.get(&addr) {
            Some(val) => *val,
            None => 0,
        });
        Ok(())
    }

    fn mark<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        index: &mut HashMap<i64, u64>,
        label: i64,
    ) -> MachineResult<()> {
        match program.stream_position() {
            Ok(pos) => {
                index.insert(label, pos);
                Ok(())
            }
            Err(err) => Err(MachineIoError(err)),
        }
    }

    fn call<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        index: &mut HashMap<i64, u64>,
        caller: &mut Vec<u64>,
        label: &i64,
    ) -> MachineResult<()> {
        match program.stream_position() {
            Ok(pos) => {
                caller.push(pos);
                self.jump(program, index, label)
            }
            Err(err) => Err(MachineIoError(err)),
        }
    }

    fn jump<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        index: &mut HashMap<i64, u64>,
        label: &i64,
    ) -> MachineResult<()> {
        match index.get(label) {
            Some(&pos) => match program.seek(SeekFrom::Start(pos)) {
                Ok(_) => Ok(()),
                Err(err) => Err(MachineIoError(err)),
            },
            None => loop {
                match program.read_inst() {
                    Some(Ok((opcode, operand))) if opcode == bytecode::CMD_MARK => {
                        match program.stream_position() {
                            Ok(pos) => {
                                index.insert(operand, pos);
                                if operand == *label {
                                    return Ok(());
                                }
                            }
                            Err(err) => return Err(MachineIoError(err)),
                        }
                    }
                    Some(Err(err)) => return Err(MachineIoError(err)),
                    None => return Err(UndefinedLabel),
                    _ => continue,
                }
            },
        }
    }

    fn jump_if<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        index: &mut HashMap<i64, u64>,
        label: &i64,
        test: impl FnOnce(i64) -> bool,
    ) -> MachineResult<()> {
        match self.stack.pop() {
            Some(x) if test(x) => self.jump(program, index, label),
            None => Err(IllegalStackManipulation),
            _ => Ok(()),
        }
    }

    fn do_return<R: ByteCodeReader>(
        &mut self,
        program: &mut R,
        caller: &mut Vec<u64>,
    ) -> MachineResult<()> {
        match caller.pop() {
            Some(to_return) => match program.seek(SeekFrom::Start(to_return)) {
                Ok(_) => Ok(()),
                Err(err) => Err(MachineIoError(err)),
            },
            None => Err(CallStackEmpty),
        }
    }

    fn put_char(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            Some(n) if n >= 0 => {
                match write!(self.stdout, "{}", u8::try_from(n).unwrap() as char) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(MachineIoError(e)),
                }
            }
            Some(_) => Err(IllegalStackManipulation),
            None => Err(IllegalStackManipulation),
        }
    }

    fn put_num(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            Some(n) => match write!(self.stdout, "{}", n) {
                Ok(_) => Ok(()),
                Err(e) => Err(MachineIoError(e)),
            },
            None => Err(IllegalStackManipulation),
        }
    }

    fn get_char(&mut self) -> MachineResult<()> {
        match self.stdin.read_char() {
            Ok(c) => {
                self.stack.push(c as i64);
                self.store()?;
                Ok(())
            }
            Err(err) => Err(MachineIoError(err)),
        }
    }

    fn get_num(&mut self) -> MachineResult<()> {
        let mut line = String::new();
        self.stdin.read_line(&mut line).map_err(MachineIoError)?;
        match line.replace("\n", "").parse() {
            Ok(n) => {
                self.stack.push(n);
                self.store()?;
                Ok(())
            }
            Err(err) => Err(MachineIoError(io::Error::new(ErrorKind::InvalidInput, err))),
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::io::{self, Cursor, Seek, SeekFrom};

    use crate::bytecode::ByteCodeWriter;

    #[test]
    fn test_stack() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_push(1).unwrap();
        bc.write_dup().unwrap();
        bc.write_copy(1).unwrap();
        bc.write_swap().unwrap();
        bc.write_discard().unwrap();
        bc.write_slide(1).unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        let mut vm = super::Machine::new(io::empty(), io::sink());
        let mut caller = vec![];
        let mut index = HashMap::new();
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1, 1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1, 1, 1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1, 1, 1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1, 1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1));
        assert!(vm.step(&mut bc, &mut index, &mut caller).is_err());
    }

    #[test]
    fn test_arithmetic() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_add().unwrap();
        bc.write_sub().unwrap();
        bc.write_mul().unwrap();
        bc.write_div().unwrap();
        bc.write_mod().unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        let mut vm = super::Machine::new(io::empty(), io::sink());
        let mut caller = vec![];
        let mut index = HashMap::new();
        vm.stack.extend_from_slice(&[2, 19, 2, 5, 1, 1]);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2, 19, 2, 5, 2));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2, 19, 2, 3));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2, 19, 6));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2, 3));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2));
        assert!(vm.step(&mut bc, &mut index, &mut caller).is_err());
    }

    #[test]
    fn test_heap() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_store().unwrap();
        bc.write_retrieve().unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        let mut vm = super::Machine::new(io::empty(), io::sink());
        let mut caller = vec![];
        let mut index = HashMap::new();
        vm.stack.extend_from_slice(&[1, 1, 2]);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1));
        assert_eq!(vm.heap.get(&1), Some(&2));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(2));
        assert!(vm.step(&mut bc, &mut index, &mut caller).is_err());
    }

    #[test]
    fn test_flow() {
        let mut bc = Cursor::new(Vec::new());
        bc.write_jump(1).unwrap();
        bc.write_mark(3).unwrap();
        bc.write_call(4).unwrap();
        bc.write_exit().unwrap();
        bc.write_mark(2).unwrap();
        bc.write_jumpn(3).unwrap();
        bc.write_mark(1).unwrap();
        bc.write_jumpz(2).unwrap();
        bc.write_mark(4).unwrap();
        bc.write_return().unwrap();

        bc.seek(SeekFrom::Start(0)).unwrap();
        let mut vm = super::Machine::new(io::empty(), io::sink());
        let mut caller = vec![];
        let mut index = HashMap::new();
        vm.stack.extend_from_slice(&[-1, 0]);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(-1, 0));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(-1));
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!());
        assert_eq!(caller.len(), 0);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(caller.len(), 1);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(caller.len(), 0);
        assert_eq!(vm.step(&mut bc, &mut index, &mut caller).unwrap(), false);
    }

    #[test]
    fn test_io() {
        let mut heap = [0, 0];
        let mut buf = [0; 2];
        {
            let mut bc = Cursor::new(Vec::new());
            bc.write_getc().unwrap();
            bc.write_getn().unwrap();
            bc.write_putc().unwrap();
            bc.write_putn().unwrap();
            bc.seek(SeekFrom::Start(0)).unwrap();
            let input = Cursor::new(vec![87, 49, 50, 51, 10]);
            let output = Cursor::new(&mut buf[..]);
            let mut vm = super::Machine::new(input, output);
            let mut caller = vec![];
            let mut index = HashMap::new();
            vm.stack.extend_from_slice(&[5, 66, 2, 1]);
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            assert!(vm.step(&mut bc, &mut index, &mut caller).is_err());

            heap[0] = *vm.heap.get(&1).unwrap();
            heap[1] = *vm.heap.get(&2).unwrap();
        }
        assert_eq!(heap, [87, 123]);
        assert_eq!(buf, [66, 53]);
    }
}
