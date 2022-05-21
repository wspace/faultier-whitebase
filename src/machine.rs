//! A virtual machine that execute Whitebase bytecode.

use std::collections::{HashMap, TreeMap};
use std::convert::{TryFrom, TryInto};
use std::io::{
    standard_error, stdin, stdout, BufRead, BufReader, EndOfFile, InvalidInput, IoError, SeekFrom,
    Stdin, Stdout, Write,
};

use log::debug;

use bytecode;
use bytecode::ByteCodeReader;

pub type MachineResult<T> = Result<T, MachineError>;

/// A list specifying VM error.
#[derive(PartialEq, Debug)]
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
    MachineIoError(IoError),
    /// Any runtime error not part of this list.
    OtherMachineError,
}

use self::MachineError::*;

/// A virtual machine.
pub struct Machine<B, W> {
    stack: Vec<i64>,
    heap: TreeMap<i64, i64>,
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
            heap: TreeMap::new(),
            stdin: stdin,
            stdout: stdout,
        }
    }

    /// Run program.
    pub fn run(&mut self, program: &mut ByteCodeReader) -> MachineResult<()> {
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

    fn step(
        &mut self,
        program: &mut ByteCodeReader,
        index: &mut HashMap<i64, u64>,
        caller: &mut Vec<u64>,
    ) -> MachineResult<bool> {
        match program.read_inst() {
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
                debug!("SWAP");
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
                debug!("RETREIVE");
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
            Err(ref e) if e.kind == EndOfFile => Err(MissingExitInstruction),
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
        let mut i = 0;
        let mut tmp = vec![];
        while i < n {
            tmp.insert(0, self.stack.pop().unwrap());
            i += 1;
        }
        let val = self.stack.pop().unwrap();
        self.stack.push(val);
        self.stack.push_all(tmp.as_slice());
        self.stack.push(val);
        Ok(())
    }

    fn swap(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            None => Err(IllegalStackManipulation),
            Some(x) => match self.stack.pop() {
                None => Err(IllegalStackManipulation),
                Some(y) => {
                    self.stack.push(x);
                    self.stack.push(y);
                    Ok(())
                }
            },
        }
    }

    fn discard(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            Some(_) => Ok(()),
            None => Err(IllegalStackManipulation),
        }
    }

    fn slide(&mut self, n: usize) -> MachineResult<()> {
        if self.stack.len() < n {
            Err(IllegalStackManipulation)
        } else {
            let top = self.stack.pop().unwrap();
            let mut i = 0;
            while i < n {
                self.stack.pop();
                i += 1;
            }
            self.stack.push(top);
            Ok(())
        }
    }

    fn calc(&mut self, f: impl FnOnce(i64, i64) -> i64) -> MachineResult<()> {
        match self.stack.pop() {
            Some(x) => match self.stack.pop() {
                Some(y) => {
                    self.stack.push(f(x, y));
                    Ok(())
                }
                None => Err(IllegalStackManipulation),
            },
            None => Err(IllegalStackManipulation),
        }
    }

    fn dcalc(&mut self, divf: impl FnOnce(i64, i64) -> i64) -> MachineResult<()> {
        match self.stack.pop() {
            Some(x) if x == 0 => Err(ZeroDivision),
            Some(x) => match self.stack.pop() {
                Some(y) => {
                    self.stack.push(divf(x, y));
                    Ok(())
                }
                None => Err(IllegalStackManipulation),
            },
            None => Err(IllegalStackManipulation),
        }
    }

    fn store(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            Some(val) => match self.stack.pop() {
                Some(addr) => {
                    self.heap.insert(addr, val);
                    Ok(())
                }
                None => Err(IllegalStackManipulation),
            },
            None => Err(IllegalStackManipulation),
        }
    }

    fn retrieve(&mut self) -> MachineResult<()> {
        match self.stack.pop() {
            Some(addr) => {
                self.stack.push(match self.heap.find(&addr) {
                    Some(val) => *val,
                    None => 0,
                });
                Ok(())
            }
            None => Err(IllegalStackManipulation),
        }
    }

    fn mark(
        &mut self,
        program: &mut ByteCodeReader,
        index: &mut HashMap<i64, u64>,
        label: i64,
    ) -> MachineResult<()> {
        match program.stream_position() {
            Ok(pos) => {
                index.insert(label, pos);
                Ok(())
            }
            Err(err) => return Err(MachineIoError(err)),
        }
    }

    fn call(
        &mut self,
        program: &mut ByteCodeReader,
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

    fn jump(
        &mut self,
        program: &mut ByteCodeReader,
        index: &mut HashMap<i64, u64>,
        label: &i64,
    ) -> MachineResult<()> {
        match index.find_copy(label) {
            Some(pos) => match program.seek(SeekFrom::Start(pos)) {
                Ok(_) => Ok(()),
                Err(err) => Err(MachineIoError(err)),
            },
            None => loop {
                match program.read_inst() {
                    Ok((opcode, operand)) if opcode == bytecode::CMD_MARK => {
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
                    Err(ref e) if e.kind == EndOfFile => return Err(UndefinedLabel),
                    Err(err) => return Err(MachineIoError(err)),
                    _ => continue,
                }
            },
        }
    }

    fn jump_if(
        &mut self,
        program: &mut ByteCodeReader,
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

    fn do_return(
        &mut self,
        program: &mut ByteCodeReader,
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
        match self.stdin.read_line() {
            Ok(line) => match from_str(line.replace("\n", "").as_slice()) {
                Some(n) => {
                    self.stack.push(n);
                    self.store()?;
                    Ok(())
                }
                None => Err(MachineIoError(standard_error(InvalidInput))),
            },
            Err(err) => Err(MachineIoError(err)),
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::io::{self, Cursor, Seek, SeekFrom};

    use bytecode::ByteCodeWriter;

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
        vm.stack.push_all([2, 19, 2, 5, 1, 1]);
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
        vm.stack.push_all([1, 1, 2]);
        vm.step(&mut bc, &mut index, &mut caller).unwrap();
        assert_eq!(vm.stack, vec!(1));
        assert_eq!(vm.heap.find(&1), Some(&2));
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
        vm.stack.push_all([-1, 0]);
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
        assert_eq!(vm.step(&mut bc, &mut index, &mut caller), Ok(false));
    }

    #[test]
    fn test_io() {
        let mut heap = [0, 0];
        let mut buf = [0, ..2];
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
            vm.stack.push_all([5, 66, 2, 1]);
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            vm.step(&mut bc, &mut index, &mut caller).unwrap();
            assert!(vm.step(&mut bc, &mut index, &mut caller).is_err());

            heap[0] = *vm.heap.find(&1).unwrap();
            heap[1] = *vm.heap.find(&2).unwrap();
        }
        assert!(heap == [87, 123]);
        assert!(buf == [66, 53]);
    }
}
