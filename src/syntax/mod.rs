//! Compilers and Decompilers.

pub use self::assembly::Assembly;
pub use self::brainfuck::Brainfuck;
pub use self::dt::DT;
pub use self::ook::Ook;
pub use self::whitespace::Whitespace;

use std::io::{self, BufRead, Write};

use crate::bytecode::{ByteCodeReader, ByteCodeWriter};

/// Convert from source code to bytecodes.
pub trait Compiler {
    /// Convert from source code to bytecodes.
    fn compile<B: BufRead, W: ByteCodeWriter>(
        &self,
        input: &mut B,
        output: &mut W,
    ) -> io::Result<()>;
}

/// Generate source code from bytecods.
pub trait Decompiler {
    /// Generate source code from bytecods.
    fn decompile<R: ByteCodeReader, W: Write>(
        &self,
        input: &mut R,
        output: &mut W,
    ) -> io::Result<()>;
}

pub mod assembly;
pub mod brainfuck;
pub mod dt;
pub mod ook;
pub mod whitespace;
