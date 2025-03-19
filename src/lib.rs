/*! The infrastructure for implementing esolang.

`whitebase` provides the virtual machine,
parsers and generators, and assembly language.

```rust
extern crate whitebase;

use std::io::{Cursor, Seek, SeekFrom};

use whitebase::machine;
use whitebase::syntax::{Compiler, Whitespace};

fn main() {
    let src = "   \t\t \t  \t\n   \t  \t   \n\t\n  \t\n  \n\n\n";
    let mut buffer = Cursor::new(src.as_bytes());
    let mut bc = Cursor::new(Vec::new());
    let ws = Whitespace::new();
    match ws.compile(&mut buffer, &mut bc) {
        Err(e) => panic!("{}", e),
        _ => {
            bc.seek(SeekFrom::Start(0)).unwrap();
            let mut machine = machine::with_stdio();
            match machine.run(&mut bc) {
                Err(e) => panic!("{:?}", e),
                _ => (),
            }
        }
    }
}
```
*/

#![crate_name = "whitebase"]
#![crate_type = "rlib"]
#![warn(missing_docs)]

extern crate log;

pub const VERSION_MAJOR: usize = 0;
pub const VERSION_MINOR: usize = 1;
pub const VERSION_TINY: usize = 0;
pub const PRE_RELEASE: bool = true;

/// Build version string.
pub fn version() -> String {
    format!(
        "{}.{}.{}{}",
        VERSION_MAJOR,
        VERSION_MINOR,
        VERSION_TINY,
        if PRE_RELEASE { "-pre" } else { "" }
    )
}

pub mod bytecode;
pub(crate) mod io;
pub mod ir;
pub mod machine;
pub mod syntax;
