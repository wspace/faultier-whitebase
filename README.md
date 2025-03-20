# Whitebase

This project provides infrastructure for implementing esolang.

This is a fork of faultier's [Whitebase](https://github.com/faultier/whitebase)
library, which updates it from [Rust](https://www.rust-lang.org/) v0.12.0-pre to
modern Rust.

## Features

- The virtual machine having the instruction set based on [Whitespace](https://web.archive.org/web/20150717203521/http://compsoc.dur.ac.uk:80/whitespace/index.php)'s specification.
- Parsers and code generators for some languages (e.g. [Whitespace](https://web.archive.org/web/20150717203521/http://compsoc.dur.ac.uk:80/whitespace/index.php), [Ook!](https://www.dangermouse.net/esoteric/ook.html), etc.)
- Simple assembly language

## Usage

### Compile and execute

```rust
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

## Application using Whitebase

- [Albino](https://github.com/wspace/faultier-albino)

## License

This project distributed under the MIT License.
https://opensource.org/license/MIT
