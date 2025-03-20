# Whitebase
[![Build Status](https://travis-ci.org/faultier/whitebase.svg?branch=master)](https://travis-ci.org/faultier/whitebase)

This project provides infrastructure for implementing esolang.

## Features

- The virtual machine having the instruction set based on [Whitespace](http://compsoc.dur.ac.uk/whitespace/index.php)'s specification.
- Parsers and code generators for some languages (e.g. [Whitespace](http://compsoc.dur.ac.uk/whitespace/index.php), [Ook!](http://www.dangermouse.net/esoteric/ook.html), etc.)
- Simple assembly language

[Rust](http://www.rust-lang.org/) v0.12.0-pre support.

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

- [Albino](https://github.com/faultier/rust-albino)

## License

This project distributed under the MIT License.
http://opensource.org/licenses/MIT
