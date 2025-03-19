use std::io::{self, BufRead, ErrorKind};
use std::str;

pub trait BufReadExt: BufRead {
    fn read_char(&mut self) -> io::Result<char> {
        const MAX_UTF8_LEN: usize = 4;
        let mut buf = [0; MAX_UTF8_LEN];
        let mut buf_len = 0;

        loop {
            let curr_buf = loop {
                match self.fill_buf() {
                    Ok(buf) => break buf,
                    Err(err) if err.kind() == ErrorKind::Interrupted => continue,
                    Err(err) => return Err(err),
                }
            };
            if curr_buf.is_empty() {
                if buf_len != 0 {
                    break;
                }
                return Err(ErrorKind::UnexpectedEof.into());
            }

            let n = (MAX_UTF8_LEN - buf_len).min(curr_buf.len());
            buf[buf_len..buf_len + n].copy_from_slice(&curr_buf[..n]);
            buf_len += n;

            let valid_prefix = match str::from_utf8(&buf[..buf_len]) {
                Ok(s) => s,
                Err(err) if err.valid_up_to() != 0 => {
                    // SAFETY: This range is guaranteed to be valid UTF-8.
                    unsafe { str::from_utf8_unchecked(&buf[..err.valid_up_to()]) }
                }
                Err(err) => match err.error_len() {
                    Some(error_len) if error_len < buf_len => {
                        self.consume(error_len - (buf_len - n));
                        break;
                    }
                    _ => {
                        self.consume(n);
                        continue;
                    }
                },
            };
            let ch = valid_prefix.chars().next().unwrap();
            self.consume(ch.len_utf8() - (buf_len - n));
            return Ok(ch);
        }
        Err(io::Error::new(
            ErrorKind::InvalidData,
            "invalid UTF-8 sequence",
        ))
    }
}

impl<R: BufRead> BufReadExt for R {}
