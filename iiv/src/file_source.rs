use std::{
    error::Error,
    fmt::{Display, Write},
    fs::File,
    io::{self, BufReader, Read, SeekFrom},
};

use crate::{Position, Source};
use std::io::Seek;

pub struct FileSource {
    file: BufReader<File>,
    name: String,
    current_line: u32,
    current_character: u32,
    line_offsets: Vec<u32>,
}

impl FileSource {
    pub fn new(file: File, name: String) -> Result<Self, Box<dyn Error>> {
        Ok(FileSource {
            file: BufReader::new(file),
            name,
            current_line: 0,
            current_character: 0,
            line_offsets: vec![],
        })
    }

    fn goto_next_line(&mut self) -> Result<(), Box<dyn Error>> {
        while let Some(character) = self.next()? {
            match character {
                '\n' => {
                    if self.line_offsets.len() == self.current_line as usize {
                        let pos = self.byte_position()?;
                        self.line_offsets.push(pos);
                    }
                    return Ok(());
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn byte_position(&mut self) -> Result<u32, io::Error> {
        Seek::stream_position(&mut self.file).map(|pos| pos as u32)
    }

    fn next_utf8(&mut self) -> Result<Option<char>, Box<dyn Error>> {
        let mut buf = [0; 4];
        if self.file.read(&mut buf[..1])? == 0 {
            return Ok(None);
        }

        if buf[0] & 0b1000_0000 == 0 {
            return Ok(Some(char::from_u32(u32::from_ne_bytes(buf)).unwrap()));
        }

        if buf[0] & 0b1110_0000 == 0b1100_0000 {
            if self.file.read(&mut buf[1..2])? == 0 {
                return Err(Box::new(Utf8Error));
            }

            return Ok(Some(char::from_u32(u32::from_ne_bytes(buf)).unwrap()));
        }

        if buf[0] & 0b1111_0000 == 0b1110_0000 {
            if self.file.read(&mut buf[1..3])? < 2 {
                return Err(Box::new(Utf8Error));
            }

            return Ok(Some(char::from_u32(u32::from_ne_bytes(buf)).unwrap()));
        }

        if buf[0] & 0b1111_1000 == 0b1111_0000 {
            if self.file.read(&mut buf[1..4])? < 3 {
                return Err(Box::new(Utf8Error));
            }

            return Ok(Some(char::from_u32(u32::from_ne_bytes(buf)).unwrap()));
        }

        Err(Box::new(Utf8Error))
    }
}

impl Source for FileSource {
    fn name(&self) -> &str {
        &self.name
    }

    fn seek(&mut self, Position { line, column }: Position) -> Result<(), Box<dyn Error>> {
        if line > self.current_line {
            while line > self.current_line {
                self.goto_next_line()?;
            }
        } else {
            Seek::seek(
                &mut self.file,
                SeekFrom::Start(self.line_offsets[line as usize] as u64),
            )?;
        }

        for _ in 0..column {
            self.next()?;
        }

        Ok(())
    }

    fn next(&mut self) -> Result<Option<char>, Box<dyn Error>> {
        let next = self.next_utf8();
        if let Ok(Some(character)) = next {
            if character == '\n' {
                self.current_character = 0;
                self.current_line += 1;
            } else {
                self.current_character += 1;
            }
        }
        next
    }
}

#[derive(Debug)]
pub struct Utf8Error;

impl Error for Utf8Error {}
impl Display for Utf8Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "an invalid utf-8 byte was encountered")
    }
}
