use std::error::Error;

use crate::Source;

pub struct StrSource {
    name: String,
    text: Vec<String>,
    line: u32,
    column: u32,
}

impl StrSource {
    pub fn new(name: String, text: &str) -> Self {
        StrSource {
            name,
            text: text.split_inclusive('\n').map(str::to_string).collect(),
            line: 0,
            column: 0,
        }
    }
}

impl Source for StrSource {
    fn name(&self) -> &str {
        &self.name
    }

    fn next(&mut self) -> Result<Option<char>, Box<dyn Error>> {
        let next = self.text[self.line as usize][self.column as usize..]
            .chars()
            .next();

        match next {
            Some(character) => {
                self.column += character.len_utf8() as u32;
                Ok(Some(character))
            }
            None => {
                if self.line + 1 == self.text.len() as u32 {
                    Ok(None)
                } else {
                    self.line += 1;
                    self.column = 0;
                    self.next()
                }
            }
        }
    }

    fn seek(&mut self, position: crate::Position) -> Result<(), Box<dyn Error>> {
        self.line = position.line;
        self.column = position.column;
        Ok(())
    }
}
