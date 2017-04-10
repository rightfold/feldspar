use std::iter;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
  pub offset: usize,
  pub line: usize,
  pub column: usize,
}

impl Position {
  pub fn new(offset: usize, line: usize, column: usize) -> Self {
    Position{
      offset: offset,
      line: line,
      column: column,
    }
  }

  pub fn zero() -> Self {
    Position::new(0, 1, 1)
  }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Lexeme<'a, T>(T, LexemeF<'a>);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum LexemeF<'a> {
  Identifier(&'a str),
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Lexer<'a> {
  input: &'a str,
  position: Position,
  abort: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Lexer{input: input, position: Position::zero(), abort: false}
  }

  fn peek_char(&self) -> Option<char> {
    self.input.chars().next()
  }

  fn take_chars_while<F>(&mut self, pred: F) -> &'a str
    where F: Fn(char) -> bool {
    let terminator = (self.input.len(), '\n');
    let mut len = 0;
    for (i, c) in self.input.char_indices().chain(iter::once(terminator)) {
      len = i;
      if !pred(c) {
        break;
      }
      self.position.offset += 1;
      if c == '\n' {
        self.position.line += 1;
        self.position.column = 1;
      } else {
        self.position.column += 1;
      }
    }
    let result = unsafe { self.input.slice_unchecked(0, len) };
    self.input = unsafe { self.input.slice_unchecked(len, self.input.len()) };
    result
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Result<Lexeme<'a, Position>, Position>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.abort {
      return None;
    }
    self.take_chars_while(is_whitespace);
    let position = self.position;
    match self.peek_char() {
      None =>
        None,
      Some(c) if is_identifier_start(c) => {
        let name = self.take_chars_while(is_identifier_continue);
        Some(Ok(Lexeme(position, LexemeF::Identifier(name))))
      },
      Some(_) => {
        self.abort = true;
        Some(Err(self.position))
      },
    }
  }
}

fn is_whitespace(char: char) -> bool {
  char.is_whitespace()
}

// FIXME: Switch to is_xid_start once it is stable.
fn is_identifier_start(char: char) -> bool {
  char.is_alphabetic()
  || char == '_'
}

// FIXME: Switch to is_xid_continue once it is stable.
fn is_identifier_continue(char: char) -> bool {
  is_identifier_start(char)
  || char.is_numeric()
  || char == '\''
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_empty() {
    assert_eq!(
      Lexer::new("").collect::<Vec<_>>(),
      vec![]
    );
  }

  #[test]
  fn test_whitespace() {
    assert_eq!(
      Lexer::new(" \t\r\n").collect::<Vec<_>>(),
      vec![]
    );
  }

  #[test]
  fn test_identifier() {
    assert_eq!(
      Lexer::new("a").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("a")))]
    );
    assert_eq!(
      Lexer::new("ab").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("ab")))]
    );
    assert_eq!(
      Lexer::new("ab_c").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("ab_c")))]
    );
    assert_eq!(
      Lexer::new("ab_cd'").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("ab_cd'")))]
    );
  }

  #[test]
  fn test_identifiers() {
    assert_eq!(
      Lexer::new("ab cd").collect::<Vec<_>>(),
      vec![
        Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("ab"))),
        Ok(Lexeme(Position::new(3, 1, 4), LexemeF::Identifier("cd"))),
      ]
    );
  }
}
