use std::iter;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

  pub fn advance(&mut self, char: char) {
    self.offset += 1;
    if char == '\n' {
      self.line += 1;
      self.column = 1;
    } else {
      self.column += 1;
    }
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Lexeme<'a, T>(pub T, pub LexemeF<'a>);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LexemeF<'a> {
  Identifier(&'a str),

  End,
  False,
  Forall,
  Fun,
  In,
  Let,
  True,
  Val,

  Arrow,
  Colon,
  Comma,
  Equals,
  LeftParenthesis,
  RightParenthesis,
  LeftBrace,
  RightBrace,

  Str(&'a str),
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
  input: &'a str,
  position: Position,
  abort: bool,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Lexer{input: input, position: Position::zero(), abort: false}
  }

  pub fn next_position(&self) -> Position {
    self.position
  }

  fn peek_char(&self) -> Option<char> {
    self.input.chars().next()
  }

  fn take_char(&mut self) -> Option<char> {
    let mut chars = self.input.chars();
    let result = chars.next();
    for c in result {
      self.position.advance(c);
    }
    self.input = chars.as_str();
    result
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
      self.position.advance(c);
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
      Some('#') => {
        self.take_chars_while(|c| c != '\n');
        self.next()
      },
      Some('(') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::LeftParenthesis)))
      },
      Some(')') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::RightParenthesis)))
      },
      Some('{') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::LeftBrace)))
      },
      Some('}') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::RightBrace)))
      },
      Some(':') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::Colon)))
      },
      Some(',') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::Comma)))
      },
      Some('=') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::Equals)))
      },
      Some('-') => {
        self.take_char();
        if self.peek_char() == Some('>') {
          self.take_char();
          Some(Ok(Lexeme(position, LexemeF::Arrow)))
        } else {
          self.abort = true;
          Some(Err(position))
        }
      },
      Some('"') => {
        self.take_char();
        let value = self.take_chars_while(|c| c != '"');
        if self.take_char() != Some('"') {
          Some(Err(position))
        } else {
          Some(Ok(Lexeme(position, LexemeF::Str(value))))
        }
      },
      Some('∀') => {
        self.take_char();
        Some(Ok(Lexeme(position, LexemeF::Forall)))
      },
      Some(c) if is_identifier_start(c) => {
        let name = self.take_chars_while(is_identifier_continue);
        Some(Ok(Lexeme(position, match name {
          "end"    => LexemeF::End,
          "false"  => LexemeF::False,
          "forall" => LexemeF::Forall,
          "fun"    => LexemeF::Fun,
          "in"     => LexemeF::In,
          "let"    => LexemeF::Let,
          "true"   => LexemeF::True,
          "val"    => LexemeF::Val,
          _ => LexemeF::Identifier(name),
        })))
      },
      Some(_) => {
        self.abort = true;
        Some(Err(position))
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
  || char == '%'
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
    assert_eq!(
      Lexer::new("funend").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Identifier("funend")))]
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

  #[test]
  fn test_keywords() {
    assert_eq!(
      Lexer::new("fun end").collect::<Vec<_>>(),
      vec![
        Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Fun)),
        Ok(Lexeme(Position::new(4, 1, 5), LexemeF::End)),
      ]
    );
  }

  #[test]
  fn test_punctuation() {
    assert_eq!(
      Lexer::new("(){},->").collect::<Vec<_>>(),
      vec![
        Ok(Lexeme(Position::new(0, 1, 1), LexemeF::LeftParenthesis)),
        Ok(Lexeme(Position::new(1, 1, 2), LexemeF::RightParenthesis)),
        Ok(Lexeme(Position::new(2, 1, 3), LexemeF::LeftBrace)),
        Ok(Lexeme(Position::new(3, 1, 4), LexemeF::RightBrace)),
        Ok(Lexeme(Position::new(4, 1, 5), LexemeF::Comma)),
        Ok(Lexeme(Position::new(5, 1, 6), LexemeF::Arrow)),
      ]
    );
  }

  #[test]
  fn test_string() {
    assert_eq!(
      Lexer::new("\"foo\"").collect::<Vec<_>>(),
      vec![Ok(Lexeme(Position::new(0, 1, 1), LexemeF::Str("foo")))]
    );
    assert_eq!(
      Lexer::new("\"foo").collect::<Vec<_>>(),
      vec![Err(Position::new(0, 1, 1))]
    );
  }
}
