#[derive(Clone, Copy, Debug)]
pub struct Pos(pub usize);

impl Pos {
  pub fn offset(&self) -> usize {
    self.0
  }
}
