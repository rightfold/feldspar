extern crate gcc;

fn main() {
  gcc::compile_library("libvalue.a", &["src/value.c"]);
}
