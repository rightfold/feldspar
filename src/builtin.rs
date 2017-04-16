use check::{Ty, TY_BYTES, TY_INT, TY_STR};
use std::collections::HashMap;
use typed_arena::Arena;

pub fn env<'ty>(arena: &'ty Arena<Ty<'ty>>)
  -> HashMap<&'static str, &'ty Ty<'ty>> {
  #[allow(non_snake_case)]
  let Func = |a, b| arena.alloc(Ty::Func(a, b));

  let mut map = HashMap::new();
  map.insert("stdout#", &TY_INT);
  map.insert("to_utf8#", Func(&TY_STR, &TY_BYTES));
  map.insert("write#", Func(&TY_INT, Func(&TY_BYTES, &TY_INT))); // FIXME: Return io ()
  map
}
