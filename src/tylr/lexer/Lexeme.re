include Base.Lexeme;

let is_porous =
  fun
  | S(_)
  | G(_) => true
  | T(_) => false;

let is_space =
  fun
  | G(_)
  | T(_) => None
  | S(s) => Some(s);
