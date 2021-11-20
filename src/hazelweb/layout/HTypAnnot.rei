type t =
  | HoleLabel
  | TyVarHole
  | Delim
  | Step(int)
  | Term;
