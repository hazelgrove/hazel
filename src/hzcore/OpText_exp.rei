type t =
  | Underscore
  | IntLit(IntLit.t)
  | FloatLit(FloatLit.t)
  | BoolLit(bool)
  | Keyword(Keyword.t)
  | Var(VarId.t)
  | Invalid(string);

let of_string: string => t;
