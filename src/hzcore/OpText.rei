type t =
  | Underscore
  | IntLit(IntLit.t)
  | FloatLit(FloatLit.t)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(VarId.t)
  | Invalid(string);

let of_string: string => t;