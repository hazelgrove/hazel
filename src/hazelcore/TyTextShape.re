open Sexplib.Std;

[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(string);

let builtin = (name: string): option(t) =>
  switch (name) {
  | "Bool" => Some(Bool)
  | "Int" => Some(Int)
  | "Float" => Some(Float)
  | _ => None
  };

let of_string = (name: string): option(t) => {
  switch (ExpandingKeyword.of_string(name), builtin(name)) {
  | (Some(k), _) => Some(ExpandingKeyword(k))
  | (_, Some(ty)) => Some(ty)
  | (None, None) => TyVar.is_valid(name) ? Some(TyVar(name)) : None
  };
};
