open Sexplib.Std;

[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyVar.t)
  | InvalidText(string);

let builtin = (name: string): option(t) =>
  switch (name) {
  | "Bool" => Some(Bool)
  | "Int" => Some(Int)
  | "Float" => Some(Float)
  | _ => None
  };

let of_string = (text: string): t => {
  switch (ExpandingKeyword.of_string(text), builtin(text)) {
  | (Some(k), _) => ExpandingKeyword(k)
  | (_, Some(ty)) => ty
  | (None, None) => TyVar.is_valid(text) ? TyVar(text) : InvalidText(text)
  };
};
