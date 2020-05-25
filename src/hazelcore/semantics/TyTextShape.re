open Sexplib.Std;

[@deriving sexp]
type t =
  /* Might be useful if/when we add type keywords or Undescore */
  /* | Underscore */
  | BuiltIn(TyId.t)
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyId.t);

let of_builtIn_opt = (text: TyId.t): option(t) =>
  if (TyId.is_Bool(text) || TyId.is_Int(text) || TyId.is_Float(text)) {
    Some(BuiltIn(text));
  } else {
    None;
  };
let of_text = (text: TyId.t): option(t) => {
  /* if (String.equal("_", text)) { */
  /* Some(Underscore); */
  /* } else */
  switch (of_builtIn_opt(text), ExpandingKeyword.mk(text)) {
  | (Some(_), _) => Some(BuiltIn(text))
  | (_, Some(k)) => Some(ExpandingKeyword(k))
  | (None, None) =>
    if (text |> TyId.is_valid) {
      Some(TyVar(text));
    } else {
      None;
    }
  };
};