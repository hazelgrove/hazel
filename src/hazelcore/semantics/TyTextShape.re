open Sexplib.Std;

[@deriving sexp]
type t =
  /* Might be useful if/when we add type keywords or Undescore */
  /* | Underscore */
  /* | ExpandingKeyword(ExpandingKeyword.t) */
  | Var(Var.t);

let of_text = (text: string): option(t) =>
  /* if (String.equal("_", text)) { */
  /* Some(Underscore); */
  /* } else */
  if (Var.is_valid(text)) {
    Some(Var(text));
  } else {
    None;
  };
