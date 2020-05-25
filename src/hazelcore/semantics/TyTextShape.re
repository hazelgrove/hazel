[@deriving sexp]
type t =
  /* Might be useful if/when we add type keywords or Undescore */
  /* | Underscore */
  /* | ExpandingKeyword(ExpandingKeyword.t) */
  | TyVar(TyId.t);

let of_text = (text: string): option(t) =>
  /* if (String.equal("_", text)) { */
  /* Some(Underscore); */
  /* } else */
  if (TyId.is_valid(text)) {
    Some(TyVar(text));
  } else {
    None;
  };
