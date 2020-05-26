[@deriving sexp]
type t =
  /* Might be useful if/when we add type keywords or Undescore */
  /* | Underscore */
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyId.t);

let of_builtIn_opt = (text: TyId.t): option(t) =>
  if (TyId.is_Bool(text)) {
    Some(Bool);
  } else if (TyId.is_Int(text)) {
    Some(Int);
  } else if (TyId.is_Float(text)) {
    Some(Float);
  } else {
    None;
  };

let of_text = (text: TyId.t): option(t) => {
  /* if (String.equal("_", text)) { */
  /* Some(Underscore); */
  /* } else */
  switch (ExpandingKeyword.mk(text), of_builtIn_opt(text)) {
  | (Some(k), _) => Some(ExpandingKeyword(k))
  | (_, Some(ty)) => Some(ty)
  | (None, None) =>
    if (text |> TyId.is_valid) {
      Some(TyVar(text));
    } else {
      None;
    }
  };
};