[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyId.t);

let of_builtIn_opt = (id: TyId.t): option(t) =>
  if (TyId.is_Bool(id)) {
    Some(Bool);
  } else if (TyId.is_Int(id)) {
    Some(Int);
  } else if (TyId.is_Float(id)) {
    Some(Float);
  } else {
    None;
  };

let of_tyid = (id: TyId.t): option(t) => {
  switch (ExpandingKeyword.mk(TyId.to_string(id)), of_builtIn_opt(id)) {
  | (Some(k), _) => Some(ExpandingKeyword(k))
  | (_, Some(ty)) => Some(ty)
  | (None, None) =>
    if (id |> TyId.is_valid) {
      Some(TyVar(id));
    } else {
      None;
    }
  };
};
