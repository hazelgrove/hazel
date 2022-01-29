[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyVar.Name.t);

let of_builtIn_opt = (name: TyVar.Name.t): option(t) =>
  switch (TyVar.Name.to_string(name)) {
  | "Bool" => Some(Bool)
  | "Int" => Some(Int)
  | "Float" => Some(Float)
  | _ => None
  };

let of_tyvar_name = (name: TyVar.Name.t): option(t) => {
  switch (
    ExpandingKeyword.mk(TyVar.Name.to_string(name)),
    of_builtIn_opt(name),
  ) {
  | (Some(k), _) => Some(ExpandingKeyword(k))
  | (_, Some(ty)) => Some(ty)
  | (None, None) =>
    TyVar.Name.(valid(to_string(name))) ? Some(TyVar(name)) : None
  };
};
