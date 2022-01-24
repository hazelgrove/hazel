// [@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyVar.Name.t);

let of_builtIn_opt: TyVar.Name.t => option(t);
let of_tyvar_name: TyVar.Name.t => option(t);
