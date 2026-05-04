open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_exp =
  | ContainsUnknown(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_pat =
  | UnusedVar(string)
  | ContainsUnknown(Typ.t);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_typ =
  | ContainsUnknown(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | WarningExp(warning_exp)
  | WarningPat(warning_pat)
  | WarningTyp(warning_typ)
  | None;

[@deriving (show({with_path: false}), sexp, yojson)]
type list_item =
  | Exp(warning_exp)
  | Pat(warning_pat)
  | Typ(warning_typ);

let to_list: t => list(list_item) =
  fun
  | None => []
  | WarningExp(e) => [Exp(e)]
  | WarningPat(p) => [Pat(p)]
  | WarningTyp(t) => [Typ(t)];

let empty: t = None;

let var_is_unused = (co_ctx, name): t =>
  if (String.starts_with(~prefix="_", name) || CoCtx.contains_hole(co_ctx)) {
    None;
  } else {
    switch (VarMap.lookup(co_ctx, name)) {
    | None => WarningPat(UnusedVar(name))
    | Some(_) => None
    };
  };
