open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_exp =
  | PlaceholderWarningExp(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_pat =
  | UnusedVar(string)
  | PlaceholderWarningPat(string);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_typ =
  | PlaceholderWarningTyp(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_tpat =
  | PlaceholderWarningTPat(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | WarningExp(warning_exp)
  | WarningPat(warning_pat)
  | WarningTyp(warning_typ)
  | WarningTPat(warning_tpat)
  | None;

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
