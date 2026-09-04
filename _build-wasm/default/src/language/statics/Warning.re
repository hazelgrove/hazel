open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_pat =
  | UnusedVar(string);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | WarningPat(warning_pat)
  | None;

[@deriving (show({with_path: false}), sexp, yojson)]
type list_item =
  | Pat(warning_pat);

let to_list: t => list(list_item) =
  fun
  | None => []
  | WarningPat(p) => [Pat(p)];

let var_is_unused = (co_ctx, name): t =>
  if (String.starts_with(~prefix="_", name) || CoCtx.contains_hole(co_ctx)) {
    None;
  } else {
    switch (VarMap.lookup(co_ctx, name)) {
    | None => WarningPat(UnusedVar(name))
    | Some(_) => None
    };
  };
