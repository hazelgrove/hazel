open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_pat =
  | UnusedVar(string);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_typ =
  /* A type defined as itself: no value has it. */
  | NonContractive(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | WarningPat(warning_pat)
  | None;

[@deriving (show({with_path: false}), sexp, yojson)]
type list_item =
  | Pat(warning_pat)
  | TPat(warning_typ)
  | Typ(warning_typ);

let to_list: t => list(list_item) =
  fun
  | None => []
  | WarningPat(p) => [Pat(p)];

/* The warnings for binding type alias [name] to [ty]. */
let type_binding = (name, ty): list(list_item) =>
  switch (Typ.non_contractive(ty)) {
  | Some(_) => [TPat(NonContractive(name))]
  | None => []
  };

/* The warnings for a written recursive type [ty] binding [name]. Of nested
   binders, only the one its body is is warned. */
let rec_type = (name, ty): list(list_item) =>
  switch (Typ.non_contractive(ty)) {
  | Some(x) when String.equal(x, name) => [Typ(NonContractive(name))]
  | Some(_)
  | None => []
  };

let var_is_unused = (co_ctx, name): t =>
  if (String.starts_with(~prefix="_", name) || CoCtx.contains_hole(co_ctx)) {
    None;
  } else {
    switch (VarMap.lookup(co_ctx, name)) {
    | None => WarningPat(UnusedVar(name))
    | Some(_) => None
    };
  };
