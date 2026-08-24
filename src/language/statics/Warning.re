open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type warning_pat =
  | UnusedVar(string);
//  | ShadowedVar(string, string)

/* Expression forms that have no evaluation semantics
   (docs/prover-obligations.md §0, §1.3). These are legal *propositions*,
   so they are warnings rather than marks: they only mean "this will not
   run" when written outside a theorem statement or proof. */
[@deriving (show({with_path: false}), sexp, yojson)]
type warning_exp =
  /* `forall` / `forall ... where ...` used as ordinary code. */
  | ForallOutsideTheorem
  /* `==` / `!=` at a type containing a function; carries the meet type. */
  | FunEqOutsideTheorem(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | WarningPat(warning_pat)
  | WarningExp(warning_exp)
  | None;

[@deriving (show({with_path: false}), sexp, yojson)]
type list_item =
  | Pat(warning_pat)
  | Exp(warning_exp);

let to_list: t => list(list_item) =
  fun
  | None => []
  | WarningPat(p) => [Pat(p)]
  | WarningExp(e) => [Exp(e)];

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
