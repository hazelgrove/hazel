open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type warning_pat =
  | UnusedVar(string);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type warning_typ =
  /* A type defined as itself: no value has it. */
  | NonContractive(string);

/* Expression forms that have no evaluation semantics. These are legal
   propositions, so they are warnings rather than marks: they only mean
   "this will not run" when written outside a theorem statement or
   proof. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type warning_exp =
  /* `forall` / `forall ... where ...` used as ordinary code. */
  | ForallOutsideTheorem;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type list_item =
  | Pat(warning_pat)
  | TPat(warning_typ)
  | Typ(warning_typ)
  | Exp(warning_exp);

/* The warnings for binding type alias [name] to [def]. */
let type_binding = (name, def: Typ.alias_def): list(list_item) =>
  switch (def) {
  | Recursive(ty) when Option.is_some(Typ.non_contractive(ty)) => [
      TPat(NonContractive(name)),
    ]
  | Recursive(_)
  | Plain => []
  };

/* The warnings for a written recursive type [ty] whose binder is [binder].
   Of nested binders, only the one the body reduces to is warned:
   `rec x -> rec y -> x` on the outer type, `rec x -> rec y -> y` and
   `rec x -> rec x -> x` on the inner one. */
let rec_type = (binder: TPat.t, ty): list(list_item) =>
  switch (TPat.tyvar_of_utpat(binder), Typ.non_contractive(ty)) {
  | (Some(name), Some(loop))
      when Id.equal(TPat.rep_id(loop), TPat.rep_id(binder)) => [
      Typ(NonContractive(name)),
    ]
  | (Some(_) | None, Some(_) | None) => []
  };

let var_is_unused = (co_ctx, name): list(list_item) =>
  String.starts_with(~prefix="_", name)
  || CoCtx.contains_hole(co_ctx)
  || Option.is_some(VarMap.lookup(co_ctx, name))
    ? [] : [Pat(UnusedVar(name))];
