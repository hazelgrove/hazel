open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type warning_pat =
  | UnusedVar(string);
//  | ShadowedVar(string, string)

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type warning_typ =
  /* A type defined as itself: no value has it. */
  | NonContractive(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type list_item =
  | Pat(warning_pat)
  | TPat(warning_typ)
  | Typ(warning_typ);

/* The name [binder] binds, when [ty] is non-contractive and its body reduces
   to that binder. Of nested binders, only that one is: `rec x -> rec y -> x`
   loops at the outer binder, `rec x -> rec y -> y` and `rec x -> rec x -> x`
   at the inner one. */
let loops_at = (binder: TPat.t, ty): option(string) =>
  switch (TPat.tyvar_of_utpat(binder), Typ.non_contractive(ty)) {
  | (Some(name), Some(loop))
      when Id.equal(TPat.rep_id(loop), TPat.rep_id(binder)) =>
    Some(name)
  | (Some(_) | None, Some(_) | None) => None
  };

/* The warnings for binding the type alias [binder] to [ty], the type it
   stands for, whose recursive binder is [binder] itself. */
let type_binding = (binder: TPat.t, ty): list(list_item) =>
  loops_at(binder, ty)
  |> Option.map(name => TPat(NonContractive(name)))
  |> Option.to_list;

/* The warnings for a written recursive type [ty] whose binder is [binder]. */
let rec_type = (binder: TPat.t, ty): list(list_item) =>
  loops_at(binder, ty)
  |> Option.map(name => Typ(NonContractive(name)))
  |> Option.to_list;

let var_is_unused = (co_ctx, name): list(list_item) =>
  String.starts_with(~prefix="_", name)
  || CoCtx.contains_hole(co_ctx)
  || Option.is_some(VarMap.lookup(co_ctx, name))
    ? [] : [Pat(UnusedVar(name))];
