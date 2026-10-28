/* Function-definition syntactic sugar for `let`-bindings.

   Hazel supports a user-facing shorthand for defining functions:

       let f(x: Int, y) = x + y

   is sugar for the explicit `Fun` form:

       let f = fun (x: Int, y) -> x + y

   An optional return-type annotation on the binder

       let f(x: Int, y): Ret = x + y

   desugars to an ascription on the body:

       let f = fun (x: Int, y) -> (x + y : Ret)

   The desugaring is handled entirely within the statics pass: the
   surface parser produces `Let(Ap(Var(f), args), def, body)` (optionally
   wrapped in `Asc(_, ret_ty)`), and the `Let` case of `Statics.re`
   delegates to `rewrite` below to build an equivalent `Let(f, fun ..., body)`.

   This is structurally analogous to other sugar handlers in the statics
   pipeline:
     - `autolabel_singleton_tuple` (in Statics.re) which lifts values into
       singleton labeled tuples,
     - `ModuleExp(mp, def, body)` (in Statics.re) which expands to
       `Let(MPat.to_pat(mp), def, body)`.

   All of these reuse the id (and formatting secondary) of the surface
   term for the desugared outer term by going through `Exp.unwrap`, so
   that statics on the desugared form populates the info map under the
   user's original ids. */

/* Build the desugared expression:

       Let(f_name, Fun(args, (def : ret_ty)?, None, None), body)

   The outer `Let` is produced via `Exp.unwrap` on the surface `Let`,
   which preserves its ids and secondary (formatting) so that statics
   on the rewrite populates the info map under the user-facing id.
   The `Fun` and (optional) `Asc` wrappers use fresh deterministic ids
   derived from adjacent surface ids, keeping the rewrite stable across
   runs. */
let desugared_fun =
    (~args: Pat.t, ~ret_ty: option(Typ.t), ~def: Exp.t): Exp.t => {
  let def_in_fun =
    switch (ret_ty) {
    | Some(ty) =>
      (Asc(def, ty): Exp.term)
      |> IdTagged.fresh_deterministic(Typ.rep_id(ty))
    | None => def
    };
  (Fun(args, def_in_fun, None, None): Exp.term)
  |> IdTagged.fresh_deterministic(Pat.rep_id(args));
};

let rewrite =
    (
      ~orig_let: Exp.t,
      ~f_name: Pat.t,
      ~args: Pat.t,
      ~ret_ty: option(Typ.t),
      ~def: Exp.t,
      ~body: Exp.t,
    )
    : Exp.t => {
  let (_, rewrap) = Exp.unwrap(orig_let);
  rewrap(Let(f_name, desugared_fun(~args, ~ret_ty, ~def), body));
};

/* After statics has processed the desugared `Let`, the info map is
   missing entries for ids that only exist in the original binder
   pattern -- the `Ap(f, args)` wrapper and the optional outer
   `Asc(..., ret_ty)`. Populate those entries using the info already
   computed for the function name, since both sub-trees denote the
   same binder and share type and context with `f_name`.

   We also copy the ana type into `ty` so that the cursor inspector
   shows the binder's type directly (e.g. `: Int -> Int`) rather than
   the unknown synthesized type of the uncharted Ap/Asc wrapper. Both
   wrappers are tagged `Pat(ApFunc)` ("Function definition") so the
   inspector renders them with the same clean "var-like" shape
   regardless of whether the user wrote a return-type annotation. */
let add_binder_infos =
    (m: StaticsBase.Map.t, ~user_pat: Pat.t, ~f_name: Pat.t)
    : StaticsBase.Map.t => {
  let f_id = Pat.rep_id(f_name);
  switch (StaticsBase.Map.lookup_pat(f_id, m)) {
  | None => m
  | Some(f_info) =>
    let binder_info: Info.pat = {
      ...f_info,
      cls: Cls.Pat(ApFunc),
      ty: f_info.ana,
      elab_syn_ty: f_info.ana,
    };
    let add_for = (pat, m) =>
      StaticsBase.Map.add_info(
        IdTagged.ids(pat),
        Info.InfoPat({
          ...binder_info,
          user_term: pat,
          elab_term: pat,
        }),
        m,
      );
    switch (IdTagged.term_of(user_pat)) {
    | Asc(inner, _) =>
      let m = add_for(user_pat, m);
      add_for(inner, m);
    | _ => add_for(user_pat, m)
    };
  };
};
