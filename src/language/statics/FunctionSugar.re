/* Function-definition syntactic sugar for `let`-bindings.

   Hazel supports a user-facing shorthand for defining functions:

       let f(x: Int, y) = x + y

   is sugar for the explicit `Fun` form:

       let f = fun (x: Int, y) -> x + y

   A curried header gives one function per argument group:

       let f(x: Int)(y: Int) = x + y

   is `let f = fun (x: Int) -> fun (y: Int) -> x + y`. An optional
   return-type annotation on the binder

       let f(x: Int, y): Ret = x + y

   desugars to an ascription on the body:

       let f = fun (x: Int, y) -> (x + y : Ret)

   The desugaring is handled entirely within the statics pass: the
   surface parser produces `Let(Ap(Var(f), args), def, body)` (optionally
   wrapped in `Asc(_, ret_ty)`), and the `Let` and `ModLet` cases of
   `Statics.re` check the binding [binding] gives, then fill in the
   binder's own nodes with [add_binder_infos]. */

/* The binding `let p = def` makes: for the shorthand, the function's
   name bound to

       Fun(args, … Fun(last_args, (def : ret_ty)?, None, None) …, None, None)

   with one `Fun` per argument group, and otherwise the binding as
   written. The `Fun` and (optional) `Asc` wrappers use fresh
   deterministic ids derived from adjacent surface ids, keeping the
   rewrite stable across runs. */
let binding = (p: Pat.t, def: Exp.t): (Pat.t, Exp.t) =>
  switch (Pat.function_sugar(p)) {
  | Some((f_name, args, ret_ty)) =>
    let def_in_fun =
      switch (ret_ty) {
      | Some(ty) =>
        (Asc(def, ty): Exp.term)
        |> IdTagged.fresh_deterministic(Typ.rep_id(ty))
      | None => def
      };
    (
      f_name,
      List.fold_right(
        (args, body) =>
          (Fun(args, body, None, None): Exp.term)
          |> IdTagged.fresh_deterministic(Pat.rep_id(args)),
        args,
        def_in_fun,
      ),
    );
  | None => (p, def)
  };

/* After statics has processed the binding, the info map is missing
   entries for ids that only exist in the shorthand's binder pattern --
   each `Ap` on the spine (`f(x)(y)` has an inner `f(x)` node the caret
   can rest on) and the optional outer `Asc(..., ret_ty)`. Populate those
   entries using the info already computed for the function name, since
   both sub-trees denote the same binder and share type and context with
   it. A binding written plainly needs nothing.

   We also copy the ana type into `ty` so that the cursor inspector
   shows the binder's type directly (e.g. `: Int -> Int`) rather than
   the unknown synthesized type of the uncharted Ap/Asc wrapper. Both
   wrappers are tagged `Pat(ApFunc)` ("Function definition") so the
   inspector renders them with the same clean "var-like" shape
   regardless of whether the user wrote a return-type annotation. */
let add_binder_infos =
    (m: StaticsBase.Map.t, ~user_pat: Pat.t): StaticsBase.Map.t => {
  switch (
    Option.bind(Pat.function_sugar(user_pat), ((f_name, _, _)) =>
      StaticsBase.Map.lookup_pat(Pat.rep_id(f_name), m)
    )
  ) {
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
    let rec add_spine = (pat: Pat.t, m) =>
      switch (IdTagged.term_of(pat)) {
      | Ap(fn, _) => add_spine(fn, add_for(pat, m))
      | _ => m
      };
    switch (IdTagged.term_of(user_pat)) {
    | Asc(inner, _) =>
      let m = add_for(user_pat, m);
      add_spine(inner, m);
    | _ => add_spine(user_pat, m)
    };
  };
};
