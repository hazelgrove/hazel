open Suggestion;

/* For suggestions in patterns, suggest variables which
 * occur free in that pattern's scope. */
let free_variables =
    (
      expected_ty: Typ.t(IdTag.t),
      ctx: Ctx.t(IdTag.t),
      co_ctx: CoCtx.t(IdTag.t),
    )
    : list(Suggestion.t) => {
  List.filter_map(
    ((name, entries)) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | None =>
        let joint_use_typ = CoCtx.join(ctx, entries);
        if (Typ.is_consistent(ctx, expected_ty, joint_use_typ)) {
          Some({content: name, strategy: Pat(FromCoCtx(joint_use_typ))});
        } else {
          None;
        };
      | Some(_) => None
      },
    co_ctx,
  );
};

/* For suggestsions in expressions, suggest variables from the ctx */
let bound_variables =
    (ty_expect: Typ.t(IdTag.t), ctx: Ctx.t(IdTag.t)): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({content: name, strategy: Exp(Common(FromCtx(typ)))})
    | _ => None,
    ctx,
  );

let bound_constructors =
    (
      wrap: strategy_common => strategy,
      ty: Typ.t(IdTag.t),
      ctx: Ctx.t(IdTag.t),
    )
    : list(Suggestion.t) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty, typ) =>
      Some({content: name, strategy: wrap(FromCtx(typ))})
    | _ => None,
    ctx,
  );

/* Suggest applying a function from the ctx which returns an appropriate type */
let bound_aps =
    (ty_expect: Typ.t(IdTag.t), ctx: Ctx.t(IdTag.t)): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ: {term: Arrow(_, ty_out), _} as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty_expect, ty_out)
          && !Typ.is_consistent(ctx, ty_expect, ty_arr) => {
        Some({
          content: name ++ "(",
          strategy: Exp(Common(FromCtxAp(ty_out))),
        });
      }
    | _ => None,
    ctx,
  );

let bound_constructor_aps =
    (wrap, ty: Typ.t(IdTag.t), ctx: Ctx.t(IdTag.t)): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({
        typ: {term: Arrow(_, ty_out), _} as ty_arr,
        name,
        _,
      })
        when
          Typ.is_consistent(ctx, ty, ty_out)
          && !Typ.is_consistent(ctx, ty, ty_arr) =>
      Some({content: name ++ "(", strategy: wrap(FromCtxAp(ty_out))})
    | _ => None,
    ctx,
  );

/* Suggest bound type aliases in type annotations or definitions */
let typ_context_entries = (ctx: Ctx.t(IdTag.t)): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.TVarEntry({kind: Singleton(_), name, _}) =>
      Some({content: name, strategy: Typ(FromCtx)})
    | _ => None,
    ctx,
  );

let suggest_variable = (ci: Info.t): list(Suggestion.t) => {
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    bound_variables(Mode.ty_of(mode), ctx)
    @ bound_aps(Mode.ty_of(mode), ctx)
    @ bound_constructors(x => Exp(Common(x)), Mode.ty_of(mode), ctx)
    @ bound_constructor_aps(x => Exp(Common(x)), Mode.ty_of(mode), ctx)
  | InfoPat({mode, co_ctx, _}) =>
    free_variables(Mode.ty_of(mode), ctx, co_ctx)
    @ bound_constructors(x => Pat(Common(x)), Mode.ty_of(mode), ctx)
    @ bound_constructor_aps(x => Pat(Common(x)), Mode.ty_of(mode), ctx)
  | InfoTyp(_) => typ_context_entries(ctx)
  | _ => []
  };
};

/* Suggest lookahead tokens:
 *
 * Sometimes the expected type is Ty, but we want to enter something of Ty'
 * because we're going to follow it up with an infix op of type (Ty', _) -> Ty.
 *
 * For now we special-case such situations instead of deriving them from the
 * grammar. In the current grammar there are basically 3 classes:
 *
 * 1. If bool is expected, could be int, float or string (comparisons)
 * 2. If list(ty) is expected, could be ty (cons)
 * 3. If tuple([ty, ...]) is expected, could be ty (comma)

 * 2 and 3 are the easiest to make ergonomic as there is only one such
 * infix op, so we can just combine the two tokens into a single completion.
 * 1 is slightly more fraught because as we either need to not show the
 * second token, or pick an arbitrary representative op to show, and we
 * probably wouldn't want to complete that op, forcing the user to backspace
 * if they meant another, so we'd need to implement staged completion.
 * For now we just don't show a second token, which can be slightly confusing.
 *
 */

let suggest_lookahead_variable = (ci: Info.t): list(Suggestion.t) => {
  let restrategize = (suffix, {content, strategy}) => {
    content: content ++ suffix,
    strategy,
  };
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    let exp_refs = ty =>
      bound_variables(ty, ctx)
      @ bound_constructors(x => Exp(Common(x)), ty, ctx);
    let exp_aps = ty =>
      bound_aps(ty, ctx)
      @ bound_constructor_aps(x => Exp(Common(x)), ty, ctx);
    switch (Mode.ty_of(mode) |> Typ.term_of) {
    | List(ty) =>
      List.map(restrategize(" )::"), exp_aps(ty))
      @ List.map(restrategize("::"), exp_refs(ty))
    | Prod([ty, ...tys]) =>
      let commas =
        List.init(List.length(tys), _ => ",") |> String.concat(" ");
      List.map(restrategize(" )" ++ commas), exp_aps(ty))
      @ List.map(restrategize(commas), exp_refs(ty));
    | Bool =>
      /* TODO: Find a UI to make these less confusing */
      exp_refs(Int |> Typ.fresh)
      @ exp_refs(Float |> Typ.fresh)
      @ exp_refs(String |> Typ.fresh)
      @ exp_aps(Int |> Typ.fresh)
      @ exp_aps(Float |> Typ.fresh)
      @ exp_aps(String |> Typ.fresh)
    | _ => []
    };
  | InfoPat({mode, co_ctx, _}) =>
    let pat_refs = ty =>
      free_variables(ty, ctx, co_ctx)
      @ bound_constructors(x => Pat(Common(x)), ty, ctx);
    let pat_aps = ty => bound_constructor_aps(x => Pat(Common(x)), ty, ctx);
    switch (Mode.ty_of(mode) |> Typ.term_of) {
    | List(ty) =>
      List.map(restrategize(" )::"), pat_aps(ty))
      @ List.map(restrategize("::"), pat_refs(ty))
    | Prod([ty, ...tys]) =>
      let commas =
        List.init(List.length(tys), _ => ",") |> String.concat(" ");
      List.map(restrategize(" )" ++ commas), pat_aps(ty))
      @ List.map(restrategize(commas), pat_refs(ty));
    | _ => []
    };
  | InfoTyp(_) => []
  | _ => []
  };
};
