open TyDiSuggestion;
open Language;

/* For suggestions in patterns, suggest variables which
 * occur free in that pattern's scope. */
let free_variables =
    (expected_ty: Typ.t, ctx: Ctx.t, co_ctx: CoCtx.t): list(TyDiSuggestion.t) => {
  List.filter_map(
    ((name, entries)) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | None =>
        let meet_use_typ = CoCtx.meet(ctx, entries);
        if (Typ.is_consistent(ctx, expected_ty, meet_use_typ)) {
          Some({
            content: name,
            strategy: Pat(FromCoCtx(meet_use_typ)),
          });
        } else {
          None;
        };
      | Some(_) => None
      },
    co_ctx,
  );
};

/* For suggestsions in expressions, suggest variables from the ctx */
let bound_variables = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({
        content: name,
        strategy: Exp(Common(FromCtx(typ))),
      })
    | _ => None,
    ctx.entries,
  );

let bound_livelits = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.LivelitEntry({expansion_t, name, _})
        when Typ.is_consistent(ctx, ty_expect, expansion_t) =>
      Some({
        content: "^" ++ name,
        strategy: Exp(Common(FromCtx(expansion_t))),
      })
    | _ => None,
    ctx.entries,
  );

let bound_constructors =
    (wrap: strategy_common => strategy, ty: Typ.t, ctx: Ctx.t)
    : list(TyDiSuggestion.t) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty, typ) =>
      Some({
        content: name,
        strategy: wrap(FromCtx(typ)),
      })
    | _ => None,
    ctx.entries,
  );

/* Suggest applying a function from the ctx which returns an appropriate type */
let bound_aps = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
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
    ctx.entries,
  );

let bound_constructor_aps =
    (wrap, ty: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
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
      Some({
        content: name ++ "(",
        strategy: wrap(FromCtxAp(ty_out)),
      })
    | _ => None,
    ctx.entries,
  );

/* The named fields of a labeled tuple type or the value members of a
 * signature type, with their types (a signature's own manifest type members
 * are substituted into its member types). */
let named_fields = (ctx: Ctx.t, typ: Typ.t): list((string, Typ.t)) =>
  switch (Typ.normalize(ctx, typ) |> Typ.term_of) {
  | Prod(ts) => List.filter_map(Typ.match_tup_label, ts)
  | Sig(items) =>
    Sig.members(items)
    |> Sig.dedup_last
    |> List.filter_map((m: Sig.member) =>
         switch (m) {
         | Val(label, _) =>
           Typ.sig_project_value(items, label)
           |> Option.map(field_ty => (label, field_ty))
         | TypeManifest(_)
         | TypeAbstract(_) => None
         }
       )
  | _ => []
  };

/* Suggest qualified member access: for variables with labeled tuple or
 * module types, suggest Name.label for fields consistent with the expected
 * type. E.g., if String has type { let empty : String; let length : String
 * -> Int } and we expect String, suggest "String.empty".
 *
 * TODO: Only goes one level deep. Nested qualified access (A.B.x) would
 * require recursive expansion. See also: List(Prod) types could generate
 * qualified suggestions where field types are wrapped in List(...). */
let bound_qualified = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.concat_map(
    fun
    | Ctx.VarEntry({typ, name, _}) =>
      named_fields(ctx, typ)
      |> List.filter_map(((label, field_ty)) =>
           Typ.is_consistent(ctx, ty_expect, field_ty)
             ? Some(
                 TyDiSuggestion.{
                   content: name ++ "." ++ label,
                   strategy: Exp(Common(FromCtx(field_ty))),
                 },
               )
             : None
         )
    | _ => [],
    ctx.entries,
  );

/* Like bound_qualified but for arrow-typed fields: suggest Name.label(
 * when the field's return type is consistent with the expected type.
 * E.g., if String has (length=String->Int) and we expect Int,
 * suggest "String.length(". */
let bound_qualified_aps =
    (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.concat_map(
    fun
    | Ctx.VarEntry({typ, name, _}) =>
      named_fields(ctx, typ)
      |> List.filter_map(((label, field_ty: Typ.t)) =>
           switch (field_ty.term) {
           | Arrow(_, ty_out)
               when
                 Typ.is_consistent(ctx, ty_expect, ty_out)
                 && !Typ.is_consistent(ctx, ty_expect, field_ty) =>
             Some(
               TyDiSuggestion.{
                 content: name ++ "." ++ label ++ "(",
                 strategy: Exp(Common(FromCtxAp(ty_out))),
               },
             )
           | _ => None
           }
         )
    | _ => [],
    ctx.entries,
  );

/* Suggest bound type aliases in type annotations or definitions */
let typ_context_entries = (ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.TVarEntry({kind: Singleton(_), name, _}) =>
      Some({
        content: name,
        strategy: Typ(FromCtx),
      })
    | _ => None,
    ctx.entries,
  );

/* NOTE(perf): suggest_variable and suggest_lookahead_variable each iterate
 * over ctx.entries multiple times (currently ~7 passes in suggest_variable,
 * up to ~33 in lookahead worst case for Bool). At typical context sizes
 * (<500 entries) this is negligible. If it becomes a bottleneck, the main
 * optimization is a single-pass refactor that classifies entries into
 * buckets in one traversal, and/or pre-caching results for the fixed
 * builtin context. */
let suggest_variable = (ci: Info.t): list(TyDiSuggestion.t) => {
  let ctx = Info.ctx_of(ci);
  let ctx = Ctx.filter_shadowed(ctx); /* Remove shadowing */
  switch (ci) {
  | InfoExp({ana, _}) =>
    bound_variables(ana, ctx)
    @ bound_livelits(ana, ctx)
    @ bound_aps(ana, ctx)
    @ bound_qualified(ana, ctx)
    @ bound_qualified_aps(ana, ctx)
    @ bound_constructors(x => Exp(Common(x)), ana, ctx)
    @ bound_constructor_aps(x => Exp(Common(x)), ana, ctx)
  | InfoPat({ana, co_ctx, _}) =>
    free_variables(ana, ctx, co_ctx)
    @ bound_constructors(x => Pat(Common(x)), ana, ctx)
    @ bound_constructor_aps(x => Pat(Common(x)), ana, ctx)
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

let suggest_lookahead_variable = (ci: Info.t): list(TyDiSuggestion.t) => {
  let restrategize = (suffix, {content, strategy}) => {
    content: content ++ suffix,
    strategy,
  };
  let ctx = Info.ctx_of(ci);
  let ctx = Ctx.filter_shadowed(ctx); /* Remove shadowing */
  switch (ci) {
  | InfoExp({ana, _}) =>
    let exp_refs = ty =>
      bound_variables(ty, ctx)
      @ bound_qualified(ty, ctx)
      @ bound_constructors(x => Exp(Common(x)), ty, ctx);
    let exp_aps = ty =>
      bound_aps(ty, ctx)
      @ bound_qualified_aps(ty, ctx)
      @ bound_constructor_aps(x => Exp(Common(x)), ty, ctx);
    switch (ana |> Typ.term_of) {
    | List(ty) =>
      List.map(restrategize(" )::"), exp_aps(ty))
      @ List.map(restrategize("::"), exp_refs(ty))
    | Prod([ty, ...tys]) =>
      let commas =
        List.init(List.length(tys), _ => ",") |> String.concat(" ");
      List.map(restrategize(" )" ++ commas), exp_aps(ty))
      @ List.map(restrategize(commas), exp_refs(ty));
    | Atom(Bool) =>
      /* TODO: Find a UI to make these less confusing */
      exp_refs(Atom(Int) |> Typ.fresh)
      @ exp_refs(Atom(SInt) |> Typ.fresh)
      @ exp_refs(Atom(Nat) |> Typ.fresh)
      @ exp_refs(Atom(Float) |> Typ.fresh)
      @ exp_refs(Atom(String) |> Typ.fresh)
      @ exp_aps(Atom(Int) |> Typ.fresh)
      @ exp_aps(Atom(Float) |> Typ.fresh)
      @ exp_aps(Atom(String) |> Typ.fresh)
    | _ => []
    };
  | InfoPat({ana, co_ctx, _}) =>
    let pat_refs = ty =>
      free_variables(ty, ctx, co_ctx)
      @ bound_constructors(x => Pat(Common(x)), ty, ctx);
    let pat_aps = ty => bound_constructor_aps(x => Pat(Common(x)), ty, ctx);
    switch (ana |> Typ.term_of) {
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
