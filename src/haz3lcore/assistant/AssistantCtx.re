open Suggestion;

/* For suggestions in patterns, suggest variables which
 * occur free in that pattern's scope. */
let suggest_free_var =
    (expected_ty: Typ.t, ctx: Ctx.t, co_ctx: CoCtx.t): list(Suggestion.t) => {
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

let rec deep_consistent = (ctx: Ctx.t, ty_target, ty: Typ.t) =>
  /* if ty is Arrow(_, Arrow(_, ... ty_target)) return true
   */
  switch (ty) {
  | _ when Typ.is_consistent(ctx, ty, ty_target) => true
  | Arrow(_, ty_out) => deep_consistent(ctx, ty_target, ty_out)
  | _ => false
  };

/* For suggestsions in expressions, suggest variables from the ctx */
let suggest_bound_var =
    (~fns: bool, ty_expect: Typ.t, ctx: Ctx.t): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({content: name, strategy: Exp(Common(FromCtx(typ)))})
    | Ctx.VarEntry({typ: Arrow(_) as typ, name, _})
        when fns && deep_consistent(ctx, ty_expect, typ) =>
      Some({content: name, strategy: Exp(Common(FromCtx(typ)))})
    | _ => None,
    ctx,
  );

let suggest_bound_ctr =
    (~fns: bool, wrap: strategy_common => strategy, ty: Typ.t, ctx: Ctx.t)
    : list(Suggestion.t) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty, typ) =>
      Some({content: name, strategy: wrap(FromCtx(typ))})
    | Ctx.ConstructorEntry({typ: Arrow(_) as typ, name, _})
        when fns && deep_consistent(ctx, ty, typ) =>
      Some({content: name, strategy: wrap(FromCtx(typ))})
    | _ => None,
    ctx,
  );

/* Suggest applying a function from the ctx which returns an appropriate type */
let bound_aps = (ty_expect: Typ.t, ctx: Ctx.t): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
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

let bound_constructor_aps = (wrap, ty: Typ.t, ctx: Ctx.t): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty, ty_out)
          && !Typ.is_consistent(ctx, ty, ty_arr) =>
      Some({content: name ++ "(", strategy: wrap(FromCtxAp(ty_out))})
    | _ => None,
    ctx,
  );

/* Suggest bound type aliases in type annotations or definitions */
let suggest_bound_typ = (ctx: Ctx.t): list(Suggestion.t) =>
  List.filter_map(
    fun
    | Ctx.TVarEntry({kind: Singleton(_), name, _}) =>
      Some({content: name, strategy: Typ(FromCtx)})
    | _ => None,
    ctx,
  );

let suggest_bound_exp = (~fns, ty: Typ.t, ctx: Ctx.t): list(Suggestion.t) => {
  suggest_bound_var(~fns, ty, ctx)
  @ suggest_bound_ctr(~fns, x => Exp(Common(x)), ty, ctx);
};

let suggest_bound_pat = (~fns, ty: Typ.t, ctx: Ctx.t): list(Suggestion.t) => {
  suggest_bound_ctr(~fns, x => Pat(Common(x)), ty, ctx);
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

let restrategize = (suffix, {content, strategy}) => {
  content: content ++ suffix,
  strategy,
};

let rec get_lookahead_tys_pat = (ctx: Ctx.t, ty_expected: Typ.t): list(Typ.t) => {
  let to_arr = t => Typ.Arrow(Unknown(Internal), t);
  let ty_expected = Typ.normalize(ctx, ty_expected);
  [to_arr(ty_expected)]
  @ (
    switch (ty_expected) {
    | List(ty)
    | Prod([ty, ..._]) => [ty, to_arr(ty)] @ get_lookahead_tys_pat(ctx, ty)
    | _ => []
    }
  );
};

open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type type_path = list(list(Typ.t));

let show_type_path = (paths: type_path): string =>
  paths
  |> List.map(tys =>
       tys |> List.rev |> List.map(Typ.to_string) |> String.concat(" <= ")
     )
  |> String.concat("\n");

let rec get_lookahead_tys_exp =
        (ctx: Ctx.t, ty_expected: Typ.t): list(list(Typ.t)) => {
  //let to_arr = t => Typ.Arrow(Unknown(Internal), t);
  //TODO(andrew): also ?->(?->t), etc.
  /* Interesting that this doesn't blow up due to anonymous functions due
     to precedence: can't start an opseq with a fun expecting later to apply it.
     (but if you drop a parens then it blows up).
     so gotta prohibit anon funs in fun pos, or maybe more generally beta
     reducible things.
     (need to make sure logic also prohibits eg "(((fun x -> x)))(1)")
     If we do this, then i think blowup of error case is limited by
     the most right-nested arrow type in the context, or maybe more
     straightforwardly, if we abstracte arr out of this fun, it would
     suffice to search ctx for things of one of these types, or arrow types
     which eventually left-terminate at one of these types.
     actually maybe anon funs are fine here, they dont screw things up until
     you actually use one, unlike for reverse. */
  [[ty_expected]]  //, to_arr(ty_expected)]]
  @ (
    switch (Typ.normalize(ctx, ty_expected)) {
    | List(ty)
    | Prod([ty, ..._]) =>
      [[ty_expected, ty]]
      //@ [[ty_expected, ty, to_arr(ty)]]
      @ List.map(
          tys => [ty_expected, ...tys],
          get_lookahead_tys_exp(ctx, ty),
        )
    | Bool =>
      //TODO(andrew): relax this given polymorphic equality
      let from_bool = t => [[ty_expected, t]]; // @ [[ty_expected, to_arr(t)]];
      from_bool(Int) @ from_bool(Float) @ from_bool(String);
    | _ => []
    }
  );
};

let suggest_lookahead_variable_pat =
    (ty_expected: Typ.t, ctx: Ctx.t, co_ctx: CoCtx.t): list(Suggestion.t) => {
  let pat_refs = ty =>
    suggest_free_var(ty, ctx, co_ctx)
    @ suggest_bound_ctr(~fns=true, x => Pat(Common(x)), ty, ctx);
  let pat_aps = ty => bound_constructor_aps(x => Pat(Common(x)), ty, ctx);
  let ty_expected = Typ.normalize(ctx, ty_expected);
  let from_current_type = pat_aps(ty_expected);
  let from_specific_type =
    switch (ty_expected) {
    | List(ty) =>
      /*List.map(restrategize(" )::"), pat_aps(ty))
        @ List.map(restrategize("::"), pat_refs(ty))*/
      pat_aps(ty)
      @ List.map(restrategize("::"), pat_refs(ty))
      @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty)
    //TODO(andrew): hack for LSP
    | Prod([ty, ..._tys]) =>
      /*let commas =
          List.init(List.length(tys), _ => ",") |> String.concat(" ");
        List.map(restrategize(" )" ++ commas), pat_aps(ty))
        @ List.map(restrategize(commas), pat_refs(ty));*/
      pat_aps(ty)
      @ pat_refs(ty)
      @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty)
    //TODO(andrew): hack for LSP
    | _ => []
    };
  from_current_type @ from_specific_type;
};

let suggest_lookahead_variable_exp =
    (ty_expected: Typ.t, ctx: Ctx.t): list(Suggestion.t) => {
  let exp_refs = ty =>
    suggest_bound_pat(~fns=true, ty, ctx)
    @ suggest_bound_ctr(~fns=true, x => Exp(Common(x)), ty, ctx);
  let exp_aps = ty =>
    bound_aps(ty, ctx)
    @ bound_constructor_aps(x => Exp(Common(x)), ty, ctx);
  let ty_expected = Typ.normalize(ctx, ty_expected);
  let from_current_type = exp_aps(ty_expected);
  let from_specific_type =
    switch (ty_expected) {
    | List(ty) =>
      /* List.map(restrategize(" )::"), exp_aps(ty))
         @ List.map(restrategize("::"), exp_refs(ty))*/
      exp_aps(ty)
      @ List.map(restrategize("::"), exp_refs(ty))
      @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty)
    //TODO(andrew): hack for LSP
    | Prod([ty, ..._tys]) =>
      /*let commas =
          List.init(List.length(tys), _ => ",") |> String.concat(" ");
        List.map(restrategize(" )" ++ commas), exp_aps(ty))
        @ List.map(restrategize(commas), exp_refs(ty));*/
      exp_aps(ty)
      @ exp_refs(ty)
      @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty)
    //TODO(andrew): hack for LSP
    | Bool =>
      /* TODO: Find a UI to make these less confusing */
      exp_refs(Int)
      @ exp_refs(Float)
      @ exp_refs(String)
      @ exp_aps(Int)
      @ exp_aps(Float)
      @ exp_aps(String)
    | _ => []
    };
  from_current_type @ from_specific_type;
};

let suggest_variable = (ci: Info.t): list(Suggestion.t) =>
  switch (ci) {
  | InfoExp({mode, ctx, _}) =>
    suggest_bound_exp(~fns=false, Mode.ty_of(mode), ctx)
  | InfoPat({mode, ctx, co_ctx, _}) =>
    let ty = Mode.ty_of(mode);
    suggest_free_var(ty, ctx, co_ctx)
    @ suggest_bound_pat(~fns=false, ty, ctx);
  | InfoTyp({ctx, _}) => suggest_bound_typ(ctx)
  | _ => []
  };

let suggest_lookahead_variable = (ci: Info.t): list(Suggestion.t) =>
  switch (ci) {
  | InfoExp({mode, ctx, _}) =>
    let ty = Mode.ty_of(mode);
    suggest_lookahead_variable_exp(ty, ctx);
  | InfoPat({mode, ctx, co_ctx, _}) =>
    let ty = Mode.ty_of(mode);
    suggest_lookahead_variable_pat(ty, ctx, co_ctx);
  | InfoTyp(_) => []
  | _ => []
  };
