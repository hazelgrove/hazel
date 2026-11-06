open Util;

/*
     This module is a combination of a Ctx.t and a ClosureEnvironment.t
     It is used in places where we need to associate both types and values
     with variables, such as in the stepper.

     Hypotheses live only in the theorem namespace of `ctx`
     (`Ctx.TheoremEntry`), never in `env`. The prover still needs `env`:
     obligations and facts are env-substituted before discharge.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ctx: Ctx.t,
  env: Environment.t(Exp.t),
};

let of_ctx_and_env: (Ctx.t, Environment.t(Exp.t)) => t =
  (ctx, env) => {
    ctx,
    env,
  };

/* Pairs a `Ctx.t` from the statics with the runtime environment at the
   same program point.

   Theorem statements recorded by the statics are in the user's
   vocabulary (`f(2) == 3` where `f` is a let-bound function), while the
   checker computes with env-inlined terms (`Substitution.in_exp`), so the
   statements are substituted here, once.

   Unlike `of_ctx_and_env`, this substitutes: facts installed later by
   `add_hypothesis` are substituted by their caller, and substituting one
   a second time alpha-renames the binders inside its inlined closures,
   after which by-name citation no longer finds it. */
let of_program_state = (ctx: Ctx.t, env: Environment.t(Exp.t)): t => {
  ctx: {
    ...ctx,
    entries:
      List.map(
        (entry: Ctx.entry) =>
          switch (entry) {
          | TheoremEntry({prop: Some(prop), _} as e) =>
            Ctx.TheoremEntry({
              ...e,
              prop: Some(prop |> Substitution.in_exp(env)),
            })
          | e => e
          },
        ctx.entries,
      ),
  },
  env,
};

/* `ctx` with the variables `dhpat` binds, at their types when `dhpat`
   has type `ty`. `None` when `dhpat` can't have that type. */
let bind_pattern = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): option(Ctx.t) => {
  open OptUtil.Syntax;
  let rec dhpat_var_entry =
          (dhpat: DHPat.t, ty: Typ.t): option(list(Ctx.entry)) => {
    let ty' = ty;
    let ty =
      switch (ty.term) {
      | TupLabel(_, ty) => ty
      | _ => ty
      };
    switch (dhpat |> Pat.term_of) {
    | Var(name) =>
      let entry =
        Ctx.VarEntry({
          name,
          id: Id.invalid,
          typ: ty,
          custom_statics: None,
        });
      Some([entry]);
    | Label(name) =>
      Typ.equal(ty, Label(name) |> Typ.temp) ? Some([]) : None
    | ExplicitNonlabel =>
      raise(
        Failure(
          "bind_pattern ExplicitNonlabel shouldn't show up since they should only show up in in tuplabels below",
        ),
      )
    | TupLabel({term: ExplicitNonlabel, _}, dhpat) =>
      dhpat_var_entry(dhpat, ty)
    | TupLabel(_, dp1) =>
      switch (ty'.term) {
      | TupLabel(_, ty2)
          when
            LabeledTuple.has_same_labels(
              DHPat.match_tup_label(dhpat),
              Typ.match_tup_label(ty'),
            ) =>
        dhpat_var_entry(dp1, ty2)
      | TupLabel(_, _) => None
      | _ => dhpat_var_entry(dp1, ty)
      }
    | Tuple(l1) =>
      let (l1, ts) =
        MatchedTyp.prod(ctx, l1, Pat.match_tup_label, ty, (name, b) =>
          TupLabel(Label(name) |> Pat.fresh, b) |> Pat.fresh
        );
      let* l =
        List.map2((dhp, typ) => {dhpat_var_entry(dhp, typ)}, l1, ts)
        |> OptUtil.sequence;
      Some(List.concat(l));
    | Cons(dhp1, dhp2) =>
      let* t = MatchedTyp.list_strict(ctx, ty);
      let* l1 = dhpat_var_entry(dhp1, t);
      let* l2 = dhpat_var_entry(dhp2, List(t) |> Typ.temp);
      Some(l1 @ l2);
    | ListLit(l) =>
      let* t = MatchedTyp.list_strict(ctx, ty);
      let* l =
        List.map(dhp => {dhpat_var_entry(dhp, t)}, l) |> OptUtil.sequence;
      Some(List.concat(l));
    | Ap({term: Constructor(name, _), _}, dhp) =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty);
      let* typ = ConstructorMap.get_entry(name, ctrs);
      let* typ' = typ;
      dhpat_var_entry(dhp, typ');
    | Ap(_) => None
    | EmptyHole
    | Wild
    | Invalid(_)
    | MultiHole(_) => Some([])
    | Parens(dhp)
    | Projector(_, dhp) => dhpat_var_entry(dhp, ty)
    | Atom(c) =>
      Typ.equal(ty, Atom(Atom.cls_of_t(c)) |> Typ.temp) ? Some([]) : None
    | Constructor(_) => Some([]) // TODO: make this stricter
    | Asc(dhp, ty1) => dhpat_var_entry(dhp, ty1)
    /* `p where g` binds what `p` binds at the same type. */
    | Where(dhp, _) => dhpat_var_entry(dhp, ty)
    };
  };
  let+ l = dhpat_var_entry(dhpat, ty);
  List.fold_left((ctx, entry) => Ctx.extend(ctx, entry), ctx, l);
};

let add_from_pattern = ({ctx, env}: t, pattern: Pat.t, pat_typ: Typ.t) => {
  let ctx =
    bind_pattern(pattern, pat_typ, ctx) |> Option.value(~default=ctx);
  let env =
    List.fold_left(
      Environment.extend,
      env,
      List.map(
        v => (v, Exp.fresh(Var(v))),
        pattern |> Pat.bindings |> Binding.variable_names,
      ),
    );
  {
    ctx,
    env,
  };
};

/* A hypothesis goes only in the theorem namespace: not in the variable
   namespace (no `Ctx.VarEntry`, so its name is free as far as the
   statics are concerned) and not in `env` (it is not a value, so
   substitution and evaluation never see it).

   The name is used as given, never freshened: an inner `case_eq`
   shadows the outer one rather than becoming `case_eq'`. Citation
   resolves innermost-first because `Ctx.extend` prepends and
   `Ctx.lookup_theorem` takes the first match. To keep a shadowed fact
   citable, name it at introduction (`induction e as h`, `assume e as h`)
   or re-bind it with `alias h = <fact> => ...`. */
let add_hypothesis = (t: t, name: Var.t, hyp: Exp.t): (t, Binding.t) => {
  let id = Id.mk();
  (
    {
      ...t,
      ctx:
        Ctx.extend_theorem(
          t.ctx,
          {
            name,
            id,
            prop: Some(hyp),
          },
        ),
    },
    {
      id,
      name,
    },
  );
};

/* The facts visible in this scope, innermost first: the theorem-namespace
   entries whose statement is known. Built-in axioms (`prop: None`) are
   supplied separately as `Axioms.v` rule builtins. */
let facts = (t: t): list((Var.t, Id.t, Exp.t)) =>
  Ctx.get_theorem_entries(t.ctx)
  |> List.filter_map((e: Ctx.theorem_entry) =>
       switch (e.prop) {
       | Some(prop) => Some((e.name, e.id, prop))
       | None => None
       }
     );

let get_ctx = (t: t): Ctx.t => t.ctx;
let get_env = (t: t): Environment.t(Exp.t) => t.env;
