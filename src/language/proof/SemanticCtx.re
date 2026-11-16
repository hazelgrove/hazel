open Util;

/*
     This module is a combination of a Ctx.t and a ClosureEnvironment.t
     It is used in places where we need to associate both types and values
     with variables, such as in the stepper.

     Hypotheses live only in the theorem namespace of `ctx`
     (`Ctx.TheoremEntry`), never in `env`. Goals and facts name the
     program's definitions, and `env` is what those names mean
     (`Definitions`).
 */

/* What checking a theorem's proof found, for the proofs that cite it. */
[@deriving (show({with_path: false}), sexp, yojson)]
type verdict =
  | Proven
  /* Neither proven nor refuted, or proven only modulo open obligations:
   * a proof citing it rests on its statement. */
  | Unfinished
  | Refuted;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ctx: Ctx.t,
  env: Environment.t(Exp.t),
  /* The verdicts of the theorems checked before this proof, by the id of
     the name each binds. A hypothesis has none. */
  verdicts: list((Id.t, verdict)),
  /* Theorems in `ctx` that are about a variable since rebound by the
   * program, which `ctx` has no binder for. */
  captured: list(Id.t),
};

let of_ctx_and_env = (~captured=[], ctx: Ctx.t, env: Environment.t(Exp.t)): t => {
  ctx,
  env,
  verdicts: [],
  captured,
};

/* `t` with the types, constructors and variables of `typing` under its
 * own entries, for typing an expression in its scope. The facts stay
 * `t`'s. */
let with_typing = (typing: Ctx.t, t: t): t => {
  ...t,
  ctx: {
    ...t.ctx,
    entries:
      t.ctx.entries
      @ List.filter(
          ~f=
            fun
            | Ctx.TheoremEntry(_) => false
            | VarEntry(_)
            | ConstructorEntry(_)
            | TVarEntry(_)
            | LivelitEntry(_) => true,
          typing.entries,
        ),
  },
};

let with_verdicts = (verdicts: list((Id.t, verdict)), t: t): t => {
  ...t,
  verdicts,
};

let map_ctx = (f: Ctx.t => Ctx.t, t: t): t => {
  ...t,
  ctx: f(t.ctx),
};

/* The verdict of the theorem with this id, None for a hypothesis. */
let verdict = (t: t, id: Id.t): option(verdict) =>
  List.Assoc.find(t.verdicts, id, ~equal=Id.equal);

/* The verdict of the theorem `name` cites, None for a hypothesis or an
   axiom. */
let cited_verdict = (t: t, name: Var.t): option(verdict) =>
  Ctx.lookup_theorem(t.ctx, name)
  |> Option.bind(_, ~f=(e: Ctx.theorem_entry) => verdict(t, e.id));

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
        List.map2_exn(l1, ts, ~f=(dhp, typ) => {dhpat_var_entry(dhp, typ)})
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
        List.map(~f=dhp => {dhpat_var_entry(dhp, t)}, l) |> OptUtil.sequence;
      Some(List.concat(l));
    | Ap(f, dhp) =>
      let* name = Pat.ctr_name(f);
      let* ctrs = Typ.get_sum_constructors(ctx, ty);
      let* typ = ConstructorMap.get_entry(name, ctrs);
      let* typ' = typ;
      dhpat_var_entry(dhp, typ');
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
  List.fold_left(~f=(ctx, entry) => Ctx.extend(ctx, entry), ~init=ctx, l);
};

let add_from_pattern =
    ({ctx, env, _} as t: t, pattern: Pat.t, pat_typ: Typ.t) => {
  let ctx =
    bind_pattern(pattern, pat_typ, ctx) |> Option.value(~default=ctx);
  let env =
    List.fold_left(
      ~f=Environment.extend,
      ~init=env,
      List.map(
        ~f=v => (v, Exp.fresh(Var(v))),
        pattern |> Pat.bindings |> Binding.variable_names,
      ),
    );
  {
    ...t,
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
   citable, name it at introduction (`assume h = e`, `have h = e`)
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
  |> List.filter_map(~f=(e: Ctx.theorem_entry) =>
       switch (e.prop) {
       | Some(prop) => Some((e.name, e.id, prop))
       | None => None
       }
     );

let get_ctx = (t: t): Ctx.t => t.ctx;
let get_env = (t: t): Environment.t(Exp.t) => t.env;
