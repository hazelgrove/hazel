open Util;

/*
     This module is a combination of a Ctx.t and a ClosureEnvironment.t
     It is used in places where we need to associate both types and values
     with variables, such as in the stepper.

     NOTE (2026-08-21, proof/value namespace separation): this pair used
     to carry proofs in BOTH halves — a hypothesis was a `Ctx.VarEntry`
     typed `ProofOf(fact)` AND an `Environment` binding to
     `ProofObject(fact)`. It no longer does: proofs live only in the
     theorem namespace of `ctx` (`Ctx.TheoremEntry`), so this pair is
     back to its original job of relating TYPES and VALUES of program
     variables. The `env` half is still genuinely needed — obligations
     and facts are env-substituted before discharge — so the pair does
     not collapse; it just stopped being the proof carrier.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ctx: Ctx.t,
  env: Environment.t(Exp.t),
};

let free_name = (t: t, base: Var.t): Var.t =>
  Var.free_name(
    base,
    List.map((e: Ctx.var_entry) => e.name, Ctx.get_var_entries(t.ctx)),
  );

let of_ctx_and_env: (Ctx.t, Environment.t(Exp.t)) => t =
  (ctx, env) => {
    ctx,
    env,
  };

/* Entry point from a PROGRAM STATE: a `Ctx.t` produced by the statics
   paired with the runtime environment at the same point.

   Theorem statements recorded by the statics are written in the user's
   vocabulary (`f(2) == 3` where `f` is a let-bound function), while the
   checker computes with env-inlined terms — goals, targets and
   obligations all go through `Substitution.in_exp` — so the statements
   are brought into the same form here, ONCE. (Before the namespace
   separation this happened in `Transition`'s Theorem case, which bound
   `ProofObject(Substitution.in_exp(env, e))`.)

   Deliberately distinct from `of_ctx_and_env`: facts installed later by
   `add_hypothesis` are substituted by their caller, and substituting one
   a second time alpha-renames the binders inside its inlined closures,
   after which by-name citation no longer finds it. So only this
   entry-point conversion substitutes. */
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

let add_entry: (t, Id.t, Var.t, Typ.t, option(Exp.t)) => t =
  ({ctx, env}, id, name, typ, value) => {
    let ctx =
      Ctx.extend(
        ctx,
        Ctx.VarEntry({
          name,
          id,
          typ,
          custom_statics: None,
        }),
      );
    let value = OptUtil.get(() => Exp.fresh(Var(name)), value);
    let env = env |> Environment.extend(_, (name, value));
    {
      ctx,
      env,
    };
  };

let add_entry_free_name =
    (t: t, name: Var.t, typ: Typ.t, value: option(Exp.t)): (t, Binding.t) => {
  let name = free_name(t, name);
  let id = Id.mk();
  let t = add_entry(t, id, name, typ, value);
  (
    t,
    {
      id,
      name,
    },
  );
};

let add_from_pattern = ({ctx, env}: t, pattern: Pat.t, pat_typ: Typ.t) => {
  let ctx =
    ProofHacks.dhpat_extend_ctx(pattern, pat_typ, ctx)
    |> Option.value(~default=ctx);
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

/* Install a hypothesis in the THEOREM namespace.

   Nothing is added to the VARIABLE namespace: there is no `Ctx.VarEntry`
   and no `ProofOf` type, so a hypothesis name is a free variable as far
   as the statics and evaluation of expressions are concerned. A
   hypothesis is a judgment, never a value.

   Nothing is added to the ENVIRONMENT either: a hypothesis is not a
   value, so it is invisible to `Substitution.in_exp` and to evaluation.

   The auto-name is freshened against the theorem names only
   (`Ctx.theorem_names`), matching `Statics`; a program variable called
   `assume` no longer bumps the hypothesis to `assume'`. */
/* The name `add_hypothesis` would pick for `base` in this scope. Exposed
   separately so a caller installing SEVERAL entries for ONE hypothesis
   (ProofCheck.add_where_facts, which also installs a conjunctive guard's
   conjuncts) can put them all under that one name: the freshening is over
   the SET of occupied theorem names, so re-using a name keeps every later
   hypothesis's auto-name identical to what the statics predicted
   (`Statics.proof_ctx_of_goal`). */
let hypothesis_name = (t: t, base: Var.t): Var.t =>
  Var.free_name(base, Ctx.theorem_names(t.ctx));

/* Install under an EXACT name, no freshening. */
let add_hypothesis_named = (t: t, name: Var.t, hyp: Exp.t): (t, Binding.t) => {
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

let add_hypothesis = (t: t, name: Var.t, hyp: Exp.t): (t, Binding.t) =>
  add_hypothesis_named(t, hypothesis_name(t, name), hyp);

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
