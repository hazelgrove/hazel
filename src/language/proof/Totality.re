open Util;

/* TOTALITY.re — the structural-totality check (tier 1 of the two-tier
 * divergence gate, docs/prover-obligations.md §4.1).
 *
 * `check` conservatively decides "evaluating this expression cannot
 * diverge". Divergence is genuine ⊥ (§1.1): it is not expressible as a
 * boolean condition, so unlike domain errors it can never become an
 * obligation — the caller must REFUSE the step when this check fails.
 * Conservative = when unsure, fail with a reason.
 *
 * What passes:
 *   - literals, quantified variables (quantifiers range over total
 *     values, §1.3), constructors, tuples/lists;
 *   - lambdas themselves (they are values — it is their APPLICATION
 *     that forces the body, which is then checked with the arguments
 *     assumed total);
 *   - applications of visibly-total functions to total arguments;
 *   - builtin applications (every Hazel builtin terminates; their
 *     PARTIALITY is the domain scan's job, DomainConditions.re) —
 *     including int/float division, which terminates with `err`;
 *   - case/if with total scrutinee and all branches total.
 *
 * What fails:
 *   - any reachable `FixF` — general recursion. Recursive `let`s are
 *     elaborated to `FixF` (Statics.re let-elaboration), and evaluated
 *     environment values keep the `FixF` spine through
 *     `Substitution.in_exp`, so one syntactic check covers "fix",
 *     recursive lets, and references to recursive definitions alike.
 *     Recognizing TERMINATING recursion (structural recursion on strict
 *     subterms) is tier 2 — Phase 4; here recursion simply fails.
 *   - applications whose head is a variable with no visible definition
 *     (free/abstract functions);
 *   - anything else we cannot positively classify (holes, exotic
 *     forms) — conservative by construction.
 *
 * The expression is expected to be environment-substituted
 * (`Substitution.in_exp`) before calling: substitution inlines visible
 * definitions (so non-recursive `let f = fun ...` applications can be
 * checked through their bodies), erases closures, and leaves quantified
 * binders as bare `Var`s. */

[@deriving (show({with_path: false}), sexp, yojson)]
type reason = string;

let pat_name = (p: Pat.t): string =>
  switch (p |> Pat.bindings |> Binding.variable_names) {
  | [name, ..._] => name
  | [] => "_"
  };

/* Is `name` a builtin function (or constant) in scope? Builtin ctx
 * entries are installed with `Id.invalid` (BuiltinsUtil.
 * ctx_entry_of_builtin), which distinguishes them from user binders. */
let is_builtin = (ctx: Ctx.t, name: Var.t): bool =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => entry.id == Id.invalid
  | None => false
  };

let is_arrow_typed = (ctx: Ctx.t, name: Var.t): bool =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) =>
    switch (Typ.term_of(entry.typ)) {
    | Arrow(_) => true
    | _ => false
    }
  | None => false
  };

let check =
    (~info_map as _: Statics.Map.t, ~ctx: Ctx.t, exp: Exp.t)
    : result(unit, reason) => {
  let ( let* ) = (r: result(_, reason), f) =>
    switch (r) {
    | Ok(x) => f(x)
    | Error(_) as e => e
    };
  let rec go = (e: Exp.t): result(unit, reason) =>
    switch (e |> Exp.term_of) {
    /* Literals and other atomic values. */
    | Atom(_)
    | Constructor(_)
    | BuiltinFun(_)
    | Label(_)
    | ExplicitNonlabel
    | ListLit([])
    | DrvQuote(_) => Ok()
    /* A bare variable: after substitution these are quantified binders
     * (which range over total VALUES, §1.3) or free names whose env
     * entry is a self-map. An arrow-typed abstract function is only
     * refused when APPLIED (see `go_ap`) — as a value it is total —
     * but we follow the conservative reading and refuse arrow-typed
     * unknowns even bare. */
    | Var(x) =>
      if (is_arrow_typed(ctx, x) && !is_builtin(ctx, x)) {
        Error(
          "the function `"
          ++ x
          ++ "` is abstract here; cannot establish its evaluation terminates",
        );
      } else {
        Ok();
      }
    /* Lambdas are values; their bodies are checked at application.
     * A FunWhere is a function value like Fun (its guard has no
     * dynamic effect, Grammar.re). */
    | Fun(_)
    | FunWhere(_)
    | TypFun(_) => Ok()
    /* General recursion: refuse, naming the function (§4.1 tier 1;
     * tier-2 structural-recursion detection is Phase 4). */
    | FixF(p, _, _) =>
      Error(
        "the recursive function `"
        ++ pat_name(p)
        ++ "` may diverge (structural-recursion detection is not implemented)",
      )
    /* Transparent wrappers. */
    | Parens(e1)
    | Projector(_, e1)
    | Asc(e1, _)
    | Filter(_, e1)
    | Closure(_, e1)
    | TyAlias(_, _, e1)
    | Use(_, e1)
    | Test(e1) => go(e1)
    /* Strict compound values / operators: total iff the children are.
     * (BinOp partiality — 1/0 — is `err`, not ⊥: the domain scan's
     * job, not this check's.) */
    | ListLit(es)
    | Tuple(es) => go_all(es)
    | TupLabel(e1, e2)
    | Dot(e1, e2)
    | TupleExtension(e1, e2)
    | Cons(e1, e2)
    | ListConcat(e1, e2)
    | Seq(e1, e2)
    | HintedTest(e1, e2)
    | BinOp(_, e1, e2) =>
      let* _ = go(e1);
      go(e2);
    | UnOp(_, e1) => go(e1)
    /* Non-recursive let (recursive lets already carry a FixF in their
     * definiens after elaboration). */
    | Let(_, e1, e2) =>
      let* _ = go(e1);
      go(e2);
    /* case/if: total if the scrutinee and all branches are. */
    | If(e1, e2, e3) =>
      let* _ = go(e1);
      let* _ = go(e2);
      go(e3);
    | Match(scrut, rules) =>
      let* _ = go(scrut);
      go_all(List.map(snd, rules));
    | Ap(_, fn, arg) =>
      let* _ = go(arg);
      go_ap(fn);
    /* Everything else — holes, invalid text, deferrals, modules,
     * quoted derivations with holes, type applications, ... — is
     * conservatively refused. */
    | _ => Error("cannot establish that this expression terminates")
    }
  /* The head of an application. Arguments were already checked total,
   * so a lambda head reduces to its body with total values bound —
   * check the body. */
  and go_ap = (fn: Exp.t): result(unit, reason) =>
    switch (fn |> Exp.term_of) {
    | Parens(e1)
    | Projector(_, e1)
    | Asc(e1, _)
    | Closure(_, e1) => go_ap(e1)
    | Fun(_, body, _, _) => go(body)
    /* Application drops the contract guard (no dynamic effect), so
     * totality of the application is totality of the body. */
    | FunWhere(_, _, body) => go(body)
    /* All Hazel builtins terminate (partiality is `err`, scanned by
     * DomainConditions). */
    | BuiltinFun(_) => Ok()
    | Constructor(_) => Ok()
    | FixF(p, _, _) =>
      Error(
        "the recursive function `"
        ++ pat_name(p)
        ++ "` may diverge (structural-recursion detection is not implemented)",
      )
    /* Curried application: check the inner argument, keep unwrapping. */
    | Ap(_, fn', arg') =>
      let* _ = go(arg');
      go_ap(fn');
    | Var(x) =>
      if (is_builtin(ctx, x)) {
        Ok();
      } else {
        Error(
          "the definition of `"
          ++ x
          ++ "` is not visible; cannot establish its application terminates",
        );
      }
    | _ => Error("cannot establish that this application terminates")
    }
  and go_all = (es: list(Exp.t)): result(unit, reason) =>
    List.fold_left(
      (acc, e) => {
        let* _ = acc;
        go(e);
      },
      Ok(),
      es,
    );
  go(exp);
};
