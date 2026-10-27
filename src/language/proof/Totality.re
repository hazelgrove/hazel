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
 * THE UNIFIED TOTALITY TAG
 * -----------------------
 * There is ONE totality tag, not two. "Total" means: you may evaluate
 * it, and if it is a function you may APPLY it (anywhere on its type's
 * domain). There is no intermediate status of "total as a value but
 * possibly looping as a function" — a value you may not call is not a
 * total value.
 *
 * Consequences, which this file implements:
 *   - a quantified binder (§1.3) of arrow type denotes a TOTAL MAP.
 *     `forall f -> ... f(x) ...` is fine: both the bare `f` and `f(x)`
 *     are total. Quantifiers range over total maps, so instantiating
 *     one at a genuinely looping closure (`omega`) is what gets refused
 *     (ProofCheck.instantiation_gate), not the use of the binder.
 *   - `fun x -> e` is total exactly when `e` is, with the pattern's
 *     bindings assumed total (as values AND as maps, if arrow-typed).
 *     There is deliberately no "lambdas are values, never look at the
 *     body" shortcut: it would make `fun x -> omega(x)` a total value,
 *     which is precisely the split this tag removes.
 *   - a parameter of a function whose body we are walking is assumed
 *     total, because the ARGUMENTS were checked total before we got
 *     here. This is what lets `fold_left(xs, f, acc)` through: the
 *     builtin's body applies its own `f` parameter, and `f` stood for a
 *     total map at the call site.
 *
 * TOTALITY IS NOT DEFINEDNESS. `1/0`, `head([])` and friends terminate;
 * they terminate in `err`. That is the domain scan's business
 * (DomainConditions.re, which turns them into obligations), never this
 * check's. In particular `inv = fun x -> 1/x` is a total map here.
 *
 * What passes:
 *   - literals, constructors, tuples/lists;
 *   - variables — after substitution these are quantified binders,
 *     lambda/fix parameters, or builtins, all total maps (above);
 *   - lambdas whose BODY is total under the assumption that their
 *     parameters are total;
 *   - applications of total functions to total arguments — including
 *     applications of abstract (quantified) functions;
 *   - builtin applications (every Hazel builtin terminates; their
 *     PARTIALITY is the domain scan's job) — including int/float
 *     division, which terminates with `err`;
 *   - case/if with total scrutinee and all branches total.
 *
 * What fails:
 *   - any reachable `FixF` that is not STRUCTURALLY RECURSIVE
 *     (StructuralRecursion.re, tier 2 — Phase 4a). Recursive `let`s are
 *     elaborated to `FixF` (Statics.re let-elaboration), and evaluated
 *     environment values keep the `FixF` spine through
 *     `Substitution.in_exp`, so one syntactic check covers "fix",
 *     recursive lets, and references to recursive definitions alike.
 *     A structurally recursive fix passes: its body is walked with the
 *     self-name(s) ASSUMED total (sound by well-founded induction on
 *     the subterm order — StructuralRecursion guarantees every
 *     recursive call strictly descends), so the rest of the body is
 *     still held to this same standard. A non-structural fix fails
 *     with a reason distinguishing it from tier-1 failures. This is
 *     why `omega == omega` is refused even though `omega` is "only a
 *     value" there: under the unified tag a looping map is not total.
 *   - applying a variable that is definitely not of function type
 *     (nonsense — the term is ill-typed or unclassifiable);
 *   - anything else we cannot positively classify (holes, exotic
 *     forms) — conservative by construction.
 *
 * LIMITATION (v1). Quantified functions are total EVERYWHERE on their
 * type's domain; there is no way to quantify over a function that is
 * total only on a carve-out. `where` remains a boolean hypothesis, not
 * a totality domain.
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

/* Could `name` denote a function? Under the unified tag any variable
 * that CAN be a function is assumed to be a total map, so this only
 * rules out applying a name statics has pinned to a non-function type
 * (an ill-typed / unclassifiable term). Unknown types and names absent
 * from the ctx (lambda and fix parameters, whose binding site is inside
 * the expression rather than in the surrounding context) are allowed. */
let may_be_function = (ctx: Ctx.t, name: Var.t): bool =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => true
  | Some(entry) =>
    switch (Typ.term_of(entry.typ)) {
    /* Definitely not a function. Everything else — arrows, unknowns,
     * aliases, polymorphic and recursive types, wrappers — is allowed. */
    | Atom(_)
    | List(_)
    | Sum(_)
    | Prod(_)
    | Label(_)
    | ExplicitNonlabel => false
    | _ => true
    }
  };

let check =
    (~info_map as _: Statics.Map.t, ~ctx: Ctx.t, exp: Exp.t)
    : result(unit, reason) => {
  let ( let* ) = (r: result(_, reason), f) =>
    switch (r) {
    | Ok(x) => f(x)
    | Error(_) as e => e
    };
  /* `assumed`: names that may be treated as total — both as values and
   * as maps (the unified tag) — inside the term we are walking:
   *   - self-names of enclosing structurally-recursive fixpoints (sound
   *     by well-founded induction on the subterm order);
   *   - parameters of any function whose body we walk, since the
   *     arguments at the application site were checked total first, and
   *     a quantified binder standing in an argument position denotes a
   *     total map.
   * Rebinding such a name shadows the assumption. */
  let unshadow = (assumed: list(Var.t), p: Pat.t): list(Var.t) => {
    let bound = Pat.bound_vars(p);
    List.filter(n => !List.mem(n, bound), assumed);
  };
  /* Enter the body of a function/fixpoint: its parameters join
   * `assumed` (shadowing any same-named outer assumption). */
  let bind_total = (assumed: list(Var.t), p: Pat.t): list(Var.t) =>
    Pat.bound_vars(p) @ unshadow(assumed, p);
  /* On a reachable FixF: tier 2. Structural -> continue `k` into the
   * body with the self-names assumed total; otherwise fail with a
   * tier-2-specific reason. */
  let fix_gate =
      (
        ~assumed: list(Var.t),
        fix: Exp.t,
        p: Pat.t,
        body: Exp.t,
        k: (~assumed: list(Var.t), Exp.t) => result(unit, reason),
      )
      : result(unit, reason) =>
    switch (StructuralRecursion.check(fix)) {
    | Ok () => k(~assumed=bind_total(assumed, p), body)
    | Error(r) =>
      Error(
        "the recursive function `"
        ++ pat_name(p)
        ++ "` may diverge: it is recursive and not visibly structural ("
        ++ r
        ++ ")",
      )
    };
  let rec go = (~assumed: list(Var.t), e: Exp.t): result(unit, reason) =>
    switch (e |> Exp.term_of) {
    /* Literals and other atomic values. */
    | Atom(_)
    | Constructor(_)
    | BuiltinFun(_)
    | Label(_)
    | ExplicitNonlabel
    | ListLit([])
    | DrvQuote(_) => Ok()
    /* A bare variable: after substitution these are quantified binders,
     * lambda/fix parameters, or builtins. All denote TOTAL MAPS under
     * the unified tag (header), so arrow type is no obstacle — the
     * obligation to supply a total map is discharged at the
     * INSTANTIATION site (ProofCheck.instantiation_gate), which is
     * where a looping closure is caught. */
    | Var(_) => Ok()
    /* A lambda is total exactly when its body is, with the parameters
     * assumed total. No "it's a value, don't look" shortcut: that would
     * make `fun x -> omega(x)` total. A parameter's `where` guard is
     * irrelevant here: it can only make the function match FEWER
     * arguments, never diverge. */
    | Fun(p, body, _, _) => go(~assumed=bind_total(assumed, p), body)
    | TypFun(_, body, _) => go(~assumed, body)
    /* Recursion: tier 2 (§4.1). A structurally recursive fix unrolls
     * to its body with itself (a total value, by the structural
     * argument) bound — check the body under that assumption. A
     * non-structural fix is refused. */
    | FixF(p, body, _) => fix_gate(~assumed, e, p, body, go)
    /* Transparent wrappers. */
    | Parens(e1)
    | Projector(_, e1)
    | Asc(e1, _)
    | Filter(_, e1)
    | Closure(_, e1)
    | TyAlias(_, _, e1)
    | Use(_, e1)
    | Test(e1) => go(~assumed, e1)
    /* Strict compound values / operators: total iff the children are.
     * (BinOp partiality — 1/0 — is `err`, not ⊥: the domain scan's
     * job, not this check's.) */
    | ListLit(es)
    | Tuple(es) => go_all(~assumed, es)
    | TupLabel(e1, e2)
    | Dot(e1, e2)
    | TupleExtension(e1, e2)
    | Cons(e1, e2)
    | ListConcat(e1, e2)
    | Seq(e1, e2)
    | HintedTest(e1, e2)
    | BinOp(_, e1, e2) =>
      let* _ = go(~assumed, e1);
      go(~assumed, e2);
    | UnOp(_, e1) => go(~assumed, e1)
    /* `!!e` terminates exactly when e does (it may be stuck, never loops). */
    | Valued(e1) => go(~assumed, e1)
    /* Non-recursive let (recursive lets already carry a FixF in their
     * definiens after elaboration). The let's bindings shadow any
     * assumed self-names in its body — but the definiens was checked
     * total, so they are themselves assumed total (unified tag). */
    | Let(p, e1, e2) =>
      let* _ = go(~assumed, e1);
      go(~assumed=bind_total(assumed, p), e2);
    /* case/if: total if the scrutinee and all branches are. */
    | If(e1, e2, e3) =>
      let* _ = go(~assumed, e1);
      let* _ = go(~assumed, e2);
      go(~assumed, e3);
    | Match(scrut, rules) =>
      let* _ = go(~assumed, scrut);
      List.fold_left(
        (acc, (p, rhs)) => {
          let* _ = acc;
          /* Pattern bindings destructure the (total) scrutinee. */
          go(~assumed=bind_total(assumed, p), rhs);
        },
        Ok(),
        rules,
      );
    | Ap(_, fn, arg) =>
      let* _ = go(~assumed, arg);
      go_ap(~assumed, ~depth=1, fn);
    /* Everything else — holes, invalid text, deferrals, modules,
     * quoted derivations with holes, type applications, ... — is
     * conservatively refused. */
    | _ => Error("cannot establish that this expression terminates")
    }
  /* The head of an application spine. Arguments were already checked
   * total, so a lambda head reduces to its body with total values
   * bound — check the body, with the parameters ASSUMED TOTAL (they
   * stand for the checked arguments; if one is a function it is a total
   * map, so the body may apply it). `depth` counts the pending
   * arguments (curried applications increment it): a lambda head
   * consumes one — the LAST consumption forces the body as an
   * expression; an inner lambda reached with arguments still pending
   * is itself applied, so keep consuming. (Without the depth,
   * `(fun a -> fun b -> BODY)(x)(y)` would end at the inner Fun "as a
   * value" and BODY would never be checked.) */
  and go_ap =
      (~assumed: list(Var.t), ~depth: int, fn: Exp.t): result(unit, reason) =>
    switch (fn |> Exp.term_of) {
    | Parens(e1)
    | Projector(_, e1)
    | Asc(e1, _)
    | Closure(_, e1) => go_ap(~assumed, ~depth, e1)
    | Fun(p, body, _, _) =>
      let assumed = bind_total(assumed, p);
      depth <= 1
        ? go(~assumed, body) : go_ap(~assumed, ~depth=depth - 1, body);
    /* All Hazel builtins terminate (partiality is `err`, scanned by
     * DomainConditions). */
    | BuiltinFun(_) => Ok()
    | Constructor(_) => Ok()
    /* An applied fix: tier 2. Structural -> its application to total
     * arguments terminates; keep checking the (fun-chain) body with
     * the self-names assumed total. */
    | FixF(p, body, _) =>
      fix_gate(~assumed, fn, p, body, (~assumed, body) =>
        go_ap(~assumed, ~depth, body)
      )
    /* Curried application: check the inner argument, keep unwrapping. */
    | Ap(_, fn', arg') =>
      let* _ = go(~assumed, arg');
      go_ap(~assumed, ~depth=depth + 1, fn');
    /* Applying a variable. Under the unified tag every function-typed
     * name in scope denotes a TOTAL MAP, so this succeeds:
     *   - `assumed`: a recursive call of an enclosing structural fix
     *     (its strict descent was verified by StructuralRecursion), or
     *     a parameter standing for an argument already checked total;
     *   - a builtin: terminates by implementation;
     *   - otherwise a quantified binder (§1.3) — total by the
     *     quantifier's own meaning, enforced at instantiation.
     * Only a name statics has pinned to a NON-function type is
     * refused: applying it is not a divergence question at all, it is
     * a term we cannot classify. */
    | Var(x) =>
      if (List.mem(x, assumed)
          || is_builtin(ctx, x)
          || may_be_function(ctx, x)) {
        Ok();
      } else {
        Error(
          "`"
          ++ x
          ++ "` is not a function here; cannot establish this application terminates",
        );
      }
    | _ => Error("cannot establish that this application terminates")
    }
  and go_all =
      (~assumed: list(Var.t), es: list(Exp.t)): result(unit, reason) =>
    List.fold_left(
      (acc, e) => {
        let* _ = acc;
        go(~assumed, e);
      },
      Ok(),
      es,
    );
  go(~assumed=[], exp);
};
