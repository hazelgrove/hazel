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
 *     with a reason distinguishing it from tier-1 failures.
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
  /* `assumed`: self-names of enclosing structurally-recursive fixpoints,
   * whose (applications and) values may be assumed total inside their
   * own bodies (well-founded induction on the subterm order). Rebinding
   * such a name shadows the assumption. */
  let unshadow = (assumed: list(Var.t), p: Pat.t): list(Var.t) => {
    let bound = Pat.bound_vars(p);
    List.filter(n => !List.mem(n, bound), assumed);
  };
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
    | Ok () => k(~assumed=Pat.bound_vars(p) @ unshadow(assumed, p), body)
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
    /* A bare variable: after substitution these are quantified binders
     * (which range over total VALUES, §1.3) or free names whose env
     * entry is a self-map. An arrow-typed abstract function is only
     * refused when APPLIED (see `go_ap`) — as a value it is total —
     * but we follow the conservative reading and refuse arrow-typed
     * unknowns even bare. */
    | Var(x) =>
      if (List.mem(x, assumed)) {
        Ok() /* self of an enclosing structural fix: a total value */;
      } else if (is_arrow_typed(ctx, x) && !is_builtin(ctx, x)) {
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
    /* Non-recursive let (recursive lets already carry a FixF in their
     * definiens after elaboration). The let's bindings shadow any
     * assumed self-names in its body. */
    | Let(p, e1, e2) =>
      let* _ = go(~assumed, e1);
      go(~assumed=unshadow(assumed, p), e2);
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
          go(~assumed=unshadow(assumed, p), rhs);
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
   * bound — check the body. `depth` counts the pending
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
      let assumed = unshadow(assumed, p);
      depth <= 1
        ? go(~assumed, body) : go_ap(~assumed, ~depth=depth - 1, body);
    /* Application drops the contract guard (no dynamic effect), so
     * totality of the application is totality of the body. */
    | FunWhere(p, _, body) =>
      let assumed = unshadow(assumed, p);
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
    | Var(x) =>
      if (List.mem(x, assumed)) {
        Ok() /* a recursive call of an enclosing structural fix: its * strict descent was verified by StructuralRecursion */;
      } else if (is_builtin(ctx, x)) {
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
