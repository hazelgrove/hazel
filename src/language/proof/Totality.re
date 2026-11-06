open Util;

/* The structural-totality check: the proof checker's divergence gate.
 *
 * `check` conservatively decides "evaluating this expression cannot
 * diverge". Divergence is ⊥: it can't be expressed as a boolean
 * condition, so unlike a domain error it can never become an obligation,
 * and the caller must refuse the step when this check fails. When
 * unsure, the check fails with a reason.
 *
 * Totality
 * --------
 * "Total" means: you may evaluate it, and if it is a function you may
 * apply it anywhere on its type's domain. There is no status of "total
 * as a value but possibly looping as a function": a value you may not
 * call is not a total value. So:
 *   - a quantified binder of arrow type denotes a total map.
 *     `forall f -> ... f(x) ...` is fine: both `f` and `f(x)` are total.
 *     Instantiating such a binder at a looping closure (`omega`) is what
 *     gets refused (ProofCheck.Gate.instantiations), not the use of the
 *     binder.
 *   - `fun x -> e` is total exactly when `e` is, with the pattern's
 *     bindings assumed total (as values and, if arrow-typed, as maps).
 *     Treating every lambda as a total value without looking at its body
 *     would make `fun x -> omega(x)` total.
 *   - a parameter of a function whose body we are walking is assumed
 *     total, because the arguments were checked total first. This lets
 *     `fold_left(xs, f, acc)` through: the builtin's body applies its
 *     own `f` parameter, which stood for a total map at the call site.
 *
 * Totality is not definedness. `1/0` and `head([])` terminate in `err`,
 * which DomainConditions turns into obligations. `fun x -> 1/x` is a
 * total map here.
 *
 * What passes:
 *   - literals, constructors, tuples/lists;
 *   - variables: after substitution these are quantified binders,
 *     lambda/fix parameters, or builtins, all total maps (above);
 *   - lambdas whose body is total under the assumption that their
 *     parameters are total;
 *   - applications of total functions to total arguments, including
 *     applications of abstract (quantified) functions;
 *   - builtin applications (every Hazel builtin terminates; their
 *     partiality is the domain scan's job);
 *   - case/if with total scrutinee and all branches total.
 *
 * What fails:
 *   - any reachable `FixF` that is not structurally recursive
 *     (StructuralRecursion). Recursive `let`s are elaborated to `FixF`,
 *     and evaluated environment values keep the `FixF` spine through
 *     `Substitution.in_exp`, so one syntactic check covers `fix`,
 *     recursive lets, and references to recursive definitions alike.
 *     A structurally recursive fix passes: its body is walked with the
 *     self-names assumed total (sound by well-founded induction on the
 *     subterm order, since every recursive call strictly descends), so
 *     the rest of the body is held to the same standard. A
 *     non-structural fix fails with its own reason. This is why
 *     `omega == omega` is refused even though `omega` is only a value
 *     there: a looping map is not total.
 *   - applying a variable that is definitely not of function type (the
 *     term is ill-typed or unclassifiable);
 *   - anything else we can't positively classify (holes, exotic forms).
 *
 * Quantified functions are total on their type's whole domain; there is
 * no way to quantify over a function that is total only on part of it.
 * `where` is a boolean hypothesis, not a totality domain.
 *
 * The expression is expected to be environment-substituted
 * (`Substitution.in_exp`) before calling: substitution inlines visible
 * definitions (so non-recursive `let f = fun ...` applications can be
 * checked through their bodies), erases closures, and leaves quantified
 * binders as bare `Var`s. */

[@deriving (show({with_path: false}), sexp, yojson)]
type reason = string;

/* Is `name` a builtin function (or constant) in scope? Builtin ctx
 * entries are installed with `Id.invalid` (BuiltinsUtil.
 * ctx_entry_of_builtin), which distinguishes them from user binders. */
let is_builtin = (ctx: Ctx.t, name: Var.t): bool =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => entry.id == Id.invalid
  | None => false
  };

let check = (~ctx: Ctx.t, exp: Exp.t): result(unit, reason) => {
  open Result.Syntax;
  /* Could `name` denote a function? Any variable that can be a function
   * is assumed to be a total map, so this only
   * rules out applying a name statics has pinned to a non-function type
   * (an ill-typed / unclassifiable term). Unknown types and names absent
   * from the ctx (lambda and fix parameters, whose binding site is inside
   * the expression rather than in the surrounding context) are allowed. */
  let may_be_function = (name: Var.t): bool =>
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
  /* `assumed`: names that may be treated as total, both as values and
   * as maps, inside the term we are walking:
   *   - self-names of enclosing structurally-recursive fixpoints (sound
   *     by well-founded induction on the subterm order);
   *   - parameters of any function whose body we walk, since the
   *     arguments at the application site were checked total first, and
   *     a quantified binder standing in an argument position denotes a
   *     total map.
   * Rebinding such a name shadows the assumption.
   *
   * Entering the body of a function or fixpoint, its parameters join
   * `assumed`, shadowing any same-named outer assumption. */
  let bind_total = (assumed: list(Var.t), p: Pat.t): list(Var.t) => {
    let bound = Pat.bound_vars(p);
    bound @ List.filter(n => !List.mem(n, bound), assumed);
  };
  /* On a reachable FixF: if it is structurally recursive, continue `k`
   * into the body with the self-names assumed total; otherwise fail. */
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
        ++ (
          switch (p |> Pat.bindings |> Binding.variable_names) {
          | [name, ..._] => name
          | [] => "_"
          }
        )
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
     * lambda/fix parameters, or builtins, which all denote total maps
     * (header). A looping closure is caught where a binder is
     * instantiated (ProofCheck.Gate.instantiations). */
    | Var(_) => Ok()
    /* A lambda is total exactly when its body is, with the parameters
     * assumed total (header). A parameter's `where` guard can only make
     * the function match fewer arguments, never diverge. */
    | Fun(p, body, _, _) => go(~assumed=bind_total(assumed, p), body)
    | TypFun(_, body, _) => go(~assumed, body)
    /* A structurally recursive fix unrolls to its body with itself (a
     * total value, by the structural argument) bound, so check the body
     * under that assumption. A non-structural fix is refused. */
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
    /* Strict compound values and operators: total iff the children
     * are. (`1/0` is `err`, not ⊥: the domain scan's job.) */
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
    /* Non-recursive let (recursive lets carry a FixF in their definiens
     * after elaboration). The let's bindings shadow any assumed
     * self-names in its body, but the definiens was checked total, so
     * they are themselves assumed total. */
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
    /* Everything else (holes, invalid text, deferrals, modules, quoted
     * derivations with holes, type applications, ...) is refused. */
    | EmptyHole
    | Undefined
    | Invalid(_)
    | MultiHole(_)
    | DynamicErrorHole(_)
    | Deferral(_)
    | LivelitName(_)
    | TypAp(_)
    | DeferredAp(_)
    | Module(_)
    | ModuleExp(_)
    | Theorem(_)
    | Forall(_) => Error("cannot establish that this expression terminates")
    }
  /* The head of an application spine. Arguments were already checked
   * total, so a lambda head reduces to its body with its parameters
   * assumed total (if one is a function it is a total map, so the body
   * may apply it). `depth` counts the pending arguments: each lambda
   * head consumes one, and an inner lambda reached with arguments still
   * pending is itself applied. Without it,
   * `(fun a -> fun b -> body)(x)(y)` would stop at the inner Fun as a
   * value and never check `body`. */
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
    /* An applied structurally recursive fix terminates on total
     * arguments; keep checking its body with the self-names assumed
     * total. */
    | FixF(p, body, _) =>
      fix_gate(~assumed, fn, p, body, (~assumed, body) =>
        go_ap(~assumed, ~depth, body)
      )
    /* Curried application: check the inner argument, keep unwrapping. */
    | Ap(_, fn', arg') =>
      let* _ = go(~assumed, arg');
      go_ap(~assumed, ~depth=depth + 1, fn');
    /* Applying a variable. Every function-typed name in scope denotes a
     * total map:
     *   - `assumed`: a recursive call of an enclosing structural fix,
     *     or a parameter standing for an argument already checked total;
     *   - a builtin, which terminates;
     *   - otherwise a quantified binder, total by the quantifier's
     *     meaning and enforced at instantiation.
     * Only a name the statics have pinned to a non-function type is
     * refused, as a term we can't classify. */
    | Var(x) =>
      if (List.mem(x, assumed) || is_builtin(ctx, x) || may_be_function(x)) {
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
