/* STRUCTURALRECURSION.re — structural-recursion detection (tier 2 of the
 * two-tier divergence gate, docs/prover-obligations.md §4.1; Phase 4a).
 *
 * `check` conservatively decides whether a `FixF(self_pat, body, _)` is
 * STRUCTURALLY RECURSIVE: every recursive call to the self-name inside
 * the body passes, in some FIXED argument position i (the same i for all
 * call sites), a STRICT SUBTERM of that position's parameter. "Strict
 * subterm" is the classic syntactic notion: a variable bound by
 * destructuring the parameter through at least one constructor layer
 * (case patterns, destructuring lets, cons patterns `h :: t`, list
 * literal patterns, or constructor patterns directly in a `fun` binder).
 * When this holds, every recursive-call chain descends strictly in the
 * well-founded subterm order on position i, so — provided everything
 * else in the body terminates, which Totality.re verifies separately —
 * evaluation terminates.
 *
 * What is tracked (per lexical scope):
 *   - a status map: variable -> (param index, optional tuple component,
 *     strict?). Parameters are seeded non-strict ("aliases"); casing on
 *     an alias marks the pattern's bindings strict once the walk passes
 *     under a constructor (`Ap`/`Cons`/`ListLit` pattern layers — depth
 *     is transitive: `Ap(Lam(b), a)` marks both `b` and `a` strict);
 *     casing on an already-strict variable marks even a bare-var
 *     pattern strict (subterm-of-subterm).
 *   - tuple parameters: `fun (a, b) -> ...` (or `case p | (a, b) => ...`
 *     directly on parameter p) tracks components as (i, Some j); a call
 *     `f((x, y))` is decreasing at (i, Some j) when component j is a
 *     variable strict in (i, Some j). Components of a NON-strict tuple
 *     alias are themselves aliases, never strict (reconstructing the
 *     same tuple must not pass).
 *   - shadowing kills tracking: any rebinding of a tracked variable or
 *     of the self-name (fun/let/case/fix patterns) removes it from the
 *     maps in that scope.
 *
 * Conservative failures (honest by construction):
 *   - the self-name used anywhere other than as the head of a direct
 *     application (escapes: passed along, rebound, returned bare);
 *   - a call site with NO decreasing position, or call sites whose
 *     decreasing-position sets have empty intersection;
 *   - mutual recursion (`fix (f, g) -> ...` — a tuple-bound fix): the
 *     self pattern binds more than one name; refused. Detecting
 *     lexicographic/mutual descent is out of scope for 4a.
 *   - a NESTED FixF inside the body must itself pass this check (it is
 *     checked standalone, then its body is still walked for calls to
 *     the OUTER self with the inner binders shadowed);
 *   - any expression form the walk does not positively understand.
 *
 * Like Totality.check, the input is expected to be environment-
 * substituted (Substitution.in_exp): other definitions are inlined
 * (recursive ones surfacing their own FixF spine, handled as nested
 * fixes), and only the self-name refers back to this fixpoint. */

open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type reason = string;

/* Which parameter (and, for tuple parameters, which component) a
 * tracked variable descends from. */
type origin = {
  param: int,
  comp: option(int),
};

type status = {
  origin,
  strict: bool,
};

/* A candidate decreasing position at a call site. */
type pos = (int, option(int));

type env = list((Var.t, status));

let ( let* ) = (r: result(_, reason), f) =>
  switch (r) {
  | Ok(x) => f(x)
  | Error(_) as e => e
  };

/* Strip wrappers that are transparent for head/argument classification. */
let rec strip = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(e1)
  | Projector(_, e1)
  | Asc(e1, _)
  | Closure(_, e1) => strip(e1)
  | _ => e
  };

let shadow = (env: env, names: list(Var.t)): env =>
  List.filter(((n, _)) => !List.mem(n, names), env);

let drop = (selfs: list(Var.t), names: list(Var.t)): list(Var.t) =>
  List.filter(n => !List.mem(n, names), selfs);

let var_status = (env: env, e: Exp.t): option(status) =>
  switch (strip(e) |> Exp.term_of) {
  | Var(x) => List.assoc_opt(x, env)
  | _ => None
  };

/* Bindings introduced by matching a term of status `st` against pattern
 * `p`. Constructor layers (Ap/Cons/ListLit) switch on strictness —
 * transitively, so nested patterns like `Ap(Lam(b), a)` mark `b` strict
 * through both layers. Tuple layers: strict components of a strict
 * tuple stay strict (a component is a subterm); components of a
 * non-strict PARAMETER alias become component aliases (i, Some j) so
 * tuple-passing recursion `f((b, n))` can be tracked; anything deeper
 * is dropped (conservative — untracked, but still shadowed by the
 * caller via Pat.bound_vars). */
let rec pat_bind = (~origin: origin, ~strict: bool, p: Pat.t): env =>
  switch (p |> Pat.term_of) {
  | Parens(q)
  | Projector(_, q)
  | Asc(q, _)
  | TupLabel(_, q) => pat_bind(~origin, ~strict, q)
  | Var(x) => [
      (
        x,
        {
          origin,
          strict,
        },
      ),
    ]
  | Ap(_, q) => pat_bind(~origin, ~strict=true, q)
  | Cons(q1, q2) =>
    pat_bind(~origin, ~strict=true, q1) @ pat_bind(~origin, ~strict=true, q2)
  | ListLit(qs) =>
    List.concat_map(q => pat_bind(~origin, ~strict=true, q), qs)
  | Tuple(qs) =>
    if (strict) {
      List.concat_map(q => pat_bind(~origin, ~strict=true, q), qs);
    } else if (origin.comp == None) {
      List.concat(
        List.mapi(
          (j, q) =>
            pat_bind(
              ~origin={
                param: origin.param,
                comp: Some(j),
              },
              ~strict=false,
              q,
            ),
          qs,
        ),
      );
    } else {
      [];
    }
  /* Wild, atoms, nullary constructors, labels, holes: nothing tracked. */
  | _ => []
  };

/* Uncurry an application spine: `f(a)(b)` -> (head f, [a, b]). */
let uncurry = (e: Exp.t): (Exp.t, list(Exp.t)) => {
  let rec go = (e: Exp.t, acc: list(Exp.t)) => {
    let e = strip(e);
    switch (e |> Exp.term_of) {
    | Ap(_, fn, arg) => go(fn, [arg, ...acc])
    | _ => (e, acc)
    };
  };
  go(e, []);
};

/* The decreasing positions witnessed by ONE call's argument list:
 * position k is valid when its actual argument is a variable strict in
 * parameter k (or, for tuple arguments, has a component j that is a
 * variable strict in (k, Some j)). */
let valid_positions = (env: env, args: list(Exp.t)): list(pos) =>
  List.concat(
    List.mapi(
      (k, arg) =>
        switch (strip(arg) |> Exp.term_of) {
        | Var(x) =>
          switch (List.assoc_opt(x, env)) {
          | Some({origin: {param, comp: None}, strict: true}) when param == k => [
              (k, None),
            ]
          | _ => []
          }
        | Tuple(comps) =>
          List.concat(
            List.mapi(
              (j, c) =>
                switch (strip(c) |> Exp.term_of) {
                | Var(x) =>
                  switch (List.assoc_opt(x, env)) {
                  | Some({origin: {param, comp: Some(j')}, strict: true})
                      when param == k && j' == j => [
                      (k, Some(j)),
                    ]
                  | _ => []
                  }
                | _ => []
                },
              comps,
            ),
          )
        | _ => []
        },
      args,
    ),
  );

/* Walk the body accumulating, per recursive call, its set of valid
 * decreasing positions. Errors are conservative refusals. */
let rec check = (fix: Exp.t): result(unit, reason) =>
  switch (strip(fix) |> Exp.term_of) {
  | FixF(self_pat, body, _) =>
    switch (self_pat |> Pat.bound_vars) {
    /* `fix () -> ...`, `fix _ -> ...`: no self-name, nothing can
     * recur — the walk below still refuses exotic bodies via Totality
     * (which walks the body itself), so accept here. */
    | [] => Ok()
    | [self] =>
      let* possets = chain(~self, ~index=0, ~env=[], body);
      switch (possets) {
      | [] => Ok() /* no recursive calls at all */
      | [first, ...rest] =>
        let fixed =
          List.fold_left(
            (acc, s) => List.filter(p => List.mem(p, s), acc),
            first,
            rest,
          );
        if (fixed == []) {
          Error(
            "recursive calls to `"
            ++ self
            ++ "` do not all decrease on one fixed argument position",
          );
        } else {
          Ok();
        };
      };
    | _ =>
      Error(
        "the fixpoint binds several names at once (mutual recursion is not supported by structural-recursion detection)",
      )
    }
  | _ => Error("not a fixpoint")
  }

/* Collect the curried parameter chain `fun p0 -> fun p1 -> ...`,
 * seeding each parameter's bindings as (non-strict) aliases of
 * position i — through constructor layers directly in the binder they
 * are already strict subterms of the actual argument. */
and chain =
    (~self: Var.t, ~index: int, ~env: env, e: Exp.t)
    : result(list(list(pos)), reason) =>
  switch (strip(e) |> Exp.term_of) {
  | Fun(p, body, _, _) =>
    let bound = Pat.bound_vars(p);
    if (List.mem(self, bound)) {
      Ok([]) /* the self-name is shadowed by its own parameter */;
    } else {
      let env' =
        pat_bind(
          ~origin={
            param: index,
            comp: None,
          },
          ~strict=false,
          p,
        )
        @ shadow(env, bound);
      chain(~self, ~index=index + 1, ~env=env', body);
    };
  | FunWhere(p, guard, body) =>
    let bound = Pat.bound_vars(p);
    if (List.mem(self, bound)) {
      Ok([]);
    } else {
      let env' =
        pat_bind(
          ~origin={
            param: index,
            comp: None,
          },
          ~strict=false,
          p,
        )
        @ shadow(env, bound);
      let* g = go(~self, ~env=env', guard);
      let* b = chain(~self, ~index=index + 1, ~env=env', body);
      Ok(g @ b);
    };
  | _ => go(~self, ~env, e)
  }

and go =
    (~self: Var.t, ~env: env, e: Exp.t): result(list(list(pos)), reason) => {
  let go_all = (es: list(Exp.t)): result(list(list(pos)), reason) =>
    List.fold_left(
      (acc, e) => {
        let* xs = acc;
        let* ys = go(~self, ~env, e);
        Ok(xs @ ys);
      },
      Ok([]),
      es,
    );
  /* Enter a subscope under pattern `p`, optionally seeding statuses for
   * its bindings from the matched scrutinee's status. */
  let under =
      (~seed: option(status), p: Pat.t, e: Exp.t)
      : result(list(list(pos)), reason) => {
    let bound = Pat.bound_vars(p);
    if (List.mem(self, bound)) {
      Ok([]) /* self shadowed: no recursive calls possible below */;
    } else {
      let seeded =
        switch (seed) {
        | Some({origin, strict}) =>
          /* Bindings under at least one constructor layer become
           * strict; a bare alias of an ALREADY-strict variable is
           * still strict (subterm of a subterm). */
          pat_bind(~origin, ~strict, p)
        | None => []
        };
      go(~self, ~env=seeded @ shadow(env, bound), e);
    };
  };
  switch (e |> Exp.term_of) {
  | Var(x) when x == self =>
    Error(
      "`"
      ++ x
      ++ "` escapes: it is used other than as the head of a direct call",
    )
  | Var(_)
  | Atom(_)
  | Constructor(_)
  | BuiltinFun(_)
  | Label(_)
  | ExplicitNonlabel
  | LivelitName(_)
  | Undefined
  | DrvQuote(_) => Ok([])
  | Parens(e1)
  | Projector(_, e1)
  | Asc(e1, _)
  | Filter(_, e1)
  | Closure(_, e1)
  | TyAlias(_, _, e1)
  | Use(_, e1)
  | TypAp(e1, _)
  | Test(e1) => go(~self, ~env, e1)
  | ListLit(es)
  | Tuple(es) => go_all(es)
  | TupLabel(e1, e2)
  | Dot(e1, e2)
  | TupleExtension(e1, e2)
  | Cons(e1, e2)
  | ListConcat(e1, e2)
  | Seq(e1, e2)
  | HintedTest(e1, e2) => go_all([e1, e2])
  | BinOp(_, e1, e2) => go_all([e1, e2])
  | UnOp(_, e1) => go(~self, ~env, e1)
  | If(e1, e2, e3) => go_all([e1, e2, e3])
  | DeferredAp(fn, args) => go_all([fn, ...args])
  | Fun(p, body, _, _) => under(~seed=None, p, body)
  | FunWhere(p, guard, body) =>
    let* g = under(~seed=None, p, guard);
    let* b = under(~seed=None, p, body);
    Ok(g @ b);
  | TypFun(_, body, _) => go(~self, ~env, body)
  | Let(p, e1, e2) =>
    let* r1 = go(~self, ~env, e1);
    let* r2 = under(~seed=var_status(env, e1), p, e2);
    Ok(r1 @ r2);
  | Match(scrut, rules) =>
    let* r0 = go(~self, ~env, scrut);
    let seed = var_status(env, scrut);
    List.fold_left(
      (acc, (p, rhs)) => {
        let* xs = acc;
        let* ys = under(~seed, p, rhs);
        Ok(xs @ ys);
      },
      Ok(r0),
      rules,
    );
  /* A nested fixpoint must itself be structurally recursive; its body
   * is then still walked for calls to the OUTER self, with the inner
   * binder shadowing. (Totality re-checks the inner fix on its own
   * walk; requiring it here keeps this check meaningful standalone.) */
  | FixF(p, body, _) =>
    let* _ =
      switch (check(e)) {
      | Ok () => Ok([])
      | Error(r) => Error("nested fixpoint: " ++ r)
      };
    let bound = Pat.bound_vars(p);
    if (List.mem(self, bound)) {
      Ok([]);
    } else {
      go(~self, ~env=shadow(env, bound), body);
    };
  | Ap(_) =>
    let (head, args) = uncurry(e);
    switch (head |> Exp.term_of) {
    | Var(f) when f == self =>
      /* Arguments may themselves contain recursive calls. */
      let* inner = go_all(args);
      let posset = valid_positions(env, args);
      if (posset == []) {
        Error(
          "a recursive call to `"
          ++ f
          ++ "` passes no strict subterm of a parameter in any argument position",
        );
      } else {
        Ok(inner @ [posset]);
      };
    | _ =>
      let* rh = go(~self, ~env, head);
      let* ra = go_all(args);
      Ok(rh @ ra);
    };
  /* Holes, modules, theorems, quantifiers, invalid text, ... —
   * conservatively refused (Totality refuses them independently). */
  | _ =>
    Error("contains a form structural-recursion detection does not analyze")
  };
};

/* Boolean convenience wrapper; `info_map` is accepted for signature
 * stability with Totality but the check is purely syntactic today. */
let is_structural = (~info_map as _: option(Statics.Map.t)=?, e: Exp.t): bool =>
  switch (check(e)) {
  | Ok () => true
  | Error(_) => false
  };
