/* Structural-recursion detection, the part of the standardness routine
 * (`Standardness`) that accepts recursive definitions.
 *
 * `check` conservatively decides whether a `FixF(self_pat, body, _)` is
 * structurally recursive: every recursive call to the self-name inside
 * the body passes, in some fixed argument position i (the same i for all
 * call sites), a strict subterm of that position's parameter. "Strict
 * subterm" is the classic syntactic notion: a variable bound by
 * destructuring the parameter through at least one constructor layer
 * (case patterns, destructuring lets, cons patterns `h :: t`, list
 * literal patterns, or constructor patterns directly in a `fun` binder).
 * When this holds, every recursive-call chain descends strictly in the
 * well-founded subterm order on position i, so — provided everything
 * else in the body terminates, which the standardness routine judges
 * separately — evaluation terminates.
 *
 * A call may instead pass the parameter made smaller as a number, where
 * the guards of the branch it is in bound the parameter below
 * (`decreased`): `if n <= 0 then 1 else n * f(n - 1)`.
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
 *     variable strict in (i, Some j). Components of a non-strict tuple
 *     alias are themselves aliases, never strict (reconstructing the
 *     same tuple must not pass).
 *   - shadowing kills tracking: any rebinding of a tracked variable or
 *     of the self-name (fun/let/case/fix patterns) removes it from the
 *     maps in that scope.
 *
 * Refused:
 *   - the self-name used anywhere other than as the head of a direct
 *     application (passed along, rebound, returned bare);
 *   - a call site with no decreasing position, or call sites whose
 *     decreasing-position sets have empty intersection;
 *   - mutual recursion (`fix (f, g) -> ...`, a tuple-bound fix): the
 *     self pattern binds more than one name, and lexicographic or mutual
 *     descent is not detected;
 *   - a nested FixF inside the body that doesn't itself pass this check
 *     (it is checked standalone, then its body is still walked for calls
 *     to the outer self with the inner binders shadowed);
 *   - any expression form the walk does not positively understand.
 *
 * The input is expected to be environment-substituted
 * (Substitution.in_exp), as a definition's value is when the
 * standardness routine judges it: other definitions are inlined
 * (recursive ones surfacing their own FixF spine, handled as nested
 * fixes), and only the self-name refers back to this fixpoint. */

open Util;
open Result.Syntax;

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

let shadow = (env: env, names: list(Var.t)): env =>
  List.filter(
    ~f=((n, _)) => !List.mem(names, n, ~equal=String.equal),
    env,
  );

let var_status = (env: env, e: Exp.t): option(status) =>
  switch (ProofSyntax.unwrap(e) |> Exp.term_of) {
  | Var(x) => List.Assoc.find(env, x, ~equal=String.equal)
  | _ => None
  };

/* Bindings introduced by matching a term of status `st` against pattern
 * `p`. Constructor layers (Ap/Cons/ListLit) switch on strictness —
 * transitively, so nested patterns like `Ap(Lam(b), a)` mark `b` strict
 * through both layers. Tuple layers: strict components of a strict
 * tuple stay strict (a component is a subterm); components of a
 * non-strict parameter alias become component aliases (i, Some j) so
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
    List.concat_map(~f=q => pat_bind(~origin, ~strict=true, q), qs)
  | Tuple(qs) =>
    if (strict) {
      List.concat_map(~f=q => pat_bind(~origin, ~strict=true, q), qs);
    } else if (Option.is_none(origin.comp)) {
      List.concat(
        List.mapi(
          ~f=
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
    let e = ProofSyntax.unwrap(e);
    switch (e |> Exp.term_of) {
    | Ap(_, fn, arg) => go(fn, [arg, ...acc])
    | _ => (e, acc)
    };
  };
  go(e, []);
};

/* The variable `arg` makes smaller as a number, in the branch the
 * guards `guards` describe: `x - k` on an Int with a lower bound, or
 * `monus(x, k)` on a Nat known to be at least 1, `k` a positive literal.
 * Called with smaller and smaller `x`, a function stops recurring: `x`
 * minus the lowest of those bounds shrinks and stays non-negative. */
let decreased = (~guards: Bounds.t, arg: Exp.t): option(Var.t) => {
  open OptUtil.Syntax;
  let positive = (k: Exp.t) =>
    Option.exists(Bounds.literal(k), ~f=k => Bigint.(k > zero));
  switch (ProofSyntax.unwrap(arg) |> Exp.term_of) {
  | BinOp(Int(Minus), x, k) when positive(k) =>
    let* x = Bounds.variable(x);
    let+ _ = Bounds.lower(guards, x);
    x;
  | Ap(_, f, args) =>
    let is_monus =
      switch (ProofSyntax.unwrap(f) |> Exp.term_of) {
      | Var("monus")
      | BuiltinFun("monus") => true
      | _ => false
      };
    switch (ProofSyntax.unwrap(args) |> Exp.term_of) {
    | Tuple([x, k]) when is_monus && positive(k) =>
      let* x = Bounds.variable(x);
      let* lo = Bounds.lower(~nat=true, guards, x);
      Bigint.(lo >= one) ? Some(x) : None;
    | _ => None
    };
  | _ => None
  };
};

/* The decreasing positions witnessed by one call's argument list:
 * position k is valid when its actual argument is a variable strict in
 * parameter k, or parameter k itself `decreased` as a number; for a
 * tuple argument, (k, Some j) is valid when component j is either. */
let valid_positions =
    (~guards: Bounds.t, env: env, args: list(Exp.t)): list(pos) => {
  let descends = (x: Var.t, ~strict, origin: origin) =>
    switch (List.Assoc.find(env, x, ~equal=String.equal)) {
    | Some(status) =>
      status.origin.param == origin.param
      && Option.equal(Int.equal, status.origin.comp, origin.comp)
      && Bool.equal(status.strict, strict)
    | None => false
    };
  let at = (origin: origin, arg: Exp.t) =>
    switch (ProofSyntax.unwrap(arg) |> Exp.term_of, decreased(~guards, arg)) {
    | (Var(x), _) => descends(x, ~strict=true, origin)
    | (_, Some(x)) => descends(x, ~strict=false, origin)
    | _ => false
    };
  List.concat(
    List.mapi(
      ~f=
        (k, arg) =>
          switch (ProofSyntax.unwrap(arg) |> Exp.term_of) {
          | Tuple(comps) =>
            List.concat(
              List.mapi(
                ~f=
                  (j, c) =>
                    at(
                      {
                        param: k,
                        comp: Some(j),
                      },
                      c,
                    )
                      ? [(k, Some(j))] : [],
                comps,
              ),
            )
          | _ =>
            at(
              {
                param: k,
                comp: None,
              },
              arg,
            )
              ? [(k, None)] : []
          },
      args,
    ),
  );
};

/* Walk the body accumulating, per recursive call, its set of valid
 * decreasing positions. Errors are conservative refusals. */
let rec check = (fix: Exp.t): result(unit, reason) =>
  switch (ProofSyntax.unwrap(fix) |> Exp.term_of) {
  | FixF(self_pat, body, _) =>
    switch (self_pat |> Pat.bound_vars) {
    /* `fix () -> ...`, `fix _ -> ...`: no self-name, so nothing can
     * recur. The standardness routine judges the body itself. */
    | [] => Ok()
    | [self] =>
      let* possets = chain(~self, ~index=0, ~env=[], body);
      switch (possets) {
      | [] => Ok() /* no recursive calls at all */
      | [first, ...rest] =>
        let fixed =
          List.fold_left(
            ~f=
              (acc, s) =>
                List.filter(~f=p => List.mem(s, p, ~equal=Poly.equal), acc),
            ~init=first,
            rest,
          );
        if (List.is_empty(fixed)) {
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
  switch (ProofSyntax.unwrap(e) |> Exp.term_of) {
  | Fun(p, body, _, _) =>
    let bound = Pat.bound_vars(p);
    if (List.mem(bound, self, ~equal=String.equal)) {
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
  | _ => go(~self, ~env, ~guards=Bounds.empty, e)
  }

/* `guards` is what the guards of the enclosing branches say about
 * numbers (`Bounds`). */
and go =
    (~self: Var.t, ~env: env, ~guards: Bounds.t, e: Exp.t)
    : result(list(list(pos)), reason) => {
  let go_all = (es: list(Exp.t)): result(list(list(pos)), reason) =>
    List.fold_left(
      ~f=
        (acc, e) => {
          let* xs = acc;
          let* ys = go(~self, ~env, ~guards, e);
          Ok(xs @ ys);
        },
      ~init=Ok([]),
      es,
    );
  /* Enter a subscope under pattern `p`, optionally seeding statuses for
   * its bindings from the matched scrutinee's status. */
  let under =
      (
        ~guards=guards,
        ~bind=(guards: Bounds.t) => guards,
        ~seed: option(status),
        p: Pat.t,
        e: Exp.t,
      )
      : result(list(list(pos)), reason) => {
    let bound = Pat.bound_vars(p);
    if (List.mem(bound, self, ~equal=String.equal)) {
      Ok([]) /* self shadowed: no recursive calls possible below */;
    } else {
      let seeded =
        switch (seed) {
        | Some({origin, strict}) =>
          /* Bindings under at least one constructor layer become
           * strict; a bare alias of an already-strict variable is
           * still strict (subterm of a subterm). */
          pat_bind(~origin, ~strict, p)
        | None => []
        };
      go(
        ~self,
        ~env=seeded @ shadow(env, bound),
        ~guards=bind(Bounds.forget(guards, bound)),
        e,
      );
    };
  };
  switch (e |> Exp.term_of) {
  | Var(x) when String.equal(x, self) =>
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
  | Test(e1) => go(~self, ~env, ~guards, e1)
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
  | UnOp(_, e1)
  | Standard(e1) => go(~self, ~env, ~guards, e1)
  | If(c, a, b) =>
    let* rc = go(~self, ~env, ~guards, c);
    let* ra = go(~self, ~env, ~guards=Bounds.assume(guards, c, true), a);
    let* rb = go(~self, ~env, ~guards=Bounds.assume(guards, c, false), b);
    Ok(rc @ ra @ rb);
  | DeferredAp(fn, args) => go_all([fn, ...args])
  | Fun(p, body, _, _) => under(~seed=None, p, body)
  | TypFun(_, body, _) => go(~self, ~env, ~guards, body)
  | Let(p, e1, e2) =>
    let* r1 = go(~self, ~env, ~guards, e1);
    let* r2 = under(~seed=var_status(env, e1), p, e2);
    Ok(r1 @ r2);
  /* A rule on a number is reached only past the literals of the rules
   * before it, and a variable pattern is the scrutinee. */
  | Match(scrut, rules) =>
    let* r0 = go(~self, ~env, ~guards, scrut);
    let seed = var_status(env, scrut);
    let scrut_var = Bounds.variable(scrut);
    let rec literal = (p: Pat.t) =>
      switch (Pat.term_of(p)) {
      | Parens(p)
      | Asc(p, _) => literal(p)
      | Atom(Int(n) | Nat(n)) => Some(n)
      | _ => None
      };
    let rec bare = (p: Pat.t) =>
      switch (Pat.term_of(p)) {
      | Parens(p)
      | Asc(p, _) => bare(p)
      | Var(x) => Some(x)
      | _ => None
      };
    let+ (rs, _) =
      List.fold_left(
        ~f=
          (acc, (p, rhs)) => {
            let* (xs, past) = acc;
            let bind = guards =>
              switch (scrut_var, bare(p)) {
              | (Some(x), Some(m)) => Bounds.alias(guards, m, x)
              | _ => guards
              };
            let* ys = under(~guards=past, ~bind, ~seed, p, rhs);
            let past =
              switch (scrut_var, literal(p)) {
              | (Some(x), Some(k)) => Bounds.add(past, (x, Bounds.Ne, k))
              | _ => past
              };
            Ok((xs @ ys, past));
          },
        ~init=Ok((r0, guards)),
        rules,
      );
    rs;
  /* A nested fixpoint must itself be structurally recursive; its body
   * is then still walked for calls to the outer self, with the inner
   * binder shadowing. The standardness routine judges the inner fix
   * too, but this check must stand on its own. */
  | FixF(p, body, _) =>
    let* _ =
      switch (check(e)) {
      | Ok () => Ok([])
      | Error(r) => Error("nested fixpoint: " ++ r)
      };
    let bound = Pat.bound_vars(p);
    if (List.mem(bound, self, ~equal=String.equal)) {
      Ok([]);
    } else {
      go(
        ~self,
        ~env=shadow(env, bound),
        ~guards=Bounds.forget(guards, bound),
        body,
      );
    };
  | Ap(_) =>
    let (head, args) = uncurry(e);
    switch (head |> Exp.term_of) {
    | Var(f) when String.equal(f, self) =>
      /* Arguments may themselves contain recursive calls. */
      let* inner = go_all(args);
      let posset = valid_positions(~guards, env, args);
      if (List.is_empty(posset)) {
        Error(
          "a recursive call to `"
          ++ f
          ++ "` passes no strict subterm of a parameter in any argument position",
        );
      } else {
        Ok(inner @ [posset]);
      };
    | _ =>
      let* rh = go(~self, ~env, ~guards, head);
      let* ra = go_all(args);
      Ok(rh @ ra);
    };
  /* Holes, modules, theorems, quantifiers, invalid text, ... (the
   * standardness routine doesn't judge them standard either). */
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Module(_)
  | ModuleExp(_)
  | Theorem(_)
  | Forall(_) =>
    Error("contains a form structural-recursion detection does not analyze")
  };
};
