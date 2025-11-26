/*

    _____       __         __  _ __        __  _
   / ___/__  __/ /_  _____/ /_(_) /___  __/ /_(_)___  ____
   \__ \/ / / / __ \/ ___/ __/ / __/ / / / __/ / __ \/ __ \
  ___/ / /_/ / /_/ (__  ) /_/ / /_/ /_/ / /_/ / /_/ / / / /
 /____/\__,_/_.___/____/\__/_/\__/\__,_/\__/_/\____/_/ /_/


 This file defines capture-avoiding substitution of EXPRESSIONS into every sort.

 PRECONDITIONS:
  - The environment should list all free variables (each free variable in the env should just be v ↦ v)
     - This is most important for variables that are free in entries in the environment itself

 POSTCONDITIONS:
  - The resulting expression never contains closures (so this can be used to remove closures after evaluation)

 KNOWN LIMITATIONS:
  - Does not handle unbound variables correctly - these could get captured
 */

let rec in_exp = (env: Environment.t(Exp.t), exp: Exp.t) =>
  Exp.map_term(
    ~f_typ=_ => in_typ(env),
    ~f_exp=
      (cont, e) => {
        let (term, rewrap) = Exp.unwrap(e);
        switch (term) {
        // Variables: lookup if bound
        | Var(x) =>
          switch (Environment.lookup(env, x)) {
          | Some({term: Var(y), _}) when y == x => e
          | Some(e) => e |> Exp.replace_all_ids |> in_exp(Environment.empty)
          | None => e
          }

        // Forms with environments: look up in new environment
        | Closure(env, e) => in_exp(env, e)
        | Fun(p, e, t, n) =>
          let t' = Option.map(in_typ(env), t);
          let (env', p') = in_pat(env, env, p);
          Fun(p', in_exp(env', e), t', n) |> rewrap;
        | FixF(p, e, Some(env)) =>
          let (env', p') = in_pat(env, env, p);
          FixF(p', in_exp(env', e), None) |> rewrap;

        // Cases with binders: remove binder from env
        | Let(p, e1, e2) =>
          let (env', p') = in_pat(env, env, p);
          Let(p', in_exp(env', e1), in_exp(env', e2)) |> rewrap;
        | Match(e, cases) =>
          Match(
            in_exp(env, e),
            cases
            |> List.map(((p, e)) => {
                 let (env', p') = in_pat(env, env, p);
                 (p', in_exp(env', e));
               }),
          )
          |> rewrap
        | FixF(p, e, None) =>
          let (env', p') = in_pat(env, env, p);
          FixF(p', in_exp(env', e), None) |> rewrap;
        | Theorem(p, e1, e2) =>
          let (env', p') = in_pat(env, env, p);
          Theorem(p', in_exp(env', e1), in_exp(env', e2)) |> rewrap;
        | Forall(pat, e) =>
          let (env', pat') = in_pat(env, env, pat);
          Forall(pat', in_exp(env', e)) |> rewrap;

        // Other cases: recurse
        | Invalid(_)
        | EmptyHole
        | MultiHole(_)
        | DynamicErrorHole(_)
        | Deferral(_)
        | Atom(_)
        | ListLit(_)
        | Constructor(_)
        | TypFun(_)
        | Tuple(_)
        | TupLabel(_)
        | TupleExtension(_)
        | Label(_)
        | ExplicitNonlabel
        | Dot(_)
        | TyAlias(_)
        | Use(_)
        | Ap(_)
        | TypAp(_)
        | DeferredAp(_)
        | If(_)
        | Seq(_)
        | Test(_)
        | HintedTest(_)
        | Filter(_)
        | Parens(_)
        | Probe(_)
        | Cons(_)
        | ListConcat(_)
        | UnOp(_)
        | BinOp(_)
        | BuiltinFun(_)
        | Asc(_)
        | LivelitName(_)
        | ProofObject(_)
        | Undefined => cont(e)
        };
      },
    exp,
  )

// Updates a pattern to avoid variable capture
and in_pat =
    (
      env_outer: Environment.t(Exp.t), // The environment outside the binding form, used in ProofOfs
      env_acc: Environment.t(Exp.t), // The environment being built up to avoid capture
      p: Pat.t,
    )
    : (Environment.t(Exp.t), Pat.t) => {
  switch (p |> Pat.term_of) {
  // Variables: special case
  | Var(x) =>
    let x' = Environment.free_name(x, env_acc);
    if (x == x') {
      (env_acc |> Environment.extend(_, (x', Exp.fresh(Var(x')))), p);
    } else {
      (
        env_acc
        |> Environment.extend(_, (x, Exp.fresh(Var(x'))))
        |> Environment.extend(_, (x', Exp.fresh(Var(x')))),
        Var(x') |> Pat.fresh,
      );
    };

  // Atomic forms
  | Atom(_) => (env_acc, p)
  | Wild => (env_acc, p)
  | Invalid(_) => (env_acc, p)
  | EmptyHole => (env_acc, p)
  | MultiHole(_) => (env_acc, p)
  | Constructor(_, _) => (env_acc, p)
  | ExplicitNonlabel => (env_acc, p)
  | Label(_) => (env_acc, p)

  // Containers: recurse one at a time
  | Parens(p1) =>
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    (env', Parens(p1') |> Pat.fresh);
  | Probe(p1, probe) =>
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    (env', Probe(p1', probe) |> Pat.fresh);
  | Tuple(l) =>
    let (env', l') =
      List.fold_left(
        ((env_acc, l_acc), p) => {
          let (env_new, p_new) = in_pat(env_outer, env_acc, p);
          (env_new, l_acc @ [p_new]);
        },
        (env_acc, []),
        l,
      );
    (env', Tuple(l') |> Pat.fresh);
  | ListLit(l) =>
    let (env', l') =
      List.fold_left(
        ((env_acc, l_acc), p) => {
          let (env_new, p_new) = in_pat(env_outer, env_acc, p);
          (env_new, l_acc @ [p_new]);
        },
        (env_acc, []),
        l,
      );
    (env', ListLit(l') |> Pat.fresh);
  | Cons(p1, p2) =>
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    let (env'', p2') = in_pat(env_outer, env', p2);
    (env'', Cons(p1', p2') |> Pat.fresh);
  | Ap(p1, p2) =>
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    let (env'', p2') = in_pat(env_outer, env', p2);
    (env'', Ap(p1', p2') |> Pat.fresh);
  | TupLabel(p1, p2) =>
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    let (env'', p2') = in_pat(env_outer, env', p2);
    (env'', TupLabel(p1', p2') |> Pat.fresh);
  | Asc(p1, t) =>
    let t' = in_typ(env_outer, t);
    let (env', p1') = in_pat(env_outer, env_acc, p1);
    (env', Asc(p1', t') |> Pat.fresh);
  };
}

and in_typ = (env: Environment.t(Exp.t), typ: Typ.t) =>
  Typ.map_term(
    ~f_exp=_ => in_exp(env),
    ~f_pat=
      (_, _) =>
        failwith("patterns should be handled separately in substitution"),
    ~f_typ=
      (cont, t) => {
        let (term, _rewrap) = Typ.unwrap(t);
        switch (term) {
        // Cases without patterns: recurse
        | Unknown(_)
        | Atom(_)
        | Var(_)
        | List(_)
        | Arrow(_, _)
        | Sum(_)
        | Prod(_)
        | ExplicitNonlabel
        | Label(_)
        | TupLabel(_, _)
        | Parens(_)
        | Probe(_)
        | Rec(_, _)
        | Poly(_, _)
        | ProdProjection(_, _)
        | ProdExtension(_, _)
        | ProofOf(_) => cont(t)
        };
      },
    typ,
  );
