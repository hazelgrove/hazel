/*

    _____       __         __  _ __        __  _
   / ___/__  __/ /_  _____/ /_(_) /___  __/ /_(_)___  ____
   \__ \/ / / / __ \/ ___/ __/ / __/ / / / __/ / __ \/ __ \
  ___/ / /_/ / /_/ (__  ) /_/ / /_/ /_/ / /_/ / /_/ / / / /
 /____/\__,_/_.___/____/\__/_/\__/\__,_/\__/_/\____/_/ /_/



 This file defines capture-avoiding substitution of EXPRESSIONS into every sort.
 It follows the rules at the top of FreeVars.re.

 POSTCONDITIONS:
  - The resulting expression never contains closures (so this can be used to remove closures after evaluation)
 */

/* `danger` holds every name a binder in `exp` could capture: the names sealed
   in it, those free in the values `env` inserts, and those renamed binders
   take. A binder whose names are all outside it is left alone without
   looking beneath it. */
let rec go =
        (
          memo: FreeVars.Memo.t(FreeVars.t),
          ~danger: FreeVars.Names.t,
          env: Environment.t(Exp.t),
          exp: Exp.t,
        ) =>
  Exp.map_term(
    ~f_typ=(_cont, t) => in_typ(memo, ~danger, env, t),
    ~f_exp=
      (cont, e) => {
        let (term, rewrap) = Exp.unwrap(e);
        switch (term) {
        | Var(x) =>
          switch (FreeVars.replacement(env, x)) {
          | Some(value) =>
            value |> Exp.replace_all_ids |> in_exp(Environment.empty)
          | None => e
          }

        // A closure is a scope barrier: only its own environment applies.
        | Closure(env, e) => go(memo, ~danger, env, e)
        | FixF(p, e, Some(env)) =>
          let (env', p', danger') = bind(memo, ~danger, env, p, e);
          FixF(p', go(memo, ~danger=danger', env', e), None) |> rewrap;

        // Cases with binders
        | Fun(p, e, t, n) =>
          let t' = Option.map(~f=in_typ(memo, ~danger, env), t);
          let (env', p', danger') = bind(memo, ~danger, env, p, e);
          Fun(p', go(memo, ~danger=danger', env', e), t', n) |> rewrap;
        /* The binder scopes over the body only, so the definition is
           substituted in the outer environment. A recursive let is no
           exception: its definition is elaborated to a FixF, which binds the
           name itself. */
        | Let(p, e1, e2) =>
          let (env', p', danger') = bind(memo, ~danger, env, p, e2);
          Let(
            p',
            go(memo, ~danger, env, e1),
            go(memo, ~danger=danger', env', e2),
          )
          |> rewrap;
        | Match(e, cases) =>
          Match(
            go(memo, ~danger, env, e),
            cases
            |> List.map(~f=((p, e)) => {
                 let (env', p', danger') = bind(memo, ~danger, env, p, e);
                 (p', go(memo, ~danger=danger', env', e));
               }),
          )
          |> rewrap
        | FixF(p, e, None) =>
          let (env', p', danger') = bind(memo, ~danger, env, p, e);
          FixF(p', go(memo, ~danger=danger', env', e), None) |> rewrap;
        | Theorem(p, e1, e2) =>
          let (env', p', danger') = bind(memo, ~danger, env, p, e2);
          Theorem(
            p',
            go(memo, ~danger, env, e1),
            go(memo, ~danger=danger', env', e2),
          )
          |> rewrap;
        | Forall(p, e) =>
          let (env', p', danger') = bind(memo, ~danger, env, p, e);
          Forall(p', go(memo, ~danger=danger', env', e)) |> rewrap;

        // Other cases: recurse
        | Invalid(_)
        | EmptyHole
        | MultiHole(_)
        | DynamicErrorHole(_)
        | Deferral(_)
        | Atom(_)
        | DrvQuote(_)
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
        | Projector(_)
        | Cons(_)
        | ListConcat(_)
        | UnOp(_)
        | BinOp(_)
        | BuiltinFun(_)
        | Asc(_)
        | LivelitName(_)
        | ProofObject(_)
        | Undefined
        | Module(_)
        | ModuleExp(_) => cont(e)
        };
      },
    exp,
  )

/* The environment for `body` under the binders of `p`, `p` with each binder
   renamed that must be, and `danger` with the new names. A binder is renamed
   if its name is sealed in `body` or free in a value `env` inserts there; the
   new name avoids every name free in `body`, before or after substitution,
   and the pattern's other binders. */
and bind =
    (
      memo: FreeVars.Memo.t(FreeVars.t),
      ~danger: FreeVars.Names.t,
      env: Environment.t(Exp.t),
      p: Pat.t,
      body: Exp.t,
    ) =>
  if (FreeVars.Names.is_empty(danger)
      || !
           List.exists(
             ~f=x => FreeVars.Names.mem(x, danger),
             Pat.bound_vars(p),
           )) {
    let (env', p') = in_pat(memo, ~danger, ~rename=x => x, env, env, p);
    (env', p', danger);
  } else {
    let bound = Pat.bound_vars(p);
    let beneath = FreeVars.exp(~memo, body);
    let inserted =
      FreeVars.Names.fold(
        (y, acc) =>
          switch (FreeVars.replacement(env, y)) {
          | Some(value) when !List.mem(bound, y, ~equal=String.equal) =>
            FreeVars.Names.union(
              FreeVars.all(FreeVars.exp(~memo, value)),
              acc,
            )
          | _ => acc
          },
        beneath.vars,
        FreeVars.Names.empty,
      );
    let captured = FreeVars.Names.union(inserted, beneath.sealed);
    let taken =
      ref(
        FreeVars.Names.union(
          FreeVars.all(beneath),
          FreeVars.Names.union(inserted, FreeVars.Names.of_list(bound)),
        ),
      );
    let widened = ref(danger);
    let rename = x =>
      if (FreeVars.Names.mem(x, captured)) {
        let rec fresh = name =>
          FreeVars.Names.mem(name, taken^)
            ? fresh(Var.next_name(name)) : name;
        let x' = fresh(x);
        taken := FreeVars.Names.add(x', taken^);
        widened := FreeVars.Names.add(x', widened^);
        x';
      } else {
        x;
      };
    let (env', p') = in_pat(memo, ~danger, ~rename, env, env, p);
    (env', p', widened^);
  }

and in_pat =
    (
      memo: FreeVars.Memo.t(FreeVars.t),
      ~danger: FreeVars.Names.t,
      ~rename: Var.t => Var.t,
      env_outer: Environment.t(Exp.t), // The environment outside the binding form, used in ProofOfs
      env_acc: Environment.t(Exp.t), // The environment being built up to avoid capture
      p: Pat.t,
    )
    : (Environment.t(Exp.t), Pat.t) => {
  let in_pat = in_pat(memo, ~danger, ~rename, env_outer);
  switch (p |> Pat.term_of) {
  // Variables: special case
  | Var(x) =>
    let x' = rename(x);
    (
      Environment.extend(env_acc, (x, Exp.fresh(Var(x')))),
      String.equal(x, x') ? p : Var(x') |> Pat.fresh,
    );

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
    let (env', p1') = in_pat(env_acc, p1);
    (env', Parens(p1') |> Pat.fresh);
  | Projector(data, p1) =>
    let (env', p1') = in_pat(env_acc, p1);
    (env', Projector(data, p1') |> Pat.fresh);
  | Tuple(l) =>
    let (env', l') =
      List.fold(
        ~f=
          ((env_acc, l_acc), p) => {
            let (env_new, p_new) = in_pat(env_acc, p);
            (env_new, l_acc @ [p_new]);
          },
        ~init=(env_acc, []),
        l,
      );
    (env', Tuple(l') |> Pat.fresh);
  | ListLit(l) =>
    let (env', l') =
      List.fold(
        ~f=
          ((env_acc, l_acc), p) => {
            let (env_new, p_new) = in_pat(env_acc, p);
            (env_new, l_acc @ [p_new]);
          },
        ~init=(env_acc, []),
        l,
      );
    (env', ListLit(l') |> Pat.fresh);
  | Cons(p1, p2) =>
    let (env', p1') = in_pat(env_acc, p1);
    let (env'', p2') = in_pat(env', p2);
    (env'', Cons(p1', p2') |> Pat.fresh);
  | Ap(p1, p2) =>
    let (env', p1') = in_pat(env_acc, p1);
    let (env'', p2') = in_pat(env', p2);
    (env'', Ap(p1', p2') |> Pat.fresh);
  | TupLabel(p1, p2) =>
    let (env', p1') = in_pat(env_acc, p1);
    let (env'', p2') = in_pat(env', p2);
    (env'', TupLabel(p1', p2') |> Pat.fresh);
  | Asc(p1, t) =>
    let t' = in_typ(memo, ~danger, env_outer, t);
    let (env', p1') = in_pat(env_acc, p1);
    (env', Asc(p1', t') |> Pat.fresh);
  };
}

and in_typ =
    (
      memo: FreeVars.Memo.t(FreeVars.t),
      ~danger: FreeVars.Names.t,
      env: Environment.t(Exp.t),
      typ: Typ.t,
    ) =>
  Typ.map_term(
    ~f_exp=(_cont, e) => go(memo, ~danger, env, e),
    /* Types can contain patterns: `Sig`'s `SigLet` items and `MultiHole`
       children. None bind expression variables — signature names are field
       labels — so recurse for their nested types and leave binders alone. */
    ~f_pat=(cont, p) => cont(p),
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
        | Projector(_)
        | Rec(_, _)
        | Poly(_, _)
        | ProdProjection(_, _)
        | ProdExtension(_, _)
        | ProofOf(_)
        | Sig(_)
        | DrvQuoteTy(_) => cont(t)
        };
      },
    typ,
  )

and in_exp = (env: Environment.t(Exp.t), exp: Exp.t): Exp.t => {
  let has_closure = exp => {
    let found = ref(false);
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e) =>
            switch (e.term) {
            | _ when found^ => e
            | Closure(_)
            | FixF(_, _, Some(_)) =>
              found := true;
              e;
            | _ => cont(e)
            },
        exp,
      );
    found^;
  };
  /* With nothing to replace and no closure to expand there is nothing to do:
     the stepper empties every term it shows of closures, and most values
     inserted are closed. */
  let is_empty =
    switch (env) {
    | Empty => true
    | E(_) => false
    };
  if (is_empty && !has_closure(exp)) {
    exp;
  } else {
    let memo = FreeVars.Memo.create(16);
    let names = FreeVars.exp(~memo, exp);
    let danger =
      FreeVars.Names.fold(
        (y, acc) =>
          switch (FreeVars.replacement(env, y)) {
          | Some(value) =>
            FreeVars.Names.union(
              FreeVars.all(FreeVars.exp(~memo, value)),
              acc,
            )
          | None => acc
          },
        names.vars,
        names.sealed,
      );
    go(memo, ~danger, env, exp);
  };
};
