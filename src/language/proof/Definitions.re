/* The program's definitions, as a proof sees them.
 *
 * Goals and facts are kept as written: they mention the program's
 * definitions by name (`len(xs)`), resolved in the environment of the
 * scope they are in. There a definition is bound to its value and a proof
 * variable to itself. A definition's body enters a goal only where a step
 * needs it: `step` looks names up as the evaluator does, and comparisons
 * unfold a name where the other side has structure (`MatchExp`). */

let is_definition = (env: Environment.t(Exp.t), x: Var.t): bool =>
  switch (Environment.lookup(env, x)) {
  | Some({term: Var(y), _}) => y != x
  | Some(_) => true
  | None => false
  };

/* The value `x` is defined as, closed and with fresh ids. */
let unfold = (env: Environment.t(Exp.t), x: Var.t): option(Exp.t) =>
  is_definition(env, x)
    ? Some(Substitution.in_exp(env, Exp.fresh(Var(x)))) : None;

/* `e` with each function value that is a definition in `env` written as
 * the definition's name: evaluating `len(x :: xs)` leaves the recursive
 * call as `len`'s fixpoint, or that fixpoint unrolled once, and this
 * gives back `1 + len(xs)`.
 *
 * A value is folded only where the name means that definition, so not
 * under a binder of `e` with the same name. Candidates are found by the
 * name the elaborator gives a function (`len+` for a recursive one), and
 * must equal the definition up to alpha once both are closed and any
 * fixpoint is unrolled once, since either side may hold either form. */
let fold = (env: Environment.t(Exp.t), e: Exp.t): Exp.t => {
  let unrolled = (e: Exp.t): Exp.t => {
    let e = Substitution.in_exp(Environment.empty, e);
    switch (Exp.term_of(e)) {
    | FixF(p, body, _) =>
      switch (ProofSyntax.pat_to_name(p)) {
      | Some(f) =>
        Substitution.in_exp(Environment.of_bindings([(f, e)]), body)
      | None => e
      }
    | _ => e
    };
  };
  let definitions = Hashtbl.create(8);
  let defines = (name: Var.t, value: Exp.t): bool => {
    let definition =
      switch (Hashtbl.find_opt(definitions, name)) {
      | Some(d) => d
      | None =>
        let d = unfold(env, name) |> Option.map(unrolled);
        Hashtbl.add(definitions, name, d);
        d;
      };
    switch (definition) {
    | Some(d) => Equality.semantic.exp(unrolled(value), d)
    | None => false
    };
  };
  let strip = (suffix: string, name: Var.t): Var.t => {
    let n = ref(String.length(name));
    while (n^ > 0 && String.sub(name, n^ - 1, 1) == suffix) {
      decr(n);
    };
    String.sub(name, 0, n^);
  };
  /* A builtin's fixpoint has no elaborated name, so its binder's is used,
   * without the primes substitution adds to avoid capture. */
  let rec fn_name = (e: Exp.t): option(Var.t) =>
    switch (Exp.term_of(e)) {
    | Closure(_, inner)
    | Parens(inner) => fn_name(inner)
    | FixF(p, inner, _) =>
      switch (fn_name(inner)) {
      | Some(_) as name => name
      | None => ProofSyntax.pat_to_name(p) |> Option.map(strip("'"))
      }
    | Fun(_, _, _, Some(name)) => Some(strip("+", name))
    | _ => None
    };
  let rec go = (bound: list(Var.t), e: Exp.t): Exp.t =>
    Exp.map_term(~f_exp=f_exp(bound), e)
  and f_exp = (bound, continue, e: Exp.t): Exp.t => {
    let (term, rewrap) = Exp.unwrap(e);
    let under = (p: Pat.t) => go(Pat.bound_vars(p) @ bound);
    switch (fn_name(e)) {
    | Some(name) when !List.mem(name, bound) && defines(name, e) =>
      Exp.fresh(Var(name))
    | _ =>
      switch (term) {
      | Fun(p, body, t, n) => Fun(p, under(p, body), t, n) |> rewrap
      | FixF(p, body, env) => FixF(p, under(p, body), env) |> rewrap
      | Forall(p, body) => Forall(p, under(p, body)) |> rewrap
      | Let(p, def, body) =>
        Let(p, go(bound, def), under(p, body)) |> rewrap
      | Match(scrut, rules) =>
        Match(
          go(bound, scrut),
          List.map(((p, branch)) => (p, under(p, branch)), rules),
        )
        |> rewrap
      /* Names in a closure's body resolve in its own environment. */
      | Closure(_) => e
      | _ => continue(e)
      }
    };
  };
  go([], e);
};
