open Util;

/* Locating the subterm a proof focus selects.
 *
 * `from <nth> <exp> do` selects the nth subterm of the goal that equals
 * `<exp>`, counting left to right in the source and not descending into a
 * counted match. The stepper counts with `index`, which is 0-based; the checker
 * reads the occurrence back with `nth_env_target`. */

/* Immediate expression children, in `map_term` order, including those
 * inside patterns (`where` guards). Used to walk two
 * trees in lockstep when one is the env-substituted display of the other:
 * wrapping nodes (`Ap`, `BinOp`, …) keep their ids, but a `Var` may
 * become a `Fun`/`FixF` with a fresh id. */
let children = (e: Exp.t): list(Exp.t) => {
  let kids = ref([]);
  let root_id = Exp.rep_id(e);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, inner) =>
          if (Exp.rep_id(inner) == root_id) {
            cont(inner);
          } else {
            kids := [inner, ...kids^];
            inner;
          },
      e,
    );
  List.rev(kids^);
};

/* The subterms of `e` in source order, each before its own subterms, not
 * descending into a subterm `stop` accepts. `map_term` can't give this
 * order: OCaml evaluates constructor arguments right to left, so it
 * visits `b` before `a` in `a + b`. */
let rec in_source_order = (~stop: Exp.t => bool, e: Exp.t): list(Exp.t) => {
  let in_pat = (p: Pat.t): list(Exp.t) => {
    let found = ref([]);
    let _: Pat.t =
      Pat.map_term(
        ~f_exp=
          (_, e) => {
            found := [e, ...found^];
            e;
          },
        p,
      );
    List.rev(found^);
  };
  let kids =
    switch (Exp.term_of(e)) {
    | Invalid(_)
    | EmptyHole
    | Deferral(_)
    | Undefined
    | Atom(_)
    | Constructor(_)
    | Label(_)
    | ExplicitNonlabel
    | LivelitName(_)
    | Var(_)
    | BuiltinFun(_) => []
    | DynamicErrorHole(a, _)
    | TypFun(_, a, _)
    | TyAlias(_, _, a)
    | Use(_, a)
    | TypAp(a, _)
    | Test(a)
    | Filter(_, a)
    | Closure(_, a)
    | Parens(a)
    | Projector(_, a)
    | UnOp(_, a)
    | Asc(a, _) => [a]
    | TupLabel(a, b)
    | Dot(a, b)
    | Ap(_, a, b)
    | Seq(a, b)
    | HintedTest(a, b)
    | Cons(a, b)
    | ListConcat(a, b)
    | BinOp(_, a, b)
    | TupleExtension(a, b) => [a, b]
    | If(a, b, c) => [a, b, c]
    | ListLit(es)
    | Tuple(es) => es
    | DeferredAp(f, args) => [f, ...args]
    | Fun(p, body, _, _)
    | Forall(p, body)
    | FixF(p, body, _) => in_pat(p) @ [body]
    | Let(p, def, body) => in_pat(p) @ [def, body]
    | Match(scrut, rules) => [
        scrut,
        ...List.concat_map(((p, branch)) => in_pat(p) @ [branch], rules),
      ]
    /* Rare in a goal; their children's order is `map_term`'s. */
    | MultiHole(_)
    | DrvQuote(_)
    | Theorem(_)
    | Module(_)
    | ModuleExp(_) => children(e)
    };
  stop(e) ? [e] : [e, ...List.concat_map(in_source_order(~stop), kids)];
};

/* How many subterms equal to `e1` come before the node with `e1`'s id in
 * `e2`, not counting inside an earlier one. `None` if that id is not in
 * `e2`. */
let index = (e1: Exp.t, e2: Exp.t): option(int) => {
  let is_node = e => Exp.rep_id(e) == Exp.rep_id(e1);
  let is_equal = e => Equality.ignoring_ascriptions.exp(e, e1);
  let rec count = (n, subterms) =>
    switch (subterms) {
    | [] => None
    | [e, ..._] when is_node(e) => Some(n)
    | [e, ...rest] => count(is_equal(e) ? n + 1 : n, rest)
    };
  count(0, in_source_order(~stop=e => !is_node(e) && is_equal(e), e2));
};

/* The (n+1)th subterm of `e` that `matches`. */
let nth_matching = (matches: Exp.t => bool, n: int, e: Exp.t): option(Exp.t) =>
  List.nth_opt(
    in_source_order(~stop=matches, e) |> List.filter(matches),
    n,
  );

/* The (n+1)th occurrence of `e1` in `e2`. */
let nth = (e1: Exp.t, n: int, e2: Exp.t): option(Exp.t) =>
  nth_matching(exp => Equality.ignoring_ascriptions.exp(exp, e1), n, e2);

/* The (n+1)th subterm of `e2` that matches `pat`, an expression that may
 * contain `$e`/`$v`/`$x` metavariables (see MetaVar). A metavariable-free
 * pattern matches exactly the terms it is structurally equal to, so
 * indexing among matches generalizes indexing among occurrences. */
let nth_pattern =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      pat: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  nth_matching(MatchExp.matches_pattern(~info_map, ~env, pat), n, e2);

/* The subterm a selector names. Pattern matching
 * (MatchExp) and the equality `nth` uses (Equality) are different
 * relations, so only a target that uses the pattern language goes
 * through the matcher; a ground target is compared with Equality. */
let nth_target =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      selector: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  MetaVar.has_meta(selector)
    ? nth_pattern(~info_map, ~env, selector, n, e2) : nth(selector, n, e2);

/* `nth_target` with both sides env-substituted first.
 *
 * Proof-level `eval` steps run the evaluator, which substitutes the
 * runtime env into branch bodies and so inlines recursive bindings like
 * `rev` as `FixF(Var(name), ...)`. Selectors written in later steps still
 * say `Var(name)`, so both sides are substituted before matching, and
 * the match is mapped back to the unsubstituted `e2` by `Exp.rep_id`,
 * so callers see the goal as the user wrote it. `Substitution.in_exp`
 * preserves the ids of wrapping nodes (`Ap`, `Tuple`, `BinOp`, ...), so
 * the id is present in `e2`. It leaves metavariables alone, since no
 * binding can be named `$...`.
 *
 * Equality compares up to parens/projectors, ascriptions,
 * alpha-renaming (expression and type), function names, filters and
 * hole provenance, but not up to elaboration-supplied type information.
 * Two same-named constructors at different types are different terms,
 * and a step must not retarget across them. So both sides must be
 * elaborated: an unelaborated `Constructor(c, None)` never equals its
 * elaborated `Constructor(c, Some(Some(ty)))`, and the mismatch reads as
 * "target not found". This is why the theorem context records
 * elaborated statements (`Statics`' Theorem arm):
 * `ProofCtx.of_theorem_ctx` splices a cited rule's right-hand side into
 * elaborated goals.
 *
 * Substitution renames a binder of `e2` that shadows a name in `env`, so
 * a selector naming that binder's variable (`b` under `fun b ->` when
 * the theorem binds `b` too) only matches the goal as written, where it
 * is looked for when the substituted goal has no match. */
let nth_env_target =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      selector: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) => {
  let substituted =
    nth_target(
      ~info_map,
      ~env,
      Substitution.in_exp(env, selector),
      n,
      Substitution.in_exp(env, e2),
    );
  switch (substituted) {
  | Some(matched) => Exp.find_by_id(Exp.rep_id(matched), e2)
  | None => nth_target(~info_map, ~env, selector, n, e2)
  };
};

/* `exp` with the node whose id is `id` replaced by `new_exp`, ids and
 * all, so `new_exp` can be found again in the result. The caller ensures
 * none of its ids occur elsewhere in `exp`. */
let replace_by_id = (id: Id.t, exp: Exp.t, new_exp: Exp.t) =>
  Exp.map_term(
    ~f_exp=(cont, exp) => Exp.rep_id(exp) == id ? new_exp : cont(exp),
    exp,
  );

/* The id of the node in `onto` that occupies the same position as `id`
 * in `from`. `Some(id)` when the id survived substitution; the inlined
 * function when a `Var` was replaced by a `Fun`/`FixF`/`Closure`. */
let rec map_rep_id = (~from: Exp.t, ~onto: Exp.t, id: Id.t): option(Id.t) =>
  if (Exp.rep_id(from) == id) {
    Some(Exp.rep_id(onto));
  } else {
    let from_kids = children(from);
    let onto_kids = children(onto);
    List.length(from_kids) == List.length(onto_kids)
      ? List.fold_left2(
          (acc, f, o) =>
            switch (acc) {
            | Some(_) as s => s
            | None => map_rep_id(~from=f, ~onto=o, id)
            },
          None,
          from_kids,
          onto_kids,
        )
      : None;
  };

/* The patterns binding variables around the node with id `id` in `e`,
 * outermost first: the parameters of the `fun`s, `fix`es and `forall`s
 * it is inside, the patterns of the `let`s whose body it is in, and of
 * the case arms it is in. Empty if `id` isn't in `e`. */
let binders = (id: Id.t, e: Exp.t): list(Pat.t) => {
  let rec go = (outer: list(Pat.t), e: Exp.t): option(list(Pat.t)) =>
    if (Exp.rep_id(e) == id) {
      Some(List.rev(outer));
    } else {
      let under = p => go([p, ...outer]);
      switch (Exp.term_of(e)) {
      | Fun(p, _, _, _)
      | FixF(p, _, _)
      | Forall(p, _) => List.find_map(under(p), children(e))
      | Let(p, def, body) =>
        switch (go(outer, def)) {
        | Some(_) as found => found
        | None => under(p, body)
        }
      | Match(scrut, rules) =>
        switch (go(outer, scrut)) {
        | Some(_) as found => found
        | None => List.find_map(((p, branch)) => under(p, branch), rules)
        }
      | _ => List.find_map(go(outer), children(e))
      };
    };
  go([], e) |> Option.value(~default=[]);
};
