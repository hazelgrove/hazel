open Language;

/* A theorem's obligations, as its header and its side-condition circles
 * show them: the status they give the theorem, and the exits that
 * discharge a pending one by patching the program. */

module Status = {
  /* ProvenModulo reads as proven: the goal did reach `true`. The header's
   * counter of open obligations, and the class, set it apart. */
  let label = (status: ProofMap.full_status): string =>
    switch (status) {
    | Proven
    | ProvenModulo(_) => "proven true"
    | Refuted => "disproven"
    | Incomplete => "incomplete"
    };

  /* Class suffixes are kept in sync with theorems.css. */
  let class_ = (status: ProofMap.full_status): string =>
    switch (status) {
    | Proven => "true"
    | Refuted => "false"
    | ProvenModulo(_) => "modulo"
    | Incomplete => "unknown"
    };

  /* The obligations the header counts: the proof's pending ones. Function
   * contracts don't count, as they don't toward the status. */
  let open_count = (pm: ProofMap.t, proof: Proof.t): int =>
    List.length(ProofMap.pending_obligations(pm, proof));
};

/* A pending obligation's exits, each a patch to the program text.
 *
 * For a proof step's obligation:
 *   1. Add to statement: `forall n ->` becomes `forall n where <goal> ->`,
 *      extending an existing guard with `&&`. Only for the statement's
 *      binders: on an induction case's binder a restriction breaks
 *      exhaustiveness, so the exit shows disabled, with that reason.
 *   2. Prove here: wrap the proof region in `have <goal> proof ? => …`.
 *      The obligation is discharged at once by the have's hypothesis, and
 *      the `?` is proved later.
 *   3. Split on it: wrap the region in `induction <goal> | true =>
 *      <region> | false => ? end`. The `true` case's equation covers the
 *      obligation; the `false` case is left open.
 * Exits 2 and 3 are checked before they are offered: the wrapped proof is
 * checked again, and an exit that would break a step or leave the
 * obligation pending shows disabled.
 *
 * For a function's contract, which no theorem's text can discharge:
 *   Add to contract: `fun x ->` becomes `fun x where <goal> ->`. */
module Exits = {
  /* One exit. With no patch it shows disabled, its title saying why. */
  type t = {
    label: string,
    title: string,
    patch: option(Haz3lcore.EditorTransform.patch),
  };

  /* The theorem a step's exits patch: exit 1 rewrites a binder of the
   * statement, exits 2 and 3 wrap a region of the proof, checked with
   * the program's statics. */
  type ctx = {
    stmt: option(Exp.t),
    proof: Proof.t,
    proof_map: ProofMap.t,
    info_map: Statics.Map.t,
  };

  /* One theorem-level `forall` binder of the statement, kept whole so the
   * float patch can rebuild the node in place. */
  type binder = {
    /* The Forall node itself: the ExpPatch target. */
    id: Id.t,
    pat: Pat.t,
    guard: option(Exp.t),
    body: Exp.t,
    vars: list(Var.t),
  };

  /* The statement's theorem-level binders, outermost first. Only the
   * unbroken prefix of `forall`s counts: those are the binders whose
   * restriction is the public statement's. */
  let rec binders_of_stmt = (e: Exp.t): list(binder) =>
    switch (Exp.term_of(e)) {
    | Forall(p, body) =>
      /* `forall p where g -> body` is `Forall(Where(p, g), body)`: split
         the restriction back off the binder's pattern. */
      let (bare, guard) = Exp.split_pat_guard(p);
      [
        {
          id: Exp.rep_id(e),
          pat: bare,
          guard,
          body,
          vars: Pat.bound_vars(bare),
        },
        ...binders_of_stmt(body),
      ];
    | Parens(e') => binders_of_stmt(e')
    | _ => []
    };

  /* Is `origin` the id of a node in this proof subtree? */
  let rec proof_contains = (~origin: Id.t, p: Proof.t): bool =>
    Proof.rep_id(p) == origin
    || (
      switch (p.term) {
      | EmptyHole
      | Invalid(_)
      | MultiHole(_)
      | AxiomStep(_)
      | AlgebriteStep(_)
      | Contradiction(_)
      | EvalStep => false
      | Seq(a, b) =>
        proof_contains(~origin, a) || proof_contains(~origin, b)
      | Focus({body, _})
      | Parens(body)
      | Forall(_, body)
      | Funext(_, body)
      | Assume(_, _, body)
      | Generalize(_, body)
      | Alias(_, _, body)
      | Revert(_, _, body) => proof_contains(~origin, body)
      | Have(_, sub, body) =>
        proof_contains(~origin, sub) || proof_contains(~origin, body)
      | Induction(_, _, cases) =>
        List.exists(((_, body)) => proof_contains(~origin, body), cases)
      }
    );

  /* Variables bound by induction cases on the path from the proof's root
   * to `origin`: a goal mentioning one can't be added to the statement. A
   * `forall` step's binder doesn't count: the statement already declares
   * it. */
  let rec case_vars_to = (~origin: Id.t, ~acc: list(Var.t), p: Proof.t) =>
    if (Proof.rep_id(p) == origin) {
      acc;
    } else {
      switch (p.term) {
      | EmptyHole
      | Invalid(_)
      | MultiHole(_)
      | AxiomStep(_)
      | AlgebriteStep(_)
      | Contradiction(_)
      | EvalStep => acc
      | Seq(a, b) =>
        proof_contains(~origin, a)
          ? case_vars_to(~origin, ~acc, a) : case_vars_to(~origin, ~acc, b)
      | Focus({body, _})
      | Parens(body)
      | Forall(_, body)
      | Assume(_, _, body)
      | Generalize(_, body)
      | Alias(_, _, body)
      | Revert(_, _, body) => case_vars_to(~origin, ~acc, body)
      /* Unlike `forall`'s, a `funext` binder is not the statement's: the
         statement says `f == g`, and `x` exists only inside the step. */
      | Funext(pat, body) =>
        case_vars_to(~origin, ~acc=Pat.bound_vars(pat) @ acc, body)
      | Have(_, sub, body) =>
        proof_contains(~origin, sub)
          ? case_vars_to(~origin, ~acc, sub)
          : case_vars_to(~origin, ~acc, body)
      | Induction(_, _, cases) =>
        switch (
          List.find_opt(
            ((_, body)) => proof_contains(~origin, body),
            cases,
          )
        ) {
        | Some((pat, body)) =>
          case_vars_to(~origin, ~acc=Pat.bound_vars(pat) @ acc, body)
        | None => acc
        }
      };
    };

  /* Where exit 1 adds the goal. */
  type float_target =
    /* The innermost theorem binder the goal mentions. */
    | FloatTo(binder)
    /* The goal mentions a case-local binder: a restriction there is
     * unsound, so the exit is disabled with this reason. */
    | UnsoundAtCase
    /* The goal mentions this variable, bound inside the statement or by
     * the proof, which isn't in scope at the statement's binders. */
    | BoundLater(Var.t)
    /* No theorem binder is mentioned, so there is nothing to restrict
     * (ground obligations, or a goal over local definitions only). */
    | NoBinder;

  /* The names of the variables in `e`. */
  let variables = (e: Exp.t): list(Var.t) => {
    let names = ref([]);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (continue, e) =>
            switch (Exp.term_of(e)) {
            | Var(x) =>
              names := [x, ...names^];
              e;
            | _ => continue(e)
            },
        e,
      );
    names^;
  };

  let float_target_of = (~ctx: ctx, ob: Obligation.t): float_target => {
    let goal = ob.goal;
    let mentions = (v: Var.t) => ProofRule.occurs_free_any([v], goal);
    let case_vars =
      proof_contains(~origin=ob.origin, ctx.proof)
        ? case_vars_to(~origin=ob.origin, ~acc=[], ctx.proof) : [];
    /* Case-scoping wins: even if the goal also mentions a theorem binder,
     * the restriction would have to hold at the case to be usable there. */
    /* The proof's own scope, before any of its steps, once it is checked. */
    let outer =
      ProofMap.lookup(Proof.rep_id(ctx.proof), ctx.proof_map)
      |> Option.map((entry: ProofMap.entry) =>
           SemanticCtx.get_ctx(entry.ctx)
         );
    let in_step_scope = x =>
      List.exists(
        (entry: Ctx.entry) =>
          switch (entry) {
          | VarEntry({name, _}) => name == x
          | ConstructorEntry(_)
          | TVarEntry(_)
          | LivelitEntry(_)
          | TheoremEntry(_) => false
          },
        ob.bindings,
      );
    let bound_later =
      Option.bind(outer, outer =>
        List.find_opt(
          x =>
            mentions(x)
            && in_step_scope(x)
            && Ctx.lookup_var(outer, x) == None,
          variables(goal),
        )
      );
    switch (bound_later) {
    | _ when List.exists(mentions, case_vars) => UnsoundAtCase
    | Some(x) => BoundLater(x)
    | None =>
      let binders =
        switch (ctx.stmt) {
        | Some(stmt) => binders_of_stmt(stmt)
        | None => []
        };
      switch (List.filter(b => List.exists(mentions, b.vars), binders)) {
      | [] => NoBinder
      | mentioned => FloatTo(Util.ListUtil.last(mentioned))
      };
    };
  };

  /* The goal, fit to write into the program, as in
   * `StepperPicker.Terms.embed_exp`. The goal is a slice of the program
   * and carries its tiles' ids: written in verbatim, one id would land on
   * two tiles, which crashes the view. */
  let embed_goal = (ob: Obligation.t): Exp.t =>
    ob.goal |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids;

  /* `p` restricted by `<goal>`, or with an existing guard `g`, by
   * `g && <goal>`: a `forall`'s binder or a `fun`'s parameter. */
  let restrict = (~goal: Exp.t, pat: Pat.t, guard: option(Exp.t)): Pat.t => {
    let guard =
      switch (guard) {
      | None => goal
      | Some(g) => Exp.fresh(BinOp(Bool(And), g, goal))
      };
    Pat.fresh(Where(pat, guard));
  };

  let float_patch =
      (~ctx: ctx, ob: Obligation.t): option(Haz3lcore.EditorTransform.patch) =>
    switch (float_target_of(~ctx, ob)) {
    | FloatTo(b) =>
      Some(
        Haz3lcore.EditorTransform.mk_patch(
          ~target_id=b.id,
          Exp.fresh(
            Forall(restrict(~goal=embed_goal(ob), b.pat, b.guard), b.body),
          ),
        ),
      )
    | UnsoundAtCase
    | BoundLater(_)
    | NoBinder => None
    };

  /* The proof region exits 2 and 3 wrap: the whole step chain of the
   * innermost scope holding the step that incurred the obligation. It has
   * to be the innermost, or a goal mentioning a case's variable would be
   * wrapped outside the case that binds it. Taking the whole chain keeps
   * the wrapper's body a complete `Seq` spine. */
  let region_of = (~ctx: ctx, ob: Obligation.t): option(Proof.t) => {
    let origin = ob.origin;
    let rec scope_body_containing = (p: Proof.t): option(Proof.t) =>
      switch (p.term) {
      | Forall(_, body)
      | Funext(_, body)
      | Assume(_, _, body)
      | Generalize(_, body)
      | Alias(_, _, body)
      | Revert(_, _, body) =>
        proof_contains(~origin, body) ? Some(body) : None
      | Have(_, sub, body) =>
        proof_contains(~origin, body)
          ? Some(body) : proof_contains(~origin, sub) ? Some(sub) : None
      | Induction(_, _, cases) =>
        List.find_map(
          ((_, body)) => proof_contains(~origin, body) ? Some(body) : None,
          cases,
        )
      | Seq(a, b) =>
        switch (scope_body_containing(a)) {
        | Some(_) as r => r
        | None => scope_body_containing(b)
        }
      /* Parens are not a scope. Nor is a focus: a wrapper inside one would
         have the focused subterm for its goal. */
      | Parens(inner)
      | Focus({body: inner, _}) => scope_body_containing(inner)
      | EmptyHole
      | Invalid(_)
      | MultiHole(_)
      | AxiomStep(_)
      | AlgebriteStep(_)
      | Contradiction(_)
      | EvalStep => None
      };
    let rec region_containing = (root: Proof.t): option(Proof.t) =>
      if (!proof_contains(~origin, root)) {
        None;
      } else {
        switch (scope_body_containing(root)) {
        | Some(body) => region_containing(body)
        | None => Some(root)
        };
      };
    region_containing(ctx.proof);
  };

  /* `have <goal> proof ? => <region>`. */
  let have = (ob: Obligation.t, region: Proof.t): Proof.t =>
    Proof.fresh(Have(embed_goal(ob), Proof.fresh(EmptyHole), region));

  /* `induction <goal> | true => <region> | false => ? end`: an ordinary
   * case split on a bool, checked like any other. */
  let split = (ob: Obligation.t, region: Proof.t): Proof.t =>
    Proof.fresh(
      Induction(
        embed_goal(ob),
        None,
        [
          (Pat.fresh(Atom(Bool(true))), region),
          (Pat.fresh(Atom(Bool(false))), Proof.fresh(EmptyHole)),
        ],
      ),
    );

  let wrap_patch =
      (wrapper, ~ctx: ctx, ob: Obligation.t)
      : option(Haz3lcore.EditorTransform.patch) =>
    region_of(~ctx, ob)
    |> Option.map(region =>
         Haz3lcore.EditorTransform.mk_proof_patch(
           ~target_id=Proof.rep_id(region),
           wrapper(ob, region),
         )
       );
  let have_patch = wrap_patch(have);
  let split_patch = wrap_patch(split);

  /* Answers of `wrap_problem` for one proof map. A long proof takes a
   * while to check, and the view asks again on every redraw while the
   * condition is open, so answers are kept until the proof is checked
   * again. */
  let checked_wraps:
    ref((ProofMap.t, list(((string, Id.t, Id.t), option(string))))) =
    ref((ProofMap.empty, []));

  /* Why `wrapper` can't wrap `ob`'s region, if it can't. The theorem's
   * proof is checked again with the region wrapped, without the new
   * syntax's statics, so marks on the wrapper itself (an untyped split)
   * don't count: the wrap must keep every clean step clean and discharge
   * `ob`. Splitting on a condition the goal mentions rewrites the goal of
   * the steps below, for one. */
  let wrap_problem =
      (wrapper, ~ctx: ctx, ob: Obligation.t, region: Proof.t): option(string) =>
    switch (ProofMap.lookup(Proof.rep_id(ctx.proof), ctx.proof_map)) {
    | Some({incoming: Some(goal), ctx: scope, _}) =>
      let wrapped =
        Proof.map_term(
          ~f_proof=
            (continue, p) =>
              Proof.rep_id(p) == Proof.rep_id(region)
                ? wrapper(ob, region) : continue(p),
          ctx.proof,
        );
      let checked =
        Evaluator.Proofs.check_proof(
          ~statics=ctx.info_map,
          ~ctx=scope,
          ~goal,
          wrapped,
        );
      let breaks =
        Id.Map.exists(
          (id, entry: ProofMap.entry) =>
            entry.marks == [] && ProofMap.marks_of(id, checked) != [],
          ctx.proof_map,
        );
      let pending =
        switch (ProofMap.lookup(ob.origin, checked)) {
        | Some(entry) =>
          List.exists(
            (o: Obligation.t) =>
              Obligation.is_pending(o) && Exp.fast_equal(o.goal, ob.goal),
            entry.obligations,
          )
        | None => false
        };
      breaks
        ? Some("Unavailable: it would break a step of the proof")
        : pending
            ? Some("Unavailable: it wouldn't discharge this condition") : None;
    | Some(_)
    | None => None
    };

  /* A step's exits: none once the obligation is discharged. */
  let for_step = (~ctx: ctx, ob: Obligation.t): list(t) => {
    let wrap_problem = (~label, wrapper, region) => {
      let (proof_map, answers) = checked_wraps^;
      let answers = proof_map === ctx.proof_map ? answers : [];
      let key = (label, ob.origin, Exp.rep_id(ob.goal));
      switch (List.assoc_opt(key, answers)) {
      | Some(answer) => answer
      | None =>
        let answer = wrap_problem(wrapper, ~ctx, ob, region);
        checked_wraps := (ctx.proof_map, [(key, answer), ...answers]);
        answer;
      };
    };
    let wrap = (~label, ~title, wrapper) =>
      switch (region_of(~ctx, ob)) {
      | None => {
          label,
          title: "Unavailable: the step that needs this isn't in the theorem's proof",
          patch: None,
        }
      | Some(region) =>
        switch (wrap_problem(~label, wrapper, region)) {
        | Some(problem) => {
            label,
            title: problem,
            patch: None,
          }
        | None => {
            label,
            title,
            patch: wrap_patch(wrapper, ~ctx, ob),
          }
        }
      };
    let float =
      switch (float_target_of(~ctx, ob)) {
      | FloatTo(b) => {
          label: "Add to statement",
          title:
            "Restrict the theorem's `forall "
            ++ String.concat(", ", b.vars)
            ++ "` with this condition (changes the statement)",
          patch: float_patch(~ctx, ob),
        }
      | UnsoundAtCase => {
          label: "Add to statement",
          title: "Unavailable: a restriction at a case is unsound — it would break exhaustiveness",
          patch: None,
        }
      | BoundLater(x) => {
          label: "Add to statement",
          title:
            "Unavailable: this goal mentions `"
            ++ x
            ++ "`, which isn't in scope at the statement's binders",
          patch: None,
        }
      | NoBinder => {
          label: "Add to statement",
          title: "Unavailable: this goal mentions no quantified variable of the statement",
          patch: None,
        }
      };
    Obligation.is_pending(ob)
      ? [
        float,
        wrap(
          ~label="Prove here",
          ~title=
            "Wrap this part of the proof in `have <goal> proof ? => …` and prove it later",
          have,
        ),
        wrap(
          ~label="Split on it",
          ~title=
            "Case-split on this condition: the `true` branch gets it as a fact, the `false` branch stays open",
          split,
        ),
      ]
      : [];
  };

  /* A contract's one exit. `func` is the function's syntax in the
   * program being edited, or None when it isn't there (it is defined in
   * another cell). */
  let for_contract = (~func: option(Exp.t), ob: Obligation.t): list(t) => {
    let label = "Add to contract";
    let exit =
      switch (func) {
      | Some({term: Fun(p, body, typ, name), _} as f) =>
        let (pat, guard) = Exp.split_pat_guard(p);
        {
          label,
          title: "Restrict the function's parameter with this condition (changes its contract)",
          patch:
            Some(
              Haz3lcore.EditorTransform.mk_patch(
                ~target_id=Exp.rep_id(f),
                Exp.fresh(
                  Fun(
                    restrict(~goal=embed_goal(ob), pat, guard),
                    body,
                    typ,
                    name,
                  ),
                ),
              ),
            ),
        };
      | _ => {
          label,
          title: "Unavailable: the function isn't written in this cell",
          patch: None,
        }
      };
    Obligation.is_pending(ob) ? [exit] : [];
  };
};
