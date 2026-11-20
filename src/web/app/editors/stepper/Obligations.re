open Language;

/* A theorem's obligations, as its header and its side-condition circles
 * show them: the status they give the theorem, and the exits that
 * discharge a pending one by patching the program. */

module Status = {
  /* What a theorem's header says. A proof that cites unfinished theorems
   * is proven relative to them, a deliberate assumption rather than a
   * gap. Any other open condition is a gap: the proof isn't proven, and
   * the header counts them, quietly, to help the user find them. */
  type t =
    | Proven
    /* The names of the unfinished theorems it cites. */
    | Relying(list(string))
    | Refuted
    /* The open conditions, citations aside. */
    | Incomplete(int);

  let of_proof = (pm: ProofMap.t, proof: Proof.t): t => {
    let (citations, gaps) =
      ProofMap.pending_obligations(pm, proof)
      |> List.partition_tf(~f=(ob: Obligation.t) =>
           Poly.equal(ob.kind, Citation)
         );
    /* A citation's origin is the `rewrite` step naming the theorem. */
    let rec find = (id, p: Proof.t) =>
      Id.equal(Proof.rep_id(p), id)
        ? Some(p) : List.find_map(~f=find(id), Proof.children(p));
    let cited = (ob: Obligation.t) =>
      switch (Option.map(~f=(p: Proof.t) => p.term, find(ob.origin, proof))) {
      | Some(AxiomStep({equality, _})) => ProofSyntax.exp_to_name(equality)
      | _ => None
      };
    switch (ProofMap.full_status_of_proof(pm, proof), gaps) {
    | (Proven, _) => Proven
    | (Refuted, _) => Refuted
    | (ProvenModulo(_), []) =>
      Relying(List.filter_map(~f=cited, citations) |> Util.ListUtil.dedup)
    | (ProvenModulo(_) | Incomplete, _) => Incomplete(List.length(gaps))
    };
  };

  let label = (status: t): string =>
    switch (status) {
    | Proven => "proven"
    | Relying(_) => "proven*"
    | Refuted => "refuted"
    | Incomplete(0) => "incomplete"
    | Incomplete(n) => "incomplete · " ++ string_of_int(n) ++ " open"
    };

  /* Class suffixes are kept in sync with theorems.css. */
  let class_ = (status: t): string =>
    switch (status) {
    | Proven => "true"
    | Relying(_) => "relying"
    | Refuted => "false"
    | Incomplete(_) => "unknown"
    };
};

/* A pending obligation's exits, each a patch to the program text.
 *
 * For a proof step's obligation:
 *   1. Prove here: wrap the proof region in `have <goal> proof ? => …`.
 *      The obligation is discharged at once by the have's hypothesis, and
 *      the `?` is proved later.
 *   2. Split on it: wrap the region in `cases <goal> | true =>
 *      <region> | false => ? end`. The `true` case's equation covers the
 *      obligation; the `false` case is left open.
 * Both are checked before they are offered: the wrapped proof is
 * checked again, and an exit that would break a step or leave the
 * obligation pending shows disabled. */
module Exits = {
  /* One exit. With no patch it shows disabled, its title saying why. */
  type t = {
    label: string,
    title: string,
    patch: option(Haz3lcore.EditorTransform.patch),
  };

  /* The theorem a step's exits patch: they wrap a region of the proof,
   * checked with the program's statics. */
  type ctx = {
    proof: Proof.t,
    proof_map: ProofMap.t,
    info_map: Statics.Map.t,
  };

  /* Is `origin` the id of a node in this proof subtree? */
  let rec proof_contains = (~origin: Id.t, p: Proof.t): bool =>
    Id.equal(Proof.rep_id(p), origin)
    || (
      switch (p.term) {
      | EmptyHole
      | Invalid(_)
      | MultiHole(_)
      | AxiomStep(_)
      | AlgebriteStep(_)
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
      | Have(_, _, sub, body) =>
        proof_contains(~origin, sub) || proof_contains(~origin, body)
      | Cases(_, cases)
      | Induction(_, cases) =>
        List.exists(~f=((_, body)) => proof_contains(~origin, body), cases)
      }
    );

  /* The goal, fit to write into the program, as in
   * `StepperPicker.Terms.embed_exp`. The goal is a slice of the program
   * and carries its tiles' ids: written in verbatim, one id would land on
   * two tiles, which crashes the view. */
  let embed_goal = (ob: Obligation.t): Exp.t =>
    ob.goal |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids;

  /* The proof region the exits wrap: the whole step chain of the
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
      | Have(_, _, sub, body) =>
        proof_contains(~origin, body)
          ? Some(body) : proof_contains(~origin, sub) ? Some(sub) : None
      | Cases(_, cases)
      | Induction(_, cases) =>
        List.find_map(
          ~f=
            ((_, body)) =>
              proof_contains(~origin, body) ? Some(body) : None,
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
    Proof.fresh(
      Have(embed_goal(ob), None, Proof.fresh(EmptyHole), region),
    );

  /* `cases <goal> | true => <region> | false => ? end`: an ordinary
   * case split on a bool, checked like any other. */
  let split = (ob: Obligation.t, region: Proof.t): Proof.t =>
    Proof.fresh(
      Cases(
        embed_goal(ob),
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
    |> Option.map(~f=region =>
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
              Id.equal(Proof.rep_id(p), Proof.rep_id(region))
                ? wrapper(ob, region) : continue(p),
          ctx.proof,
        );
      let checked =
        Evaluator.Proofs.check_proof(
          ~statics=ProofStatics.of_info_map(ctx.info_map),
          ~ctx=scope,
          ~goal,
          wrapped,
        );
      let breaks =
        Id.Map.exists(
          (id, entry: ProofMap.entry) =>
            List.is_empty(entry.marks)
            && !List.is_empty(ProofMap.marks_of(id, checked)),
          ctx.proof_map,
        );
      let pending =
        switch (ProofMap.lookup(ob.origin, checked)) {
        | Some(entry) =>
          List.exists(
            ~f=
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
      let answers = phys_equal(proof_map, ctx.proof_map) ? answers : [];
      let key = (label, ob.origin, Exp.rep_id(ob.goal));
      switch (List.Assoc.find(answers, key, ~equal=Poly.equal)) {
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
    Obligation.is_pending(ob)
      ? [
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
};
