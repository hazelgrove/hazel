open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* HYPOTHESIS NAMING: fixed names + shadowing, `as` clauses, `alias`
 * (docs/prover-obligations.md, "Hypothesis naming").
 *
 * The rules under test:
 *
 *   1. Every auto-installed fact takes its BARE BASE NAME — `case_eq`,
 *      `ih`, `where`, `assume`, `have` — with NO freshening. A second
 *      introduction in an inner scope therefore SHADOWS the first
 *      (`SemanticCtx.hypothesis_name`).
 *   2. Citation resolves INNERMOST-FIRST, because `Ctx.extend` prepends
 *      and `Ctx.lookup_theorem` takes the first match. Citing a shadowed
 *      bare name is not an error: it denotes the nearest enclosing
 *      introduction. That is the load-bearing claim these tests pin.
 *   3. `induction <e> as <h>` names the split's case equation in EVERY
 *      case, and `assume <e> as <h>` names the assumption — so a deep
 *      leaf can still cite an OUTER introduction.
 *   4. `alias <h> = <fact> => ...` re-binds an already-known fact to a
 *      second name RETROACTIVELY: the escape hatch for a fact that has
 *      already been shadowed, and the only way to reach one of the two
 *      IHs a two-sub-term induction case installs (both are `ih`).
 *
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;

let run = (src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  (EvaluatorState.get_proof_map(state), proof_of(elab));
};

let marks_of = (pm, proof): list(ProofMark.t) =>
  switch (Test_ProofMap.find_marked_sub(pm, proof)) {
  | None => []
  | Some((_, marks)) => marks
  };

let check_mark_free = (msg, pm, proof) =>
  switch (marks_of(pm, proof)) {
  | [] => ()
  | marks =>
    Alcotest.fail(
      msg
      ++ ": expected no marks, got "
      ++ String.concat("; ", List.map(ProofMark.show, marks)),
    )
  };

let has_mark_kind = Test_ProofMap.has_mark_kind;

let check_has_unknown_equality = (msg, name, pm, proof) =>
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownEquality(n) => n == name
          | _ => false,
        )) {
    Alcotest.fail(
      msg
      ++ ": expected UnknownEquality(\""
      ++ name
      ++ "\"), got "
      ++ String.concat("; ", List.map(ProofMark.show, marks_of(pm, proof))),
    );
  };

/* WHICH fact did a name resolve to?
 *
 * `revert <name> => ?` resolves the name through `cited_fact` and cashes
 * THAT proposition into the body's goal as an antecedent
 * (`F ==> G`). So the reverted node's outgoing goal is a direct,
 * printable receipt of what the name denoted — which is exactly the
 * question shadowing raises, and a sharper instrument than mark-freeness
 * (an unrelated rewrite failure would also be a mark). */
let rec find_revert = (p: Proof.t): option(Proof.t) =>
  switch (p.term) {
  | Revert(_, _, _) => Some(p)
  | Seq(a, b) =>
    switch (find_revert(a)) {
    | Some(_) as r => r
    | None => find_revert(b)
    }
  | Forall(_, b)
  | Assume(_, _, b)
  | Alias(_, _, b)
  | Generalize(_, b) => find_revert(b)
  | Have(_, sub, b) =>
    switch (find_revert(sub)) {
    | Some(_) as r => r
    | None => find_revert(b)
    }
  | Induction(_, _, cases) =>
    List.find_map(((_, b)) => find_revert(b), cases)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | Contradiction(_)
  | EvalStep(_) => None
  };

let contains_substring = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

/* The antecedent a `revert <name>` cashed in, as printed text. Read off
   the BODY's incoming goal — that is the `F ==> G` the step built, and
   `F` is the resolved fact. (The revert node's own outgoing is its
   body's, which for a `?` body is the goal passed through.) */
let reverted_antecedent = (pm: ProofMap.t, proof: Proof.t): string =>
  switch (find_revert(proof)) {
  | None => Alcotest.fail("no revert node in the proof")
  | Some({term: Revert(_, _, body), _}) =>
    switch (ProofMap.lookup(Proof.rep_id(body), pm)) {
    | Some({incoming: Some(g), _}) => Test_ProofMap.print_exp(g)
    | _ => Alcotest.fail("the revert's body has no incoming goal")
    }
  | Some(_) => Alcotest.fail("find_revert returned a non-revert node")
  };

let check_resolves_to = (msg, ~name_denotes: string, pm, proof) => {
  let got = reverted_antecedent(pm, proof);
  if (!contains_substring(got, name_denotes)) {
    Alcotest.fail(
      msg
      ++ ": expected the cited name to denote `"
      ++ name_denotes
      ++ "`, but the reverted goal is: "
      ++ got,
    );
  };
};

/* --- 1. innermost-wins: the pin on `Ctx.lookup_theorem` --------------- */

/* Two nested `assume`s, both installed under the FIXED name `assume`.
 * Reverting the bare name shows which one it denotes: `Ctx.extend`
 * prepends and `Ctx.lookup_theorem` takes the first match, so it is the
 * INNER one. Citing a shadowed name is not an error — it just means the
 * nearest enclosing introduction. */
let nested_assume_src =
  "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof "
  ++ "assume x == 1 => assume y == 2 => revert assume => ? in t";

let test_shadowed_name_resolves_innermost = () => {
  let (pm, proof) = run(nested_assume_src);
  check_resolves_to(
    "the bare name `assume` must resolve to the INNER assumption",
    ~name_denotes="y == 2",
    pm,
    proof,
  );
};

/* The same claim on the OTHER lookup path. `revert` resolves a name
 * through `SemanticCtx`/`Ctx.lookup_theorem`; an AXIOM step resolves it
 * through `ProofCtx.lookup_rule` over `ProofCtx.of_theorem_ctx`. Both
 * have to agree, or a name would mean one thing to `revert` and another
 * to `axiom`. Here the two candidate facts have different left-hand
 * sides, so only the innermost reading can rewrite the target `m`. */
let test_axiom_step_also_resolves_innermost = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> forall m: Int -> n == 1 ==> m == 2 ==> m == m proof "
      ++ "assume n == 1 => assume m == 2 => axiom assume at 0 on m end; ? in t",
    );
  check_mark_free(
    "an axiom step citing a shadowed name must resolve to the INNER fact, "
    ++ "exactly as revert does",
    pm,
    proof,
  );
};

/* --- 2. `assume ... as <h>`: naming at introduction ------------------ */

/* With the inner assumption NAMED, the bare `assume` is no longer taken
 * by it, so the OUTER one is reachable again — and the named one is
 * reachable under its own name. Both facts, both citable. */
let test_assume_as_leaves_outer_citable = () => {
  let (pm, proof) =
    run(
      "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof "
      ++ "assume x == 1 => assume y == 2 as hy => revert assume => ? in t",
    );
  check_resolves_to(
    "naming the inner assumption leaves the outer one as `assume`",
    ~name_denotes="x == 1",
    pm,
    proof,
  );
};

let test_assume_as_name_denotes_its_own_fact = () => {
  let (pm, proof) =
    run(
      "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof "
      ++ "assume x == 1 => assume y == 2 as hy => revert hy => ? in t",
    );
  check_resolves_to(
    "an `as` name denotes the assumption it was attached to",
    ~name_denotes="y == 2",
    pm,
    proof,
  );
};

/* --- 3. `induction ... as <h>` cited from a DEEP leaf ---------------- */

/* Two nested splits. Unnamed, the inner split's `case_eq` shadows the
 * outer one, so at depth the bare name denotes the INNER equation. */
let two_splits = (outer_as: string, inner_as: string, cite: string): string =>
  "theorem t = forall n: Int -> forall m: Int -> n == n proof "
  ++ "induction n > 0"
  ++ outer_as
  ++ " | true => induction m > 0"
  ++ inner_as
  ++ " | true => revert "
  ++ cite
  ++ " => ? | false => ? end | false => ? end in t";

let test_bare_case_eq_at_depth_is_the_inner_one = () => {
  let (pm, proof) = run(two_splits("", "", "case_eq"));
  check_resolves_to(
    "at depth the bare `case_eq` is the INNER split's equation",
    ~name_denotes="m > 0",
    pm,
    proof,
  );
};

/* Named, the outer split's equation is citable from the deep leaf — the
 * migration's central idiom (see Test_Milestone_STLC's `progress`). */
let test_as_named_split_cited_from_deep_leaf = () => {
  let (pm, proof) = run(two_splits(" as hn", "", "hn"));
  check_resolves_to(
    "an `as`-named split is citable from inside a nested split",
    ~name_denotes="n > 0",
    pm,
    proof,
  );
  check_mark_free("citing an as-named outer split is mark-free", pm, proof);
};

/* --- 4. duplicate `as` names shadow too ----------------------------- */

/* Naming is not a uniqueness constraint. Two splits may ask for the same
 * name; the inner one shadows the outer, exactly as the fixed names do. */
let test_duplicate_as_name_shadows = () => {
  let (pm, proof) = run(two_splits(" as h", " as h", "h"));
  check_resolves_to(
    "a duplicate `as` name shadows: it denotes the INNER split",
    ~name_denotes="m > 0",
    pm,
    proof,
  );
};

/* --- 5. `alias`: retroactive naming past a shadow -------------------- */

/* The escape hatch. The outer split is UNNAMED, so by the time we are
 * inside the inner split its `case_eq` is shadowed. An `alias` taken in
 * the outer scope — where the bare name still resolves to it — carries
 * it in under a name the inner split cannot take. */
let test_alias_keeps_outer_fact_past_a_shadow = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> forall m: Int -> n == n proof "
      ++ "induction n > 0 | true => alias hn = case_eq => "
      ++ "induction m > 0 | true => revert hn => ? "
      ++ "| false => ? end | false => ? end in t",
    );
  check_resolves_to(
    "an alias taken before the shadowing split keeps the outer fact",
    ~name_denotes="n > 0",
    pm,
    proof,
  );
  check_mark_free("alias is mark-free", pm, proof);
};

/* An alias resolves its fact the way `revert` does, so it also accepts
 * the proposition spelled out. That is the ONLY way to reach a fact that
 * is ALREADY shadowed where the alias is written — the situation the two
 * IHs of a two-sub-term induction case create, since both are `ih`. */
let test_alias_by_spelled_out_proposition = () => {
  let (pm, proof) =
    run(
      "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof "
      ++ "assume x == 1 => assume y == 2 => alias hx = x == 1 => "
      ++ "revert hx => ? in t",
    );
  check_resolves_to(
    "alias resolves a spelled-out proposition, reaching a shadowed fact",
    ~name_denotes="x == 1",
    pm,
    proof,
  );
};

/* Aliasing something that is NOT in scope reuses revert's mark family
 * rather than inventing one, and — like every refusal in this checker —
 * recovers by checking the body against the unchanged goal. */
let test_alias_unknown_fact_marks = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> n == n proof "
      ++ "alias hn = n == 99 => axiom refl_eq at 0 on n == n end in t",
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownFactReverted => true
          | _ => false,
        )) {
    Alcotest.fail(
      "aliasing a fact that is not in scope should mark "
      ++ "UnknownFactReverted, got "
      ++ String.concat("; ", List.map(ProofMark.show, marks_of(pm, proof))),
    );
  };
};

/* An alias asserts nothing new, so it is obligation-free and
 * goal-preserving: a proof that closes without it closes with it. */
let test_alias_incurs_no_obligation = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> n == 1 ==> n == n proof "
      ++ "assume n == 1 => alias hn = assume => "
      ++ "axiom refl_eq at 0 on n == n end in t",
    );
  check_mark_free("alias is mark-free and goal-preserving", pm, proof);
  /* Zero, not "one fewer": the `assume` here is an implication INTRO
     (the goal's antecedent is exactly what it assumes), which is
     obligation-free, and the alias adds nothing on top. */
  Alcotest.check(
    Alcotest.int,
    "alias incurs no obligation of its own",
    0,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );
};

/* --- 6. names that were never introduced ---------------------------- */

let test_unknown_name_is_unknown_equality = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> n == n proof "
      ++ "induction n > 0 as hn | true => axiom hzz at 0 on n > 0 end; ? "
      ++ "| false => ? end in t",
    );
  check_has_unknown_equality(
    "citing a name that was never introduced",
    "hzz",
    pm,
    proof,
  );
};

/* An `as` name is scoped to the form that introduced it. */
let test_as_name_is_scoped_to_its_form = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> n == n proof "
      ++ "induction n > 0 as hn | true => ? | false => ? end; "
      ++ "axiom hn at 0 on n > 0 end in t",
    );
  check_has_unknown_equality(
    "an `as` name does not escape its form",
    "hn",
    pm,
    proof,
  );
};

let tests = (
  "HypothesisNaming",
  [
    test_case(
      "shadowed bare name resolves innermost",
      `Quick,
      test_shadowed_name_resolves_innermost,
    ),
    test_case(
      "axiom steps resolve innermost too",
      `Quick,
      test_axiom_step_also_resolves_innermost,
    ),
    test_case(
      "assume-as leaves the outer assume citable",
      `Quick,
      test_assume_as_leaves_outer_citable,
    ),
    test_case(
      "an as-name denotes its own fact",
      `Quick,
      test_assume_as_name_denotes_its_own_fact,
    ),
    test_case(
      "bare case_eq at depth is the inner split's",
      `Quick,
      test_bare_case_eq_at_depth_is_the_inner_one,
    ),
    test_case(
      "as-named split cited from a deep leaf",
      `Quick,
      test_as_named_split_cited_from_deep_leaf,
    ),
    test_case(
      "duplicate as-names shadow",
      `Quick,
      test_duplicate_as_name_shadows,
    ),
    test_case(
      "alias keeps an outer fact past a shadow",
      `Quick,
      test_alias_keeps_outer_fact_past_a_shadow,
    ),
    test_case(
      "alias resolves a spelled-out proposition",
      `Quick,
      test_alias_by_spelled_out_proposition,
    ),
    test_case(
      "alias of an unknown fact marks",
      `Quick,
      test_alias_unknown_fact_marks,
    ),
    test_case(
      "alias incurs no obligation",
      `Quick,
      test_alias_incurs_no_obligation,
    ),
    test_case(
      "a name never introduced is UnknownEquality",
      `Quick,
      test_unknown_name_is_unknown_equality,
    ),
    test_case(
      "an as-name does not escape its form",
      `Quick,
      test_as_name_is_scoped_to_its_form,
    ),
  ],
);
