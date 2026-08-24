open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* HARD equivalence examples, proved (or precisely pinned) with the
 * Phase-4 proof calculus: generalize + induction + eval/axiom/rewrite.
 *
 * Three families:
 *   A. the ACCUMULATOR pattern -- `acc + len(xs) == <fold>(xs, acc)`.
 *      The naive unaccumulated statement cannot close (the IH sits at
 *      accumulator 0 while the residual fold sits at 1); `generalize acc`
 *      fixes that, and the induction glue closes end-to-end.
 *   B. reverse INVOLUTION -- `rev(rev(xs)) == xs`, which needs a snoc
 *      helper that is ITSELF an induction.
 *   C. a Nat ROUND TRIP -- `of_int(to_int(n)) == n` over a recursive ADT.
 *
 * Reuses the Test_ProofMap harness and the Test_Revert dump, like
 * Test_Generalize does. Every "Incomplete"/"ProvenModulo" expectation
 * here is a deliberate PIN of a checker limitation, documented at the
 * test. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

/* Compact incoming/outgoing dump, for failure messages (Test_Revert). */
let print_exp = Test_ProofMap.print_exp;
let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => String.map(c => c == '\n' ? ' ' : c, print_exp(e))
  | None => "<none>"
  };
let rec dump = (pm: ProofMap.t, p: Proof.t, ind: string): string => {
  let (inc, out, marks) =
    switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
    | Some({incoming, outgoing, marks, _}) => (
        opt_exp_str(incoming),
        opt_exp_str(outgoing),
        String.concat("; ", List.map(ProofMark.show, marks)),
      )
    | None => ("<no entry>", "", "")
    };
  let here =
    ind
    ++ "in:  "
    ++ inc
    ++ "\n"
    ++ ind
    ++ "out: "
    ++ out
    ++ (marks == "" ? "" : "\n" ++ ind ++ "marks: " ++ marks);
  let kids =
    switch (p.term) {
    | Seq(a, b) => [dump(pm, a, ind), dump(pm, b, ind)]
    | Forall(_, b)
    | Assume(_, _, b)
    | Generalize(_, b)
    | Revert(_, _, b) => [dump(pm, b, ind ++ "  ")]
    | Induction(_, _, cases) =>
      List.map(((_, b)) => dump(pm, b, ind ++ "  "), cases)
    | _ => []
    };
  String.concat("\n", [here, ...kids]);
};

let check_status = (msg, expected: ProofMap.full_status, pm, proof) => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  if (actual != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(actual)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof, ""),
    );
  };
};

let check_proven = (msg, pm, proof) =>
  check_status(msg, ProofMap.Proven, pm, proof);

let check_incomplete = (msg, pm, proof) =>
  check_status(msg, ProofMap.Incomplete, pm, proof);

let check_mark_free = (msg, pm, proof) =>
  switch (Test_ProofMap.find_marked_sub(pm, proof)) {
  | None => ()
  | Some(_) =>
    Alcotest.fail(msg ++ "\n--- proof dump ---\n" ++ dump(pm, proof, ""))
  };

let check_no_obligations = (msg, pm, proof) =>
  Alcotest.check(
    Alcotest.int,
    msg,
    0,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );

let has_pattern_not_found = (pm, proof) =>
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.PatternNotFound(_) => true
    | _ => false,
  );

let has_nothing_to_step = (pm, proof) =>
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.NothingToStep(_) => true
    | _ => false,
  );

let has_possibly_divergent = (pm, proof) =>
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.PossiblyDivergentInstantiation(_) => true
    | _ => false,
  );

let has_unknown_equality = (pm, proof) =>
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.UnknownEquality(_) => true
    | _ => false,
  );

/* --- A. len vs a fold: the accumulator pattern ------------------------- */

let prelude_a = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in let add1 = fun (acc, _) -> acc + 1 in |};

/* Defining equations that DO close by one `eval` + `refl_eq`: `len` is
 * an ordinary structural recursion, and `fold_left`/`add1` at the empty
 * list reduce to the accumulator. */
let len_nil = {|theorem len_nil = len([]) == 0 proof eval len([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in |};
let len_cons = {|theorem len_cons = forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in |};
let fold_nil = {|theorem fold_nil = forall a: Int -> fold_left([], add1, a) == a proof eval fold_left([], add1, a) at 0 end; axiom refl_eq at 0 on a == a end in |};
let add1_step = {|theorem add1_step = forall a: Int -> forall b: Int -> add1((a, b)) == a + 1 proof eval add1((a, b)) at 0 end; axiom refl_eq at 0 on a + 1 == a + 1 end in |};

/* `shuffle` is pure arithmetic and closes by the Algebrite rewrite. */
let shuffle = {|theorem shuffle = forall a: Int -> forall b: Int -> a + (1 + b) == (a + 1) + b proof rewrite a + (1 + b) with (a + 1) + b at 0 end; axiom refl_eq at 0 on (a + 1) + b == (a + 1) + b end in |};

/* `fold_cons` is the ONE hole: see
 * test_refl_eq_refused_on_fold_left_expression below. */
let fold_cons_hole = {|theorem fold_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) proof ? in |};

let lemmas_a =
  len_nil ++ len_cons ++ fold_nil ++ add1_step ++ shuffle ++ fold_cons_hole;

let test_a_defining_equations_prove = () => {
  let cases = [
    ("len_nil", len_nil),
    ("len_cons", len_cons),
    ("fold_nil", fold_nil),
    ("add1_step", add1_step),
    ("shuffle", shuffle),
  ];
  List.iter(
    ((name, lemma)) => {
      let (pm, proof) = run_named(name, prelude_a ++ lemma ++ name);
      check_proven(
        "defining equation " ++ name ++ " closes by eval/rewrite + refl",
        pm,
        proof,
      );
    },
    cases,
  );
};

/* THE headline theorem: `forall xs -> forall acc -> acc + len(xs) ==
 * fold_left(xs, add1, acc)`.
 *
 * `generalize acc` makes the IH forall-quantified over the accumulator,
 * so the cons case can cite it at `acc + 1`. Note the closing move: the
 * IH is applied in the REVERSE direction (`axiomrev`), rewriting the
 * fold side back into `(acc + 1) + len(xs')`, so the final `refl_eq`
 * lands on a `len`-only expression. Applying it forward would leave
 * `fold_left(..) == fold_left(..)`, which `refl_eq` cannot close (the
 * instantiation gate; see the pin below). */
let thm_a = {|theorem lenfold = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, acc) end; rewrite acc + 0 with acc at 0 end; axiom refl_eq at 0 on acc == acc end | x :: xs' => forall acc => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, acc) end; axiom shuffle at 0 on acc + (1 + len(xs')) end; axiomrev ih at 0 on fold_left(xs', add1, acc + 1) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_generalized_accumulator_proves = () => {
  let (pm, proof) =
    run_named("lenfold", prelude_a ++ lemmas_a ++ thm_a ++ "lenfold");
  check_mark_free("the generalized len/fold proof is mark-free", pm, proof);
  check_no_obligations("no obligations", pm, proof);
  check_proven(
    "acc + len(xs) == fold_left(xs, add1, acc) is Proven",
    pm,
    proof,
  );
};

/* Negative control: WITHOUT the accumulator generalization the theorem
 * is `len(xs) == fold_left(xs, add1, 0)`. In the cons case `fold_cons`
 * moves the fold to accumulator `0 + 1`, but the IH only speaks about
 * accumulator `0` -- the cited fold does not occur in the goal, hence
 * PatternNotFound. This is the accumulator mismatch, concretely. */
let test_naive_accumulator_is_incomplete = () => {
  let src =
    prelude_a
    ++ lemmas_a
    ++ {|theorem naive = forall xs: [Int] -> len(xs) == fold_left(xs, add1, 0) proof induction xs | [] => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, 0) end; axiom refl_eq at 0 on 0 == 0 end | x :: xs' => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, 0) end; axiomrev ih at 0 on fold_left(xs', add1, 0) end; axiom refl_eq at 0 on $e == $e end end in naive|};
  let (pm, proof) = run_named("naive", src);
  Alcotest.check(
    Alcotest.bool,
    "the IH at accumulator 0 does not occur in the residual goal",
    true,
    has_pattern_not_found(pm, proof),
  );
  check_incomplete("the naive induction does not close", pm, proof);
};

/* PIN (why `fold_cons` must be a hole): `refl_eq` is
 * `forall x -> (x == x) == true`, and the Phase-3a instantiation gate
 * refuses any instantiation that Totality.check cannot show total.
 * The closure-library `fold_left` fix APPLIES its function parameter
 * `f`, an abstract variable with no visible definition, so the check
 * fails -- divergence is refused outright, never an obligation
 * (ProofCheck.instantiation_gate). Consequence: NO goal of the shape
 * `E == E` with `fold_left` in `E` can be closed by refl_eq, which is
 * exactly what proving `fold_cons` would require after the folds are
 * unfolded. The same statement about `len` (structurally recursive, no
 * abstract head) closes fine. */
let test_refl_eq_refused_on_fold_left_expression = () => {
  let src =
    prelude_a
    ++ {|theorem w = forall t: [Int] -> fold_left(t, add1, 0) == fold_left(t, add1, 0) proof axiom refl_eq at 0 on fold_left(t, add1, 0) == fold_left(t, add1, 0) end in w|};
  let (pm, proof) = run_named("w", src);
  Alcotest.check(
    Alcotest.bool,
    "refl_eq at a fold_left expression is refused as possibly divergent",
    true,
    has_possibly_divergent(pm, proof),
  );
  check_incomplete("so even X == X does not close", pm, proof);
  /* Control: the same shape over `len` is Proven. */
  let control =
    prelude_a
    ++ {|theorem w2 = forall t: [Int] -> len(t) == len(t) proof axiom refl_eq at 0 on len(t) == len(t) end in w2|};
  let (pm', proof') = run_named("w2", control);
  check_proven("control: refl_eq over `len` closes", pm', proof');
};

/* PIN (currying, not higher-orderness): with a FIRST-ORDER but CURRIED
 * accumulator the defining equation still does not close -- one `eval`
 * step only applies the outer argument, leaving
 * `(fun acc -> case h :: t | ...)(a)` un-beta-reduced, and further
 * `eval $e` steps advance the two sides of the equation out of step. */
let test_curried_accumulator_equation_incomplete = () => {
  let src = {|let fadd = fun xs -> fun acc -> case xs | [] => acc | _ :: t => fadd(t)(acc + 1) end in theorem fadd_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fadd(h :: t)(a) == fadd(t)(a + 1) proof eval fadd(h :: t)(a) at 0 end; axiom refl_eq at 0 on $e == $e end in fadd_cons|};
  let (pm, proof) = run_named("fadd_cons", src);
  check_incomplete(
    "the curried accumulator's cons equation does not close in one step",
    pm,
    proof,
  );
};

/* ...and UNCURRYING the accumulator removes every hole: the tupled
 * `fa((xs, acc))` reduces beta + case in a single visible `eval` step,
 * exactly like `len`, and it has no function parameter for the totality
 * check to choke on. This is the accumulator theorem with ZERO hole
 * lemmas and zero obligations -- proof that the generalize/induction
 * GLUE is complete, and that the `fold_left` friction above is entirely
 * about `fold_left`'s higher-order parameter. */
let prelude_fu = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in let fa = fun (xs, acc) -> case xs | [] => acc | _ :: t => fa((t, acc + 1)) end in |};
let lemmas_fu =
  len_nil
  ++ len_cons
  ++ {|theorem fa_nil = forall a: Int -> fa(([], a)) == a proof eval fa(([], a)) at 0 end; axiom refl_eq at 0 on a == a end in theorem fa_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fa((h :: t, a)) == fa((t, a + 1)) proof eval fa((h :: t, a)) at 0 end; axiom refl_eq at 0 on $e == $e end in |}
  ++ shuffle;
let thm_fu = {|theorem lenfa = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fa((xs, acc)) proof generalize acc => induction xs | [] => forall acc => axiom len_nil at 0 on len([]) end; axiom fa_nil at 0 on fa(([], acc)) end; rewrite acc + 0 with acc at 0 end; axiom refl_eq at 0 on acc == acc end | x :: xs' => forall acc => axiom len_cons at 0 on len(x :: xs') end; axiom fa_cons at 0 on fa((x :: xs', acc)) end; axiom shuffle at 0 on acc + (1 + len(xs')) end; axiomrev ih at 0 on fa((xs', acc + 1)) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_uncurried_accumulator_hole_free = () => {
  let (pm, proof) =
    run_named("lenfa", prelude_fu ++ lemmas_fu ++ thm_fu ++ "lenfa");
  check_mark_free("hole-free accumulator proof is mark-free", pm, proof);
  check_no_obligations("no obligations", pm, proof);
  check_proven(
    "acc + len(xs) == fa((xs, acc)) is Proven with no hole lemmas",
    pm,
    proof,
  );
};

/* --- B. reverse involution -------------------------------------------- */

let prelude_b = {|let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in |};

let rev_nil = {|theorem rev_nil = rev([]) == [] proof eval rev([]) at 0 end; axiom refl_eq at 0 on ([]: [Int]) == ([]: [Int]) end in |};
let rev_cons = {|theorem rev_cons = forall x: Int -> forall t: [Int] -> rev(x :: t) == rev(t) @ [x] proof eval rev(x :: t) at 0 end; axiom refl_eq at 0 on rev(t) @ [x] == rev(t) @ [x] end in |};

/* The APPEND equations are holes: `@` is a builtin, and it will not step
 * on a symbolic list argument -- see
 * test_append_equations_are_not_eval_provable. */
let app_holes = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof ? in theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof ? in |};

let lemmas_b = rev_nil ++ rev_cons ++ app_holes;

/* The helper that reverse involution needs -- and it is ITSELF an
 * induction over xs (with the outer `x` held fixed, so no `generalize`
 * is needed here). The nil case is pure computation: five `eval $e`
 * steps take `rev([] @ [x])` down to `x :: []`. The cons case is the
 * interesting one and closes with app_cons / rev_cons / IH / app_cons /
 * rev_cons. */
let snoc = {|theorem snoc = forall xs: [Int] -> forall x: Int -> rev(xs @ [x]) == x :: rev(xs) proof induction xs | [] => axiom rev_nil at 0 on rev([]) end; eval $e at 0 end; eval $e at 0 end; eval $e at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end | y :: t => axiom app_cons at 0 on (y :: t) @ [x] end; axiom rev_cons at 0 on rev(y :: (t @ [x])) end; axiom ih at 0 on rev(t @ [x]) end; axiom app_cons at 0 on (x :: rev(t)) @ [y] end; axiom rev_cons at 0 on rev(y :: t) end; axiom refl_eq at 0 on $e == $e end end in |};

let invol = {|theorem invol = forall xs: [Int] -> rev(rev(xs)) == xs proof induction xs | [] => axiom rev_nil at 0 on rev([]) end; axiom rev_nil at 0 on rev([]) end; axiom refl_eq at 0 on $e == $e end | x :: t => axiom rev_cons at 0 on rev(x :: t) end; axiom snoc at 0 on rev(rev(t) @ [x]) end; axiom ih at 0 on rev(rev(t)) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_rev_defining_equations_prove = () => {
  List.iter(
    ((name, lemma)) => {
      let (pm, proof) = run_named(name, prelude_b ++ lemma ++ name);
      check_proven("rev equation " ++ name ++ " closes by eval", pm, proof);
    },
    [("rev_nil", rev_nil), ("rev_cons", rev_cons)],
  );
};

/* PIN: `[] @ m` and `(y :: l) @ m` cannot be computed -- the builtin
 * append needs its list argument to be a value, and a quantified `m` is
 * not one, so the step reports NothingToStep. These two list-algebra
 * facts are therefore supplied as hole lemmas. */
let test_append_equations_are_not_eval_provable = () => {
  let src = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof eval [] @ m at 0 end; axiom refl_eq at 0 on m == m end in app_nil_l|};
  let (pm, proof) = run_named("app_nil_l", src);
  Alcotest.check(
    Alcotest.bool,
    "append on a symbolic tail has nothing to step",
    true,
    has_nothing_to_step(pm, proof),
  );
  check_incomplete("so the append equation stays open", pm, proof);
};

/* The snoc helper: an induction in its own right, and it PROVES (given
 * the two append equations). */
let test_snoc_proves = () => {
  let (pm, proof) =
    run_named("snoc", prelude_b ++ lemmas_b ++ snoc ++ "snoc");
  check_mark_free("the snoc induction is mark-free", pm, proof);
  check_proven(
    "rev(xs @ [x]) == x :: rev(xs) is Proven (induction on xs)",
    pm,
    proof,
  );
};

/* ...and with snoc in hand the involution is a short induction. */
let test_reverse_involution_proves = () => {
  let (pm, proof) =
    run_named("invol", prelude_b ++ lemmas_b ++ snoc ++ invol ++ "invol");
  check_mark_free("the involution proof is mark-free", pm, proof);
  check_no_obligations("no obligations", pm, proof);
  check_proven("rev(rev(xs)) == xs is Proven", pm, proof);
};

/* --- C. Nat round trip ------------------------------------------------- */

/* NOTE the ADT name. `Nat` is a RESERVED Hazel type keyword
 * (Token.re / Ctx.re / MakeTerm's `["Nat"] => Atom(Nat)`), so
 * `type Nat = +Z + S(Nat)` does not introduce a recursive ADT at all and
 * `induction n` over it generates NO inductive hypotheses -- pinned by
 * test_nat_is_a_reserved_type_name below. Use any other name. */
let prelude_c = {|type Nt = +Z + S(Nt) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun k -> if k <= 0 then Z else S(of_int(k - 1)) in |};

/* `to_int`'s equations are one-step evals. `of_int(0)` needs THREE eval
 * steps (unfold, decide `0 <= 0`, select the branch) -- Hazel's `if` has
 * no `end` delimiter, and writing one silently produces a hole-riddled
 * elaboration in which of_int does look stuck. */
let lemmas_c = {|theorem to_z = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in theorem of_z = of_int(0) == Z proof eval of_int(0) at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end in theorem of_s = forall k: Int -> k > 0 ==> of_int(k) == S(of_int(k - 1)) proof ? in theorem minus_one = forall j: Int -> 1 + j - 1 == j proof rewrite 1 + j - 1 with j at 0 end; axiom refl_eq at 0 on j == j end in |};

let round = {|theorem round = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => axiom to_z at 0 on to_int(Z) end; axiom of_z at 0 on of_int(0) end; axiom refl_eq at 0 on $e == $e end | S(m) => axiom to_s at 0 on to_int(S(m)) end; axiom of_s at 0 on of_int(1 + to_int(m)) end; axiom minus_one at 0 on 1 + to_int(m) - 1 end; axiom ih at 0 on of_int(to_int(m)) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_to_int_and_of_int_equations = () => {
  let to_z = {|theorem to_z = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in to_z|};
  let (pm, proof) = run_named("to_z", prelude_c ++ to_z);
  check_proven("to_int(Z) == 0 closes by eval", pm, proof);
  let to_s = {|theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in to_s|};
  let (pm, proof) = run_named("to_s", prelude_c ++ to_s);
  check_proven("to_int(S(m)) == 1 + to_int(m) closes by eval", pm, proof);
  /* of_int is NOT stuck: it just needs three steps through the `if`. */
  let of_z = {|theorem of_z = of_int(0) == Z proof eval of_int(0) at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end in of_z|};
  let (pm, proof) = run_named("of_z", prelude_c ++ of_z);
  check_proven("of_int(0) == Z closes in three eval steps", pm, proof);
};

/* PIN: the Algebrite `rewrite` step locates its target WITHOUT
 * environment substitution, so it cannot find `1 + to_int(m) - 1` once
 * an earlier axiom step has left the environment-substituted `to_int`
 * blob in the goal. An arithmetic AXIOM lemma (`minus_one`) does find it
 * -- axiom steps match up to the environment. */
let test_rewrite_does_not_match_through_the_environment = () => {
  let src =
    prelude_c
    ++ lemmas_c
    ++ {|theorem r = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => ? | S(m) => axiom to_s at 0 on to_int(S(m)) end; axiom of_s at 0 on of_int(1 + to_int(m)) end; rewrite 1 + to_int(m) - 1 with to_int(m) at 0 end end in r|};
  let (pm, proof) = run_named("r", src);
  Alcotest.check(
    Alcotest.bool,
    "the written `1 + to_int(m) - 1` is not found by the rewrite step",
    true,
    has_pattern_not_found(pm, proof),
  );
};

/* PIN: shadowing the reserved `Nat` type name yields an induction with
 * no IHs (citing `ih` is UnknownEquality); the identical program under a
 * non-reserved name has the IH available (the cited pattern is simply
 * absent from that stub goal, hence PatternNotFound rather than
 * UnknownEquality). */
let test_nat_is_a_reserved_type_name = () => {
  let bad = {|type Nat = +Z + S(Nat) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in theorem t = forall n: Nat -> to_int(n) == 0 proof induction n | Z => ? | S(m) => axiom ih at 0 on to_int(m) end end in t|};
  let (pm, proof) = run_named("t", bad);
  Alcotest.check(
    Alcotest.bool,
    "induction over a `Nat`-named ADT generates no IH",
    true,
    has_unknown_equality(pm, proof),
  );
  let good = {|type Nt = +Z + S(Nt) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in theorem t = forall n: Nt -> to_int(n) == 0 proof induction n | Z => ? | S(m) => axiom ih at 0 on to_int(m) end end in t|};
  let (pm', proof') = run_named("t", good);
  Alcotest.check(
    Alcotest.bool,
    "under a non-reserved name the IH exists",
    false,
    has_unknown_equality(pm', proof'),
  );
};

/* The round trip: mark-free and ProvenModulo. The single pending
 * obligation is `of_s`'s guard instantiated at the cons case,
 * `1 + to_int(m) > 0`; discharging it needs `to_int(m) >= 0`, which is
 * another induction over Nt (deliberately not supplied here). */
let test_nat_round_trip_proven_modulo = () => {
  let (pm, proof) =
    run_named("round", prelude_c ++ lemmas_c ++ round ++ "round");
  check_mark_free("the round-trip proof is mark-free", pm, proof);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.int,
    "exactly one obligation (of_s's positivity guard)",
    1,
    List.length(obs),
  );
  Alcotest.check(
    Alcotest.bool,
    "and it is pending",
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProofMap.ProvenModulo(_) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo, got "
      ++ ProofMap.show_full_status(other)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof, ""),
    )
  };
};

let tests = (
  "Equivalence inductions",
  [
    test_case(
      "A: len/fold/add1 defining equations close by eval",
      `Quick,
      test_a_defining_equations_prove,
    ),
    test_case(
      "A: generalize acc + induction proves acc + len(xs) == fold_left(xs, add1, acc)",
      `Quick,
      test_generalized_accumulator_proves,
    ),
    test_case(
      "A: control -- the naive unaccumulated statement cannot close",
      `Quick,
      test_naive_accumulator_is_incomplete,
    ),
    test_case(
      "A: pin -- refl_eq is refused on any fold_left expression",
      `Quick,
      test_refl_eq_refused_on_fold_left_expression,
    ),
    test_case(
      "A: pin -- a curried accumulator's cons equation does not close",
      `Quick,
      test_curried_accumulator_equation_incomplete,
    ),
    test_case(
      "A: the uncurried accumulator theorem proves with no hole lemmas",
      `Quick,
      test_uncurried_accumulator_hole_free,
    ),
    test_case(
      "B: rev defining equations close by eval",
      `Quick,
      test_rev_defining_equations_prove,
    ),
    test_case(
      "B: pin -- append equations are not eval-provable",
      `Quick,
      test_append_equations_are_not_eval_provable,
    ),
    test_case(
      "B: the snoc helper is itself an induction, and it proves",
      `Quick,
      test_snoc_proves,
    ),
    test_case(
      "B: reverse involution proves given snoc",
      `Quick,
      test_reverse_involution_proves,
    ),
    test_case(
      "C: to_int and of_int defining equations close by eval",
      `Quick,
      test_to_int_and_of_int_equations,
    ),
    test_case(
      "C: pin -- rewrite does not match through the environment",
      `Quick,
      test_rewrite_does_not_match_through_the_environment,
    ),
    test_case(
      "C: pin -- `Nat` is reserved, so its induction has no IH",
      `Quick,
      test_nat_is_a_reserved_type_name,
    ),
    test_case(
      "C: of_int(to_int(n)) == n is ProvenModulo the positivity guard",
      `Quick,
      test_nat_round_trip_proven_modulo,
    ),
  ],
);
