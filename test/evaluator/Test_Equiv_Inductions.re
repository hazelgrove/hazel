open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* Harder equivalence proofs: generalize + induction +
 * eval/axiom/rewrite.
 *
 * Three families:
 *   A. the accumulator pattern, `acc + len(xs) == <fold>(xs, acc)`.
 *      The unaccumulated statement can't close (the IH is at
 *      accumulator 0 while the residual fold is at 1); `generalize acc`
 *      fixes that.
 *   B. reverse involution, `rev(rev(xs)) == xs`, which needs a snoc
 *      helper that is itself an induction.
 *   C. a round trip `of_int(to_int(n)) == n` over a recursive ADT.
 *
 * Families A and B have no hole lemmas. Every
 * "Incomplete"/"ProvenModulo" expectation pins a limitation, documented
 * at the test: (i) the unaccumulated statement, which shows the wrong
 * accumulator rather than a checker limit; (ii) family C's `of_s`
 * positivity obligation, not discharged; (iii) the two rewrite/`Nat`
 * pins in C. */

let has_mark_kind = Test_ProofMap.has_mark_kind;

let check_incomplete = (msg, pm, proof) =>
  check_status(msg, ProofMap.Incomplete, pm, proof);

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

/* Defining equations that close by one `eval` + `refl_eq`: `len` is
 * an ordinary structural recursion, and `fold_left`/`add1` at the empty
 * list reduce to the accumulator. */
let len_nil = {|theorem len_nil = len([]) == 0 proof eval len([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in |};
let len_cons = {|theorem len_cons = forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in |};
let fold_nil = {|theorem fold_nil = forall a: Int -> fold_left([], add1, a) == a proof eval fold_left([], add1, a) at 0 end; axiom refl_eq at 0 on a == a end in |};
let add1_step = {|theorem add1_step = forall a: Int -> forall b: Int -> add1((a, b)) == a + 1 proof eval add1((a, b)) at 0 end; axiom refl_eq at 0 on a + 1 == a + 1 end in |};

/* `shuffle` is pure arithmetic and closes by the Algebrite rewrite. */
let shuffle = {|theorem shuffle = forall a: Int -> forall b: Int -> a + (1 + b) == (a + 1) + b proof rewrite a + (1 + b) with (a + 1) + b at 0 end; axiom refl_eq at 0 on (a + 1) + b == (a + 1) + b end in |};

/* `fold_cons` closes because:
 *   (1) the closing `refl_eq` lands on an expression containing
 *       `fold_left`, which applies its quantified function parameter,
 *       and a quantified `f` is a total map, so the instantiation gate
 *       lets it through;
 *   (2) enough `eval` steps make both sides converge to the same `case`
 *       on `t`. There is no refolding: the residual is the inlined `fix`
 *       body, not the surface call `fold_left(t, ..)`. The right side
 *       unfolds to the same inlined shape, and `refl_eq` matches the two
 *       up to alpha-renaming of the `fix`/`fun` binders.
 * It takes exactly three `eval $e` after the targeted step. Fewer leaves
 * the sides out of phase; more also fails, because the two sides then
 * step at different rates. See test_fold_cons_proves_by_eval. */
let fold_cons = {|theorem fold_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) proof eval fold_left(h :: t, add1, a) at 0 end; eval $e at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end in |};

let lemmas_a =
  len_nil ++ len_cons ++ fold_nil ++ add1_step ++ shuffle ++ fold_cons;

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

/* The main theorem: `forall xs -> forall acc -> acc + len(xs) ==
 * fold_left(xs, add1, acc)`.
 *
 * `generalize acc` makes the IH forall-quantified over the accumulator,
 * so the cons case can cite it at `acc + 1`. This version applies the IH
 * in the reverse direction (`axiomrev`), rewriting the fold side back
 * into `(acc + 1) + len(xs')`, so the final `refl_eq` lands on a
 * `len`-only expression. thm_a_fwd below closes the other way round. */
let thm_a = {|theorem lenfold = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, acc) end; rewrite acc + 0 with acc at 0 end; axiom refl_eq at 0 on acc == acc end | x :: xs' => forall acc => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, acc) end; axiom shuffle at 0 on acc + (1 + len(xs')) end; axiomrev ih at 0 on fold_left(xs', add1, acc + 1) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_generalized_accumulator_proves = () => {
  let (pm, proof) =
    run_named("lenfold", prelude_a ++ lemmas_a ++ thm_a ++ "lenfold");
  check_mark_free("the generalized len/fold proof is mark-free", pm, proof);
  check_obligation_count("no obligations", 0, pm, proof);
  check_proven(
    "acc + len(xs) == fold_left(xs, add1, acc) is Proven",
    pm,
    proof,
  );
};

/* The same theorem closing the other way. Here the IH is cited forward,
 * rewriting `(acc + 1) + len(xs')` into `fold_left(xs', add1, acc + 1)`,
 * so the residual goal is
 * `fold_left(xs', add1, acc + 1) == fold_left(xs', add1, acc + 1)` and
 * the closing move is a plain `axiom refl_eq`. That passes the
 * instantiation gate although `fold_left` applies its abstract function
 * parameter, because a quantified `f` is a total map. */
let thm_a_fwd = {|theorem lenfoldf = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, acc) end; rewrite acc + 0 with acc at 0 end; axiom refl_eq at 0 on acc == acc end | x :: xs' => forall acc => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, acc) end; axiom shuffle at 0 on acc + (1 + len(xs')) end; axiom ih at 0 on (acc + 1) + len(xs') end; axiom refl_eq at 0 on $e == $e end end in |};

let test_forward_ih_accumulator_proves = () => {
  let (pm, proof) =
    run_named("lenfoldf", prelude_a ++ lemmas_a ++ thm_a_fwd ++ "lenfoldf");
  check_mark_free("the forward-IH len/fold proof is mark-free", pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "closing refl_eq on a residual fold_left == fold_left is not refused",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_obligation_count("no obligations", 0, pm, proof);
  check_proven(
    "forward-IH version closes on refl_eq at fold_left == fold_left",
    pm,
    proof,
  );
};

/* `fold_cons` proves by eval alone. This also pins the shape of what
 * happens, so a failure is legible: `eval` doesn't refold. The residual
 * after the targeted step is the inlined `fold_left` fix body applied to
 * `(t, add1, ..)`, not the surface call; the proof closes only because
 * the right-hand side unfolds to the same inlined shape and `refl_eq`
 * matches them up to alpha-renaming. So the step count is brittle in
 * both directions: exactly three `eval $e`. */
let test_fold_cons_proves_by_eval = () => {
  let steps = n =>
    prelude_a
    ++ {|theorem fc = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) proof eval fold_left(h :: t, add1, a) at 0 end; |}
    ++ String.concat("", List.init(n, _ => {|eval $e at 0 end; |}))
    ++ {|axiom refl_eq at 0 on $e == $e end in fc|};
  let (pm, proof) = run_named("fc", steps(3));
  Alcotest.check(
    Alcotest.bool,
    "eval DOES step fold_left on a cons -- this was never a stuck-step pin",
    false,
    has_nothing_to_step(pm, proof),
  );
  Alcotest.check(
    Alcotest.bool,
    "and the closing refl_eq is not refused as divergent",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_mark_free("fold_cons by eval is mark-free", pm, proof);
  check_proven(
    "fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) by eval + refl",
    pm,
    proof,
  );
  /* The count is exact in both directions: too few steps leaves the two
   * sides out of phase, too many steps advances them out of phase
   * again. */
  List.iter(
    n => {
      let (pm, proof) = run_named("fc", steps(n));
      check_incomplete(
        "fold_cons does not close with " ++ string_of_int(n) ++ " `eval $e`",
        pm,
        proof,
      );
    },
    [0, 1, 2, 4, 5],
  );
};

/* Negative control: without the accumulator generalization the theorem
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

/* `refl_eq` is `forall x -> (x == x) == true`, and the instantiation
 * gate refuses any instantiation Totality.check can't show total. The
 * `fold_left` fix applies its function parameter `f`, an abstract
 * variable, but a quantified function parameter is a total map, so
 * `fold_left(t, add1, 0) == fold_left(t, add1, 0)` is Proven, like the
 * `len` control. (A divergent instantiation such as `omega == omega` is
 * still refused; see Test_FunctionTotality.) */
let test_refl_eq_proves_on_fold_left_expression = () => {
  let src =
    prelude_a
    ++ {|theorem w = forall t: [Int] -> fold_left(t, add1, 0) == fold_left(t, add1, 0) proof axiom refl_eq at 0 on fold_left(t, add1, 0) == fold_left(t, add1, 0) end in w|};
  let (pm, proof) = run_named("w", src);
  Alcotest.check(
    Alcotest.bool,
    "refl_eq at a fold_left expression is not refused as divergent",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_mark_free("and it leaves no marks", pm, proof);
  check_proven("so X == X over fold_left closes", pm, proof);
  /* Control: the same shape over `len` is Proven. */
  let control =
    prelude_a
    ++ {|theorem w2 = forall t: [Int] -> len(t) == len(t) proof axiom refl_eq at 0 on len(t) == len(t) end in w2|};
  let (pm', proof') = run_named("w2", control);
  check_proven("control: refl_eq over `len` closes", pm', proof');
};

/* Currying is not a barrier, but it makes the step count matter. One
 * `eval` step applies only the outer argument, leaving
 * `(fun acc -> case h :: t | ...)(a)` un-beta-reduced. `eval $e` advances
 * whichever side is reducible, and at exactly five extra steps the two
 * sides land on the same shape and `refl_eq` closes; four is too few and
 * six too many. `eval` can't say "reduce the left side until it matches
 * the right", so the user must count. That is a usability problem, not
 * a limit of the calculus. Same as `fold_cons` above. */
let test_curried_accumulator_equation_is_step_count_brittle = () => {
  let steps = n =>
    {|let fadd = fun xs -> fun acc -> case xs | [] => acc | _ :: t => fadd(t)(acc + 1) end in theorem fadd_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fadd(h :: t)(a) == fadd(t)(a + 1) proof eval fadd(h :: t)(a) at 0 end; |}
    ++ String.concat("", List.init(n, _ => {|eval $e at 0 end; |}))
    ++ {|axiom refl_eq at 0 on $e == $e end in fadd_cons|};
  let (pm, proof) = run_named("fadd_cons", steps(5));
  check_mark_free(
    "the curried cons equation is mark-free at 5 steps",
    pm,
    proof,
  );
  check_proven(
    "the curried accumulator's cons equation DOES close, at exactly five `eval $e`",
    pm,
    proof,
  );
  List.iter(
    n => {
      let (pm, proof) = run_named("fadd_cons", steps(n));
      check_incomplete(
        "the curried cons equation is out of phase at "
        ++ string_of_int(n)
        ++ " `eval $e`",
        pm,
        proof,
      );
    },
    [0, 1, 2, 3, 4, 6, 7],
  );
};

/* Uncurrying the accumulator avoids the counting: the tupled
 * `fa((xs, acc))` reduces beta + case in a single `eval` step, like
 * `len`. */
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
  check_obligation_count("no obligations", 0, pm, proof);
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

/* The append equations. `@` steps on constructor form, so `[] @ m` and
 * `(y :: l) @ m` each take one `eval` step with `l`/`m` still
 * quantified, and `refl_eq` closes (test_append_equations_prove_by_eval). */
let app_lemmas = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof eval [] @ m at 0 end; axiom refl_eq at 0 on m == m end in theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof eval (y :: l) @ m at 0 end; axiom refl_eq at 0 on y :: (l @ m) == y :: (l @ m) end in |};

let lemmas_b = rev_nil ++ rev_cons ++ app_lemmas;

/* The helper that reverse involution needs, itself an induction over xs
 * (with the outer `x` held fixed, so no `generalize` is needed). The nil
 * case is pure computation: five `eval $e`
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

/* `[] @ m` and `(y :: l) @ m` step although `m` is quantified, because
 * `@` steps on constructor form: each closes by one `eval` + `refl_eq`,
 * with no marks. */
let test_append_equations_prove_by_eval = () => {
  let nil_l = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof eval [] @ m at 0 end; axiom refl_eq at 0 on m == m end in app_nil_l|};
  let (pm, proof) = run_named("app_nil_l", nil_l);
  Alcotest.check(
    Alcotest.bool,
    "`[] @ m` is not stuck",
    false,
    has_nothing_to_step(pm, proof),
  );
  check_mark_free("the `[] @ m` proof is mark-free", pm, proof);
  check_proven("`[] @ m == m` is Proven for quantified m", pm, proof);
  let cons = {|theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof eval (y :: l) @ m at 0 end; axiom refl_eq at 0 on y :: (l @ m) == y :: (l @ m) end in app_cons|};
  let (pm', proof') = run_named("app_cons", cons);
  check_mark_free("the `(y :: l) @ m` proof is mark-free", pm', proof');
  check_proven(
    "`(y :: l) @ m == y :: (l @ m)` is Proven for quantified l, m",
    pm',
    proof',
  );
};

/* The snoc helper: an induction in its own right, on top of the two
 * append lemmas. */
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
  check_obligation_count("no obligations", 0, pm, proof);
  check_proven("rev(rev(xs)) == xs is Proven", pm, proof);
};

/* --- C. Nat round trip ------------------------------------------------- */

/* The ADT isn't called `Nat`, which is a reserved Hazel type name
 * (Token.re / Ctx.re / MakeTerm's `["Nat"] => Atom(Nat)`):
 * `type Nat = +Z + S(Nat)` doesn't introduce a recursive ADT, and
 * `induction n` over it generates no inductive hypotheses
 * (test_nat_is_a_reserved_type_name). */
let prelude_c = {|type Nt = +Z + S(Nt) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun k -> if k <= 0 then Z else S(of_int(k - 1)) in |};

/* `to_int`'s equations are one-step evals. `of_int(0)` needs three eval
 * steps (unfold, decide `0 <= 0`, select the branch). Hazel's `if` has
 * no `end` delimiter, and writing one produces a hole-riddled
 * elaboration in which of_int looks stuck. */
let lemmas_c = {|theorem to_z = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in theorem of_z = of_int(0) == Z proof eval of_int(0) at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end in theorem of_s = forall k: Int -> k > 0 ==> of_int(k) == S(of_int(k - 1)) proof ? in theorem minus_one = forall j: Int -> 1 + j - 1 == j proof rewrite 1 + j - 1 with j at 0 end; axiom refl_eq at 0 on j == j end in |};

let round = {|theorem round = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => axiom to_z at 0 on to_int(Z) end; axiom of_z at 0 on of_int(0) end; axiom refl_eq at 0 on $e == $e end | S(m) => axiom to_s at 0 on to_int(S(m)) end; axiom of_s at 0 on of_int(1 + to_int(m)) end; axiom minus_one at 0 on 1 + to_int(m) - 1 end; axiom ih at 0 on of_int(to_int(m)) end; axiom refl_eq at 0 on $e == $e end end in |};

let test_to_int_and_of_int_equations = () => {
  let to_z = {|theorem to_z = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in to_z|};
  let (pm, proof) = run_named("to_z", prelude_c ++ to_z);
  check_proven("to_int(Z) == 0 closes by eval", pm, proof);
  let to_s = {|theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in to_s|};
  let (pm, proof) = run_named("to_s", prelude_c ++ to_s);
  check_proven("to_int(S(m)) == 1 + to_int(m) closes by eval", pm, proof);
  /* of_int isn't stuck: it needs three steps through the `if`. */
  let of_z = {|theorem of_z = of_int(0) == Z proof eval of_int(0) at 0 end; eval $e at 0 end; eval $e at 0 end; axiom refl_eq at 0 on $e == $e end in of_z|};
  let (pm, proof) = run_named("of_z", prelude_c ++ of_z);
  check_proven("of_int(0) == Z closes in three eval steps", pm, proof);
};

/* The Algebrite `rewrite` step locates its target up to the environment,
 * like axiom steps, so it finds `1 + to_int(m) - 1` after an earlier axiom
 * step has left the environment-substituted `to_int` in the goal. */
let test_rewrite_matches_through_the_environment = () => {
  let src =
    prelude_c
    ++ lemmas_c
    ++ {|theorem r = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => ? | S(m) => axiom to_s at 0 on to_int(S(m)) end; axiom of_s at 0 on of_int(1 + to_int(m)) end; rewrite 1 + to_int(m) - 1 with to_int(m) at 0 end end in r|};
  let (pm, proof) = run_named("r", src);
  Alcotest.check(
    Alcotest.bool,
    "the written `1 + to_int(m) - 1` is found by the rewrite step",
    false,
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
 * `1 + to_int(m) > 0`; discharging it needs `to_int(m) >= 0`, another
 * induction over Nt, not supplied here. */
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
      ++ dump(pm, proof),
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
      "A: the same theorem closes with the IH cited forward",
      `Quick,
      test_forward_ih_accumulator_proves,
    ),
    test_case(
      "A: fold_cons closes by eval + refl",
      `Quick,
      test_fold_cons_proves_by_eval,
    ),
    test_case(
      "A: control -- the naive unaccumulated statement cannot close",
      `Quick,
      test_naive_accumulator_is_incomplete,
    ),
    test_case(
      "A: refl_eq proves an equation between fold_left expressions",
      `Quick,
      test_refl_eq_proves_on_fold_left_expression,
    ),
    test_case(
      "A: a curried accumulator's cons equation closes, but only at an exact step count",
      `Quick,
      test_curried_accumulator_equation_is_step_count_brittle,
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
      "B: append equations prove by eval + refl (no holes)",
      `Quick,
      test_append_equations_prove_by_eval,
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
      "C: rewrite matches through the environment",
      `Quick,
      test_rewrite_matches_through_the_environment,
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
