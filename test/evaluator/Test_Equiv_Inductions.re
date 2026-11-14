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
    | ProofMark.NothingToStep => true
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

/* Defining equations that close by one `step` + `refl`: `len` is
 * an ordinary structural recursion, and `fold_left`/`add1` at the empty
 * list reduce to the accumulator. */
let len_nil = {|theorem len_nil = len([]) == 0 proof from len([]) do step end; from 0 == 0 do rewrite refl end end in |};
let len_cons = {|theorem len_cons = forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t) proof from len(h :: t) do step end; from 1 + len(t) == 1 + len(t) do rewrite refl end end in |};
let fold_nil = {|theorem fold_nil = forall a: Int -> fold_left([], add1, a) == a proof from fold_left([], add1, a) do step end; from a == a do rewrite refl end end in |};
let add1_step = {|theorem add1_step = forall a: Int -> forall b: Int -> add1((a, b)) == a + 1 proof from add1((a, b)) do step end; from a + 1 == a + 1 do rewrite refl end end in |};

/* `shuffle` is pure arithmetic and closes by the Algebrite rewrite. */
let shuffle = {|theorem shuffle = forall a: Int -> forall b: Int -> a + (1 + b) == (a + 1) + b proof from a + (1 + b) do algebra (a + 1) + b end end; from (a + 1) + b == (a + 1) + b do rewrite refl end end in |};

/* `fold_cons` closes because:
 *   (1) the closing `refl` lands on an expression containing
 *       `fold_left`, which applies its quantified function parameter,
 *       and a quantified `f` is a total map, so the instantiation gate
 *       lets it through;
 *   (2) enough `step` steps make both sides converge to the same `case`
 *       on `t`, which `refl` matches up to alpha-renaming.
 * The left side takes three steps (step the cons, which leaves
 * `fold_left(t, add1, add1(a, h))`, apply `add1`, unfold on `t`) and the
 * right side one. See test_fold_cons_proves_by_eval. */
let fold_cons = {|theorem fold_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) proof from fold_left(h :: t, add1, a) do step; from $e(a, h) do step end; step end; from fold_left(t, add1, a + 1) do step end; rewrite refl end in |};

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
 * in the reverse direction (`rewrite <-`), rewriting the fold side back
 * into `(acc + 1) + len(xs')`, so the final `refl` lands on a
 * `len`-only expression. thm_a_fwd below closes the other way round. */
let thm_a = {|theorem lenfold = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => from len([]) do rewrite len_nil end end; from fold_left([], add1, acc) do rewrite fold_nil end end; from acc + 0 do algebra acc end end; from acc == acc do rewrite refl end end | x :: xs' => forall acc => from len(x :: xs') do rewrite len_cons end end; from fold_left(x :: xs', add1, acc) do rewrite fold_cons end end; from acc + (1 + len(xs')) do rewrite shuffle end end; from fold_left(xs', add1, acc + 1) do rewrite <- ih_xs' end end; from $e == $e do rewrite refl end end end in |};

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
 * the closing move is a plain `rewrite refl`. That passes the
 * instantiation gate although `fold_left` applies its abstract function
 * parameter, because a quantified `f` is a total map. */
let thm_a_fwd = {|theorem lenfoldf = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => from len([]) do rewrite len_nil end end; from fold_left([], add1, acc) do rewrite fold_nil end end; from acc + 0 do algebra acc end end; from acc == acc do rewrite refl end end | x :: xs' => forall acc => from len(x :: xs') do rewrite len_cons end end; from fold_left(x :: xs', add1, acc) do rewrite fold_cons end end; from acc + (1 + len(xs')) do rewrite shuffle end end; from (acc + 1) + len(xs') do rewrite ih_xs' end end; from $e == $e do rewrite refl end end end in |};

let test_forward_ih_accumulator_proves = () => {
  let (pm, proof) =
    run_named("lenfoldf", prelude_a ++ lemmas_a ++ thm_a_fwd ++ "lenfoldf");
  check_mark_free("the forward-IH len/fold proof is mark-free", pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "closing refl on a residual fold_left == fold_left is not refused",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_obligation_count("no obligations", 0, pm, proof);
  check_proven(
    "forward-IH version closes on refl at fold_left == fold_left",
    pm,
    proof,
  );
};

/* `fold_cons` proves by eval alone. One step short, the sides don't
 * match yet. */
let test_fold_cons_proves_by_eval = () => {
  let fc = proof =>
    prelude_a
    ++ {|theorem fc = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) proof |}
    ++ proof
    ++ {| in fc|};
  let (pm, proof) =
    run_named(
      "fc",
      fc(
        {|from fold_left(h :: t, add1, a) do step; from $e(a, h) do step end; step end; from fold_left(t, add1, a + 1) do step end; rewrite refl end|},
      ),
    );
  Alcotest.check(
    Alcotest.bool,
    "eval DOES step fold_left on a cons -- this was never a stuck-step pin",
    false,
    has_nothing_to_step(pm, proof),
  );
  Alcotest.check(
    Alcotest.bool,
    "and the closing refl is not refused as divergent",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_mark_free("fold_cons by eval is mark-free", pm, proof);
  check_proven(
    "fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1) by eval + refl",
    pm,
    proof,
  );
  let (pm, proof) =
    run_named(
      "fc",
      fc(
        {|from fold_left(h :: t, add1, a) do step; from $e(a, h) do step end end; from fold_left(t, add1, a + 1) do step end; rewrite refl end|},
      ),
    );
  check_incomplete(
    "fold_cons does not close with the left side one step short",
    pm,
    proof,
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
    ++ {|theorem naive = forall xs: [Int] -> len(xs) == fold_left(xs, add1, 0) proof induction xs | [] => from len([]) do rewrite len_nil end end; from fold_left([], add1, 0) do rewrite fold_nil end end; from 0 == 0 do rewrite refl end end | x :: xs' => from len(x :: xs') do rewrite len_cons end end; from fold_left(x :: xs', add1, 0) do rewrite fold_cons end end; from fold_left(xs', add1, 0) do rewrite <- ih_xs' end end; from $e == $e do rewrite refl end end end in naive|};
  let (pm, proof) = run_named("naive", src);
  Alcotest.check(
    Alcotest.bool,
    "the IH at accumulator 0 does not occur in the residual goal",
    true,
    has_pattern_not_found(pm, proof),
  );
  check_incomplete("the naive induction does not close", pm, proof);
};

/* `refl` is `forall x -> (x == x) == true`, and the instantiation
 * gate refuses any instantiation Totality.check can't show total. The
 * `fold_left` fix applies its function parameter `f`, an abstract
 * variable, but a quantified function parameter is a total map, so
 * `fold_left(t, add1, 0) == fold_left(t, add1, 0)` is Proven, like the
 * `len` control. (A divergent instantiation such as `omega == omega` is
 * still refused; see Test_FunctionTotality.) */
let test_refl_eq_proves_on_fold_left_expression = () => {
  let src =
    prelude_a
    ++ {|theorem w = forall t: [Int] -> fold_left(t, add1, 0) == fold_left(t, add1, 0) proof from fold_left(t, add1, 0) == fold_left(t, add1, 0) do rewrite refl end end in w|};
  let (pm, proof) = run_named("w", src);
  Alcotest.check(
    Alcotest.bool,
    "refl at a fold_left expression is not refused as divergent",
    false,
    has_possibly_divergent(pm, proof),
  );
  check_mark_free("and it leaves no marks", pm, proof);
  check_proven("so X == X over fold_left closes", pm, proof);
  /* Control: the same shape over `len` is Proven. */
  let control =
    prelude_a
    ++ {|theorem w2 = forall t: [Int] -> len(t) == len(t) proof from len(t) == len(t) do rewrite refl end end in w2|};
  let (pm', proof') = run_named("w2", control);
  check_proven("control: refl over `len` closes", pm', proof');
};

/* Currying is not a barrier, but the left side's first redex is not its
 * root: `fadd(h :: t)(a)` applies `fadd(h :: t)`, which has to step
 * first. Focused there, one step applies the outer argument; then the
 * left side's root steps to the right side's shape and `refl`
 * closes. */
let test_curried_accumulator_equation_closes_by_focus = () => {
  let fadd_cons = proof =>
    {|let fadd = fun xs -> fun acc -> case xs | [] => acc | _ :: t => fadd(t)(acc + 1) end in theorem fadd_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fadd(h :: t)(a) == fadd(t)(a + 1) proof |}
    ++ proof
    ++ {| in fadd_cons|};
  let (pm, proof) =
    run_named(
      "fadd_cons",
      fadd_cons(
        {|from fadd(h :: t)(a) do from fadd(h :: t) do step end; step end; rewrite refl end|},
      ),
    );
  check_mark_free("the curried cons equation is mark-free", pm, proof);
  check_proven("the curried accumulator's cons equation closes", pm, proof);
  let (pm, proof) =
    run_named(
      "fadd_cons",
      fadd_cons({|from fadd(h :: t)(a) do step end; rewrite refl end|}),
    );
  Alcotest.check(
    Alcotest.bool,
    "the root of `fadd(h :: t)(a)` is not a redex",
    true,
    has_nothing_to_step(pm, proof),
  );
};

/* Uncurrying the accumulator avoids the counting: the tupled
 * `fa((xs, acc))` reduces beta + case in a single `step` step, like
 * `len`. */
let prelude_fu = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in let fa = fun (xs, acc) -> case xs | [] => acc | _ :: t => fa((t, acc + 1)) end in |};
let lemmas_fu =
  len_nil
  ++ len_cons
  ++ {|theorem fa_nil = forall a: Int -> fa(([], a)) == a proof from fa(([], a)) do step end; from a == a do rewrite refl end end in theorem fa_cons = forall h: Int -> forall t: [Int] -> forall a: Int -> fa((h :: t, a)) == fa((t, a + 1)) proof from fa((h :: t, a)) do step end; from $e == $e do rewrite refl end end in |}
  ++ shuffle;
let thm_fu = {|theorem lenfa = forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fa((xs, acc)) proof generalize acc => induction xs | [] => forall acc => from len([]) do rewrite len_nil end end; from fa(([], acc)) do rewrite fa_nil end end; from acc + 0 do algebra acc end end; from acc == acc do rewrite refl end end | x :: xs' => forall acc => from len(x :: xs') do rewrite len_cons end end; from fa((x :: xs', acc)) do rewrite fa_cons end end; from acc + (1 + len(xs')) do rewrite shuffle end end; from fa((xs', acc + 1)) do rewrite <- ih_xs' end end; from $e == $e do rewrite refl end end end in |};

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

let rev_nil = {|theorem rev_nil = rev([]) == [] proof from rev([]) do step end; from ([]: [Int]) == ([]: [Int]) do rewrite refl end end in |};
let rev_cons = {|theorem rev_cons = forall x: Int -> forall t: [Int] -> rev(x :: t) == rev(t) @ [x] proof from rev(x :: t) do step end; from rev(t) @ [x] == rev(t) @ [x] do rewrite refl end end in |};

/* The append equations. `@` steps on constructor form, so `[] @ m` and
 * `(y :: l) @ m` each take one `step` step with `l`/`m` still
 * quantified, and `refl` closes (test_append_equations_prove_by_eval). */
let app_lemmas = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof from [] @ m do step end; from m == m do rewrite refl end end in theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof from (y :: l) @ m do step end; from y :: (l @ m) == y :: (l @ m) do rewrite refl end end in |};

let lemmas_b = rev_nil ++ rev_cons ++ app_lemmas;

/* The helper that reverse involution needs, itself an induction over xs
 * (with the outer `x` held fixed, so no `generalize` is needed). The nil
 * case is pure computation: a focus on `rev([] @ [x])` evaluates it down
 * to `[x]`, which `x :: []` also steps to. The cons case is the
 * interesting one and closes with app_cons / rev_cons / IH / app_cons /
 * rev_cons. */
let snoc = {|theorem snoc = forall xs: [Int] -> forall x: Int -> rev(xs @ [x]) == x :: rev(xs) proof induction xs | [] => from rev([]) do rewrite rev_nil end end; from rev([] @ [x]) do from [] @ [x] do step end; step; from $e([]) do step end; step end; from x :: [] do step end; rewrite refl end | y :: t => from (y :: t) @ [x] do rewrite app_cons end end; from rev(y :: (t @ [x])) do rewrite rev_cons end end; from rev(t @ [x]) do rewrite ih_t end end; from (x :: rev(t)) @ [y] do rewrite app_cons end end; from rev(y :: t) do rewrite rev_cons end end; from $e == $e do rewrite refl end end end in |};

let invol = {|theorem invol = forall xs: [Int] -> rev(rev(xs)) == xs proof induction xs | [] => from rev([]) do rewrite rev_nil end end; from rev([]) do rewrite rev_nil end end; from $e == $e do rewrite refl end end | x :: t => from rev(x :: t) do rewrite rev_cons end end; from rev(rev(t) @ [x]) do rewrite snoc end end; from rev(rev(t)) do rewrite ih_t end end; from $e == $e do rewrite refl end end end in |};

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
 * `@` steps on constructor form: each closes by one `step` + `refl`,
 * with no marks. */
let test_append_equations_prove_by_eval = () => {
  let nil_l = {|theorem app_nil_l = forall m: [Int] -> [] @ m == m proof from [] @ m do step end; from m == m do rewrite refl end end in app_nil_l|};
  let (pm, proof) = run_named("app_nil_l", nil_l);
  Alcotest.check(
    Alcotest.bool,
    "`[] @ m` is not stuck",
    false,
    has_nothing_to_step(pm, proof),
  );
  check_mark_free("the `[] @ m` proof is mark-free", pm, proof);
  check_proven("`[] @ m == m` is Proven for quantified m", pm, proof);
  let cons = {|theorem app_cons = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof from (y :: l) @ m do step end; from y :: (l @ m) == y :: (l @ m) do rewrite refl end end in app_cons|};
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
let lemmas_c = {|theorem to_z = to_int(Z) == 0 proof from to_int(Z) do step end; from 0 == 0 do rewrite refl end end in theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof from to_int(S(m)) do step end; from 1 + to_int(m) == 1 + to_int(m) do rewrite refl end end in theorem of_z = of_int(0) == Z proof from of_int(0) do step end; from $e do step end; from $e do step end; from $e == $e do rewrite refl end end in theorem of_s = forall k: Int -> k > 0 ==> of_int(k) == S(of_int(k - 1)) proof ? in theorem minus_one = forall j: Int -> 1 + j - 1 == j proof from 1 + j - 1 do algebra j end end; from j == j do rewrite refl end end in |};

let round = {|theorem round = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => from to_int(Z) do rewrite to_z end end; from of_int(0) do rewrite of_z end end; from $e == $e do rewrite refl end end | S(m) => from to_int(S(m)) do rewrite to_s end end; from of_int(1 + to_int(m)) do rewrite of_s end end; from 1 + to_int(m) - 1 do rewrite minus_one end end; from of_int(to_int(m)) do rewrite ih_m end end; from $e == $e do rewrite refl end end end in |};

let test_to_int_and_of_int_equations = () => {
  let to_z = {|theorem to_z = to_int(Z) == 0 proof from to_int(Z) do step end; from 0 == 0 do rewrite refl end end in to_z|};
  let (pm, proof) = run_named("to_z", prelude_c ++ to_z);
  check_proven("to_int(Z) == 0 closes by eval", pm, proof);
  let to_s = {|theorem to_s = forall m: Nt -> to_int(S(m)) == 1 + to_int(m) proof from to_int(S(m)) do step end; from 1 + to_int(m) == 1 + to_int(m) do rewrite refl end end in to_s|};
  let (pm, proof) = run_named("to_s", prelude_c ++ to_s);
  check_proven("to_int(S(m)) == 1 + to_int(m) closes by eval", pm, proof);
  /* of_int isn't stuck: it needs three steps through the `if`. */
  let of_z = {|theorem of_z = of_int(0) == Z proof from of_int(0) do step; from 0 <= 0 do step end; step end; rewrite refl end in of_z|};
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
    ++ {|theorem r = forall n: Nt -> of_int(to_int(n)) == n proof induction n | Z => ? | S(m) => from to_int(S(m)) do rewrite to_s end end; from of_int(1 + to_int(m)) do rewrite of_s end end; from 1 + to_int(m) - 1 do algebra to_int(m) end end end in r|};
  let (pm, proof) = run_named("r", src);
  Alcotest.check(
    Alcotest.bool,
    "the written `1 + to_int(m) - 1` is found by the rewrite step",
    false,
    has_pattern_not_found(pm, proof),
  );
};

/* PIN: shadowing the reserved `Nat` type name yields an induction with
 * no IHs (citing `ih_m` is UnknownEquality); the identical program under a
 * non-reserved name has the IH available (it doesn't rewrite the goal,
 * so the citation is refused for that, not for an unknown name). */
let test_nat_is_a_reserved_type_name = () => {
  let bad = {|type Nat = +Z + S(Nat) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in theorem t = forall n: Nat -> to_int(n) == 0 proof induction n | Z => ? | S(m) => rewrite ih_m end end in t|};
  let (pm, proof) = run_named("t", bad);
  Alcotest.check(
    Alcotest.bool,
    "induction over a `Nat`-named ADT generates no IH",
    true,
    has_unknown_equality(pm, proof),
  );
  let good = {|type Nt = +Z + S(Nt) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in theorem t = forall n: Nt -> to_int(n) == 0 proof induction n | Z => ? | S(m) => rewrite ih_m end end in t|};
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
      "A: refl proves an equation between fold_left expressions",
      `Quick,
      test_refl_eq_proves_on_fold_left_expression,
    ),
    test_case(
      "A: a curried accumulator's cons equation closes by focusing on its head",
      `Quick,
      test_curried_accumulator_equation_closes_by_focus,
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
