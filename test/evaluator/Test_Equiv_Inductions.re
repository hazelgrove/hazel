open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Reconnaissance: HARD equivalence examples (accumulator len/fold,
 * reverse snoc+involution, Nat of_int/to_int round-trip). Status
 * expectations are pinned to OBSERVED checker results; dumps stay in
 * failure messages. Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

let print_exp = Test_ProofMap.print_exp;

let trunc = (s: string): string => {
  let s = String.map(c => c == '\n' ? ' ' : c, s);
  String.length(s) > 2000 ? String.sub(s, 0, 2000) ++ " ..." : s;
};

let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => trunc(print_exp(e))
  | None => "<none>"
  };

let rec dump_proof = (pm: ProofMap.t, p: Proof.t, indent: string): string => {
  let id = Proof.rep_id(p);
  let entry = ProofMap.lookup(id, pm);
  let (inc, out, marks, obs) =
    switch (entry) {
    | Some({incoming, outgoing, marks, obligations, _}) => (
        opt_exp_str(incoming),
        opt_exp_str(outgoing),
        marks
        |> List.map(m => trunc(ProofMark.show(m)))
        |> String.concat("; "),
        obligations
        |> List.map((ob: Obligation.t) =>
             trunc(print_exp(ob.goal))
             ++ " ["
             ++ Obligation.show_discharge(ob.discharge)
             ++ "]"
           )
        |> String.concat("; "),
      )
    | None => ("<no entry>", "", "", "")
    };
  let kind =
    switch (p.term) {
    | EmptyHole => "hole"
    | Invalid(_) => "invalid"
    | MultiHole(_) => "multihole"
    | Seq(_) => "seq"
    | Forall(_) => "forall"
    | Assume(_) => "assume"
    | Alias(_) => "alias"
    | Generalize(_) => "generalize"
    | Revert(_) => "revert"
    | Contradiction(_, _) => "contradiction"
    | Induction(_) => "induction"
    | AxiomStep(_) => "axiom"
    | AlgebriteStep(_) => "rewrite"
    | EvalStep(_) => "eval"
    | Have(_, _, _) => "have"
    };
  let here =
    indent
    ++ kind
    ++ "\n"
    ++ indent
    ++ "  in:  "
    ++ inc
    ++ "\n"
    ++ indent
    ++ "  out: "
    ++ out
    ++ (marks == "" ? "" : "\n" ++ indent ++ "  marks: " ++ marks)
    ++ (obs == "" ? "" : "\n" ++ indent ++ "  obs: " ++ obs);
  let children =
    switch (p.term) {
    | Seq(a, b) => [dump_proof(pm, a, indent), dump_proof(pm, b, indent)]
    | Forall(_, b)
    | Assume(_, _, b)
    | Alias(_, _, b)
    | Generalize(_, b)
    | Revert(_, _, b) => [dump_proof(pm, b, indent ++ "  ")]
    | Have(_, sub, b) => [
        dump_proof(pm, sub, indent ++ "  "),
        dump_proof(pm, b, indent ++ "  "),
      ]
    | Induction(_, _, cases) =>
      List.map(
        ((pat, body)) =>
          indent
          ++ "| "
          ++ trunc(Pat.show(pat))
          ++ "\n"
          ++ dump_proof(pm, body, indent ++ "  "),
        cases,
      )
    | _ => []
    };
  String.concat("\n", [here, ...children]);
};

let dump = (pm, proof): string => dump_proof(pm, proof, "");

let check_full_status = (msg, expected: ProofMap.full_status, pm, proof): unit => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  if (actual != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(actual)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof),
    );
  };
};

let check_has_mark = (msg, pred, pm, proof) =>
  if (!has_mark_kind(pm, proof, pred)) {
    Alcotest.fail(msg ++ "\n--- proof dump ---\n" ++ dump(pm, proof));
  };

/* ===================================================================== */
/* A. Rec vs fold — accumulator theorem                                  */
/* ===================================================================== */

let len_add1_prelude = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in let add1 = fun (acc, _) -> acc + 1 in |};

let len_nil_real = {|theorem len_nil = len([]) == 0 proof eval len([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in |};

let len_cons_real = {|theorem len_cons = forall h -> forall t -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in |};

let fold_nil_real = {|theorem fold_nil = forall f -> forall acc -> fold_left([], f, acc) == acc proof eval fold_left([], f, acc) at 0 end; axiom refl_eq at 0 on acc == acc end in |};

let fold_cons_real = {|theorem fold_cons = forall x -> forall xs -> forall f -> forall acc -> fold_left(x :: xs, f, acc) == fold_left(xs, f, f(acc, x)) proof eval fold_left(x :: xs, f, acc) at 0 end; axiom refl_eq at 0 on fold_left(xs, f, f(acc, x)) == fold_left(xs, f, f(acc, x)) end in |};

let add1_step_real = {|theorem add1_step = forall acc -> forall x -> add1((acc, x)) == acc + 1 proof eval add1((acc, x)) at 0 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in |};

let len_nil_hole = {|theorem len_nil = len([]) == 0 proof ? in |};
let len_cons_hole = {|theorem len_cons = forall h -> forall t -> len(h :: t) == 1 + len(t) proof ? in |};
let fold_nil_hole = {|theorem fold_nil = forall f -> forall acc -> fold_left([], f, acc) == acc proof ? in |};
let fold_cons_hole = {|theorem fold_cons = forall x -> forall xs -> forall f -> forall acc -> fold_left(x :: xs, f, acc) == fold_left(xs, f, f(acc, x)) proof ? in |};
let add1_step_hole = {|theorem add1_step = forall acc -> forall x -> add1((acc, x)) == acc + 1 proof ? in |};
let shuffle_hole = {|theorem shuffle = forall a -> forall b -> a + (1 + b) == (a + 1) + b proof ? in |};

let wrap = (lemmas, body) => len_add1_prelude ++ lemmas ++ body;

let test_len_nil_real = () => {
  let src = wrap(len_nil_real, "len_nil");
  let (pm, proof) = run_named("len_nil", src);
  check_full_status("len_nil by eval+refl", ProofMap.Proven, pm, proof);
};

let test_len_cons_real = () => {
  let src = wrap(len_cons_real, "len_cons");
  let (pm, proof) = run_named("len_cons", src);
  check_full_status("len_cons by eval+refl", ProofMap.Proven, pm, proof);
};

let test_fold_nil_real = () => {
  let src = wrap(fold_nil_real, "fold_nil");
  let (pm, proof) = run_named("fold_nil", src);
  check_full_status("fold_nil by eval+refl", ProofMap.Proven, pm, proof);
};

let test_fold_cons_real = () => {
  let src = wrap(fold_cons_real, "fold_cons");
  let (pm, proof) = run_named("fold_cons", src);
  /* Observed: one eval unwraps FixF to a residual lambda application. */
  check_full_status("fold_cons by eval+refl", ProofMap.Incomplete, pm, proof);
};

let test_add1_step_real = () => {
  let src = wrap(add1_step_real, "add1_step");
  let (pm, proof) = run_named("add1_step", src);
  check_full_status("add1_step by eval+refl", ProofMap.Proven, pm, proof);
};

let naive_lemmas =
  len_nil_hole ++ len_cons_hole ++ fold_nil_hole ++ fold_cons_hole ++ add1_step_hole;

let naive_src =
  wrap(
    naive_lemmas,
    {|theorem u = forall xs: [Int] -> len(xs) == fold_left(xs, add1, 0) proof induction xs | [] => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, 0) end; axiom refl_eq at 0 on 0 == 0 end | x :: xs' => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, 0) end; axiom add1_step at 0 on add1((0, x)) end; axiom ih at 0 on 1 + len(xs') == fold_left(xs', add1, 1) end end in u|},
  );

let test_naive_len_fold_too_weak = () => {
  let (pm, proof) = run_named("u", naive_src);
  check_has_mark(
    "naive cons-case IH cite misses (unfolded len; acc 0 vs 1)",
    fun
    | ProofMark.PatternNotFound(_) => true
    | ProofMark.RuleDoesNotApply(_) => true
    | _ => false,
    pm,
    proof,
  );
  check_full_status(
    "naive len==fold induction is Incomplete",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* Forward IH rewrites the integer side onto two alpha-different FixF
 * unfoldings of fold_left; refl_eq then dies with
 * PossiblyDivergentInstantiation (instance contains add1). Reverse the
 * IH so both sides become the integer expression, then refl. */
let acc_thm = {|theorem len_fold = forall xs: [Int] -> forall acc -> acc + len(xs) == fold_left(xs, add1, acc) proof generalize acc => induction xs | [] => forall acc => axiom len_nil at 0 on len([]) end; axiom fold_nil at 0 on fold_left([], add1, acc) end; rewrite acc + 0 with acc at 0 end; axiom refl_eq at 0 on acc == acc end | x :: xs' => forall acc => axiom len_cons at 0 on len(x :: xs') end; axiom fold_cons at 0 on fold_left(x :: xs', add1, acc) end; axiom add1_step at 0 on add1((acc, x)) end; axiom shuffle at 0 on acc + (1 + len(xs')) end; axiomrev ih at 0 on fold_left(xs', add1, acc + 1) end; axiom refl_eq at 0 on (acc + 1) + len(xs') == (acc + 1) + len(xs') end end in |};
let acc_proof = acc_thm ++ {|len_fold|};

let acc_lemmas_holes =
  len_nil_hole
  ++ len_cons_hole
  ++ fold_nil_hole
  ++ fold_cons_hole
  ++ add1_step_hole
  ++ shuffle_hole;

let acc_src_holes = wrap(acc_lemmas_holes, acc_proof);

let test_len_fold_generalized_holes = () => {
  let (pm, proof) = run_named("len_fold", acc_src_holes);
  check_full_status(
    "generalized acc+len==fold (hole lemmas, axiomrev IH)",
    ProofMap.Proven,
    pm,
    proof,
  );
};

let acc_lemmas_real =
  len_nil_real
  ++ len_cons_real
  ++ fold_nil_real
  ++ fold_cons_real
  ++ add1_step_real
  ++ shuffle_hole;

let acc_src_real = wrap(acc_lemmas_real, acc_proof);

let test_len_fold_generalized_real = () => {
  let (pm, proof) = run_named("len_fold", acc_src_real);
  check_full_status(
    "generalized acc+len==fold (real eval lemmas + hole shuffle)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let cor_src =
  wrap(
    acc_lemmas_holes ++ acc_thm,
    {|theorem cor = forall xs: [Int] -> len(xs) == fold_left(xs, add1, 0) proof axiom len_fold with acc = 0 at 0 on 0 + len(xs) end; rewrite 0 + len(xs) with len(xs) at 0 end; axiom refl_eq at 0 on fold_left(xs, add1, 0) == fold_left(xs, add1, 0) end in cor|},
  );

let test_len_fold_corollary = () => {
  let (pm, proof) = run_named("cor", cor_src);
  check_full_status(
    "corollary len==fold at acc=0",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* ===================================================================== */
/* B. Reverse involution                                                 */
/* ===================================================================== */

let rev_prelude = {|let rev = fun xs -> case xs | [] => [] | x :: xs => rev(xs) @ [x] end in |};

let rev_nil_real = {|theorem rev_nil = rev([]) == [] proof eval rev([]) at 0 end; axiom refl_eq at 0 on [] == [] end in |};
let rev_cons_real = {|theorem rev_cons = forall x -> forall xs -> rev(x :: xs) == rev(xs) @ [x] proof eval rev(x :: xs) at 0 end; axiom refl_eq at 0 on rev(xs) @ [x] == rev(xs) @ [x] end in |};
let app_nil_real = {|theorem app_nil = forall ys -> [] @ ys == ys proof eval [] @ ys at 0 end; axiom refl_eq at 0 on ys == ys end in |};
let app_cons_real = {|theorem app_cons = forall h -> forall t -> forall ys -> (h :: t) @ ys == h :: (t @ ys) proof eval (h :: t) @ ys at 0 end; axiom refl_eq at 0 on h :: (t @ ys) == h :: (t @ ys) end in |};

let rev_nil_hole = {|theorem rev_nil = rev([]) == [] proof ? in |};
let rev_cons_hole = {|theorem rev_cons = forall x -> forall xs -> rev(x :: xs) == rev(xs) @ [x] proof ? in |};
let app_nil_hole = {|theorem app_nil = forall ys -> [] @ ys == ys proof ? in |};
let app_cons_hole = {|theorem app_cons = forall h -> forall t -> forall ys -> (h :: t) @ ys == h :: (t @ ys) proof ? in |};

let test_rev_nil_real = () => {
  let src = rev_prelude ++ rev_nil_real ++ "rev_nil";
  let (pm, proof) = run_named("rev_nil", src);
  check_full_status("rev_nil by eval+refl", ProofMap.Proven, pm, proof);
};

let test_rev_cons_real = () => {
  let src = rev_prelude ++ rev_cons_real ++ "rev_cons";
  let (pm, proof) = run_named("rev_cons", src);
  check_full_status("rev_cons by eval+refl", ProofMap.Proven, pm, proof);
};

let snoc_thm = {|theorem rev_snoc = forall xs: [Int] -> forall x -> rev(xs @ [x]) == x :: rev(xs) proof generalize x => induction xs | [] => forall x => axiom app_nil at 0 on [] @ [x] end; axiom rev_nil at 0 on rev([]) end; eval rev([x]) at 0 end; axiom refl_eq at 0 on [x] == [x] end | h :: t => forall x => axiom app_cons at 0 on (h :: t) @ [x] end; axiom rev_cons at 0 on rev(h :: (t @ [x])) end; axiom ih at 0 on rev(t @ [x]) end; axiom rev_cons at 0 on rev(h :: t) end; axiom refl_eq at 0 on x :: (rev(t) @ [h]) == x :: (rev(t) @ [h]) end end in |};
let snoc_proof = snoc_thm ++ {|rev_snoc|};

let snoc_lemmas_holes = rev_nil_hole ++ rev_cons_hole ++ app_nil_hole ++ app_cons_hole;

let test_rev_snoc = () => {
  let src = rev_prelude ++ snoc_lemmas_holes ++ snoc_proof;
  let (pm, proof) = run_named("rev_snoc", src);
  check_full_status(
    "rev_snoc helper (itself an induction, hole lemmas)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let involution_proof = {|theorem rev_rev = forall xs: [Int] -> rev(rev(xs)) == xs proof induction xs | [] => axiom rev_nil at 0 on rev([]) end; axiom rev_nil at 0 on rev([]) end; axiom refl_eq at 0 on [] == [] end | x :: xs' => axiom rev_cons at 0 on rev(x :: xs') end; axiom ih at 0 on rev(rev(xs')) end; axiom rev_snoc at 0 on rev(rev(xs') @ [x]) end; axiom refl_eq at 0 on x :: xs' == x :: xs' end end in rev_rev|};

let test_rev_involution = () => {
  let src = rev_prelude ++ snoc_lemmas_holes ++ snoc_thm ++ involution_proof;
  let (pm, proof) = run_named("rev_rev", src);
  check_full_status(
    "rev involution using snoc helper",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let ra_prelude = {|let ra = fun xs -> fun ys -> case xs | [] => ys | h :: t => ra(t)(h :: ys) end in |};
let ra_nil_hole = {|theorem ra_nil = forall zs -> ra([])(zs) == zs proof ? in |};
let ra_cons_hole = {|theorem ra_cons = forall h -> forall t -> forall zs -> ra(h :: t)(zs) == ra(t)(h :: zs) proof ? in |};

let ra_gen_proof = {|theorem ra_inv = forall xs: [Int] -> forall ys -> ra(ra(xs)(ys))([]) == ra(ys)(xs) proof generalize ys => induction xs | [] => forall ys => axiom ra_nil at 0 on ra([])(ys) end; axiom refl_eq at 0 on ra(ys)([]) == ra(ys)([]) end | h :: t => forall ys => axiom ra_cons at 0 on ra(h :: t)(ys) end; axiom ih at 0 on ra(ra(t)(h :: ys))([]) end; axiom ra_cons at 0 on ra(h :: ys)(t) end; axiom refl_eq at 0 on ra(ys)(h :: t) == ra(ys)(h :: t) end end in ra_inv|};

let test_ra_involution_gen = () => {
  let src = ra_prelude ++ ra_nil_hole ++ ra_cons_hole ++ ra_gen_proof;
  let (pm, proof) = run_named("ra_inv", src);
  check_full_status(
    "ra involution via generalized helper (hole lemmas)",
    ProofMap.Proven,
    pm,
    proof,
  );
};

let ra_direct_proof = {|theorem ra_inv0 = forall xs: [Int] -> ra(ra(xs)([]))([]) == xs proof induction xs | [] => axiom ra_nil at 0 on ra([])([]) end; axiom ra_nil at 0 on ra([])([]) end; axiom refl_eq at 0 on [] == [] end | h :: t => axiom ra_cons at 0 on ra(h :: t)([]) end; axiom ih at 0 on ra(ra(t)([]))([]) end; axiom ra_cons at 0 on ra([])(h :: t) end end in ra_inv0|};

let test_ra_involution_direct = () => {
  let src = ra_prelude ++ ra_nil_hole ++ ra_cons_hole ++ ra_direct_proof;
  let (pm, proof) = run_named("ra_inv0", src);
  check_full_status(
    "direct ra(ra(xs)([]))([]) induction (no generalize)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* ===================================================================== */
/* C. Nat round-trip                                                     */
/* ===================================================================== */

let nat_prelude = {|type Nat = rec n -> +Z + S(n) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun k -> if k <= 0 then Z else S(of_int(k - 1)) end in |};

let test_to_int_z = () => {
  let src =
    nat_prelude
    ++ {|theorem t = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};
  let (pm, proof) = run_named("t", src);
  check_full_status("to_int(Z) by eval+refl", ProofMap.Proven, pm, proof);
};

let test_to_int_s = () => {
  let src =
    nat_prelude
    ++ {|theorem t = forall m -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in t|};
  let (pm, proof) = run_named("t", src);
  check_full_status("to_int(S(m)) by eval+refl", ProofMap.Proven, pm, proof);
};

let test_of_int_0_eval = () => {
  let src =
    nat_prelude
    ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};
  let (pm, proof) = run_named("t", src);
  /* Observed: one eval yields `if 0 <= 0 then Z else S(FixF...)`, not Z. */
  check_full_status("of_int(0) by one eval+refl", ProofMap.Incomplete, pm, proof);
};

let test_of_int_0_if_chain = () => {
  let src =
    nat_prelude
    ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; eval 0 <= 0 at 0 end; eval (if true then Z else S(of_int(0 - 1))) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};
  let (pm, proof) = run_named("t", src);
  check_full_status(
    "of_int(0) by eval of_int then 0<=0 then if-true",
    ProofMap.Proven,
    pm,
    proof,
  );
};

let test_of_int_0_assume = () => {
  let src =
    nat_prelude
    ++ {|theorem t = of_int(0) == Z proof assume of_int(0) == Z => axiom assume at 0 on of_int(0) end; axiom refl_eq at 0 on Z == Z end in t|};
  let (pm, proof) = run_named("t", src);
  /* Channel-2 Evaluated does not fire: obligation stays Pending. */
  check_full_status(
    "of_int(0) by assume+Evaluated",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let test_of_int_2_assume = () => {
  let src =
    nat_prelude
    ++ {|theorem t = of_int(2) == S(S(Z)) proof assume of_int(2) == S(S(Z)) => axiom assume at 0 on of_int(2) end; axiom refl_eq at 0 on S(S(Z)) == S(S(Z)) end in t|};
  let (pm, proof) = run_named("t", src);
  check_full_status(
    "of_int(2) by assume+Evaluated (pins stuckness if Incomplete/Pending)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let nat_prelude_eq = {|type Nat = rec n -> +Z + S(n) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun k -> if k == 0 then Z else S(of_int(k - 1)) end in |};

let test_of_int_2_assume_eq = () => {
  let src =
    nat_prelude_eq
    ++ {|theorem t = of_int(2) == S(S(Z)) proof assume of_int(2) == S(S(Z)) => axiom assume at 0 on of_int(2) end; axiom refl_eq at 0 on S(S(Z)) == S(S(Z)) end in t|};
  let (pm, proof) = run_named("t", src);
  check_full_status(
    "of_int(2) with ==0 spelling, assume+Evaluated",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let of_z_hole = {|theorem of_z = of_int(0) == Z proof ? in |};
let of_succ_hole = {|theorem of_succ = forall k -> of_int(1 + k) == S(of_int(k)) proof ? in |};
let to_z_hole = {|theorem to_z = to_int(Z) == 0 proof ? in |};
let to_s_hole = {|theorem to_s = forall m -> to_int(S(m)) == 1 + to_int(m) proof ? in |};

let roundtrip_proof = {|theorem rt = forall n: Nat -> of_int(to_int(n)) == n proof induction n | Z => axiom to_z at 0 on to_int(Z) end; axiom of_z at 0 on of_int(0) end; axiom refl_eq at 0 on Z == Z end | S(m) => axiom to_s at 0 on to_int(S(m)) end; axiom of_succ at 0 on of_int(1 + to_int(m)) end; axiom ih at 0 on of_int(to_int(m)) end; axiom refl_eq at 0 on S(m) == S(m) end end in rt|};

let test_nat_roundtrip_holes = () => {
  let src =
    nat_prelude ++ of_z_hole ++ of_succ_hole ++ to_z_hole ++ to_s_hole ++ roundtrip_proof;
  let (pm, proof) = run_named("rt", src);
  check_full_status(
    "of_int(to_int(n))==n with hole lemmas (Nat induction glue)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let tests = (
  "Equivalence inductions",
  [
    test_case("A len_nil eval+refl", `Quick, test_len_nil_real),
    test_case("A len_cons eval+refl", `Quick, test_len_cons_real),
    test_case("A fold_nil eval+refl", `Quick, test_fold_nil_real),
    test_case("A fold_cons eval+refl", `Quick, test_fold_cons_real),
    test_case("A add1_step eval+refl", `Quick, test_add1_step_real),
    test_case("A naive len/fold IH too weak", `Quick, test_naive_len_fold_too_weak),
    test_case(
      "A generalized len_fold hole lemmas",
      `Quick,
      test_len_fold_generalized_holes,
    ),
    test_case(
      "A generalized len_fold real lemmas",
      `Quick,
      test_len_fold_generalized_real,
    ),
    test_case("A corollary acc=0", `Quick, test_len_fold_corollary),
    test_case("B rev_nil eval+refl", `Quick, test_rev_nil_real),
    test_case("B rev_cons eval+refl", `Quick, test_rev_cons_real),
    test_case("B rev_snoc helper induction", `Quick, test_rev_snoc),
    test_case("B rev involution via snoc", `Quick, test_rev_involution),
    test_case("B ra generalized involution", `Quick, test_ra_involution_gen),
    test_case("B ra direct involution too weak", `Quick, test_ra_involution_direct),
    test_case("C to_int(Z)", `Quick, test_to_int_z),
    test_case("C to_int(S(m))", `Quick, test_to_int_s),
    test_case("C of_int(0) one eval", `Quick, test_of_int_0_eval),
    test_case("C of_int(0) if-chain evals", `Quick, test_of_int_0_if_chain),
    test_case("C of_int(0) assume", `Quick, test_of_int_0_assume),
    test_case("C of_int(2) assume", `Quick, test_of_int_2_assume),
    test_case("C of_int(2) ==0 assume", `Quick, test_of_int_2_assume_eq),
    test_case("C Nat round-trip hole lemmas", `Quick, test_nat_roundtrip_holes),
  ],
);
