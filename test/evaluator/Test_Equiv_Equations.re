open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Reconnaissance: cheap equivalence examples (defining equations +
 * pointwise commute) against the current proof calculus. Status
 * expectations are pinned to observed full_status; dumps live in
 * failure messages and comments. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let proof_of_named = Test_ProofMap.proof_of_named;
let print_exp = Test_ProofMap.print_exp;

let run = (src: string): (ProofMap.t, Proof.t, Statics.Map.t, Exp.t) => {
  let (state, statics, elab) = src |> parse_exp |> eval_with_proof;
  (EvaluatorState.get_proof_map(state), proof_of(elab), statics, elab);
};

let run_named =
    (name: string, src: string): (ProofMap.t, Proof.t, Statics.Map.t, Exp.t) => {
  let (state, statics, elab) = src |> parse_exp |> eval_with_proof;
  (
    EvaluatorState.get_proof_map(state),
    proof_of_named(name, elab),
    statics,
    elab,
  );
};

let trunc = (s: string): string => {
  let s = String.map(c => c == '\n' ? ' ' : c, s);
  String.length(s) > 400 ? String.sub(s, 0, 400) ++ " ..." : s;
};

let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => trunc(print_exp(e))
  | None => "<none>"
  };

let rec dump = (pm: ProofMap.t, p: Proof.t, ind: string): string => {
  let (inc, out, marks, auto_in, auto_out) =
    switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
    | Some({incoming, outgoing, marks, auto_incoming, auto_outgoing, _}) => (
        opt_exp_str(incoming),
        opt_exp_str(outgoing),
        String.concat(
          "; ",
          List.map(
            m =>
              switch (m) {
              | ProofMark.PatternNotFound({at_exp, at_idx}) =>
                "PatternNotFound(at_idx="
                ++ string_of_int(at_idx)
                ++ ", at="
                ++ opt_exp_str(Some(at_exp))
                ++ ")"
              | ProofMark.RuleDoesNotApply({equality, direction}) =>
                "RuleDoesNotApply("
                ++ equality
                ++ ", "
                ++ Util.Direction.show(direction)
                ++ ")"
              | _ => ProofMark.show(m)
              },
            marks,
          ),
        ),
        auto_incoming
        |> List.map(((j, e)) => j ++ " => " ++ trunc(print_exp(e)))
        |> String.concat(" | "),
        auto_outgoing
        |> List.map(((e, j)) => trunc(print_exp(e)) ++ " => " ++ j)
        |> String.concat(" | "),
      )
    | None => ("<no entry>", "", "", "", "")
    };
  let kind = Proof.show_cls(Proof.cls_of_term(p.term));
  let here =
    ind
    ++ kind
    ++ "\n"
    ++ ind
    ++ "in:  "
    ++ inc
    ++ "\n"
    ++ ind
    ++ "out: "
    ++ out
    ++ (auto_in == "" ? "" : "\n" ++ ind ++ "auto_in:  " ++ auto_in)
    ++ (auto_out == "" ? "" : "\n" ++ ind ++ "auto_out: " ++ auto_out)
    ++ (marks == "" ? "" : "\n" ++ ind ++ "marks: " ++ marks);
  let kids =
    switch (p.term) {
    | Seq(a, b) => [dump(pm, a, ind), dump(pm, b, ind)]
    | Forall(_, b)
    | Assume(_, _, b)
    | Generalize(_, b)
    | Revert(_, _, b)
    | Alias(_, _, b) => [dump(pm, b, ind ++ "  ")]
    | Have(_, sub, body) => [
        dump(pm, sub, ind ++ "  "),
        dump(pm, body, ind ++ "  "),
      ]
    | Induction(_, _, cases) =>
      List.map(((_, b)) => dump(pm, b, ind ++ "  "), cases)
    | _ => []
    };
  String.concat("\n", [here, ...kids]);
};

let dump_obs = (pm: ProofMap.t, p: Proof.t): string =>
  ProofMap.obligations_of_proof(pm, p)
  |> List.map((ob: Obligation.t) =>
       "  "
       ++ trunc(print_exp(Obligation.display_goal_of(ob)))
       ++ " -> "
       ++ Obligation.show_discharge(ob.discharge)
     )
  |> String.concat("\n");

/* CompareFun is gone on this snapshot (noncomputable-as-warning). Function
 * `==` inside a theorem elaborates with no mark; outside a theorem it is
 * Warning.FunEqOutsideTheorem. Pin both. */
let statics_note = (statics: Statics.Map.t): string => {
  let marks = ref([]);
  let warnings = ref([]);
  Id.Map.iter(
    (_, info: Info.t) => {
      marks := Info.marks_of(info) @ marks^;
      warnings := Info.warnings_of(info) @ warnings^;
    },
    statics,
  );
  let mark_s =
    switch (marks^) {
    | [] => "no statics marks"
    | ms => "statics marks: " ++ String.concat("; ", List.map(Mark.show, ms))
    };
  let has_fun_eq =
    List.exists(
      fun
      | Warning.Exp(FunEqOutsideTheorem(_)) => true
      | _ => false,
      warnings^,
    );
  mark_s
  ++ (
    has_fun_eq
      ? "; FunEqOutsideTheorem PRESENT"
      : "; FunEqOutsideTheorem absent (CompareFun ctor not in this snapshot)"
  );
};

let check_status = (msg, expected: ProofMap.full_status, pm, proof, statics) => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  if (actual != expected) {
    let cf = "\n" ++ statics_note(statics);
    let obs = dump_obs(pm, proof);
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(actual)
      ++ cf
      ++ (obs == "" ? "" : "\n--- obligations ---\n" ++ obs)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* --- 1. Pointwise commute (Algebrite) -------------------------------- */

let commute_src = {|theorem t = forall x -> x + 1 == 1 + x proof rewrite x + 1 with 1 + x at 0 end; axiom refl_eq at 0 on 1 + x == 1 + x end in t|};

let test_pointwise_commute = () => {
  let (pm, proof, statics, _) = run(commute_src);
  check_status(
    "pointwise commute x+1 == 1+x",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* --- 2. refl_eq on identical lambdas --------------------------------- */

let identical_fun_src = {|theorem t = (fun x -> x + 1) == (fun x -> x + 1) proof axiom refl_eq at 0 on (fun x -> x + 1) == (fun x -> x + 1) end in t|};

let test_identical_fun_refl = () => {
  let (pm, proof, statics, _) = run(identical_fun_src);
  check_status(
    "refl_eq on identical lambdas: " ++ statics_note(statics),
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* --- 3. Distinct bodies, no funext ----------------------------------- */

let distinct_fun_src = {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t|};

let test_distinct_fun_incomplete = () => {
  let (pm, proof, statics, _) = run(distinct_fun_src);
  /* Pin: statement elaborates (CompareFun gone; FunEq warning only
   * outside theorems) and the hole proof is Incomplete. */
  check_status(
    "distinct lambdas, no funext: " ++ statics_note(statics),
    ProofMap.Incomplete,
    pm,
    proof,
    statics,
  );
};

/* --- 4. Recursive len defining equations ----------------------------- */

let len_prelude = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in |};

let len_nil_src =
  len_prelude
  ++ {|theorem t = len([]) == 0 proof eval len([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};

let test_len_nil = () => {
  let (pm, proof, statics, _) = run_named("t", len_nil_src);
  check_status(
    "len([]) == 0 by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

let len_cons_src =
  len_prelude
  ++ {|theorem t = forall h -> forall t -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in t|};

let test_len_cons = () => {
  let (pm, proof, statics, _) = run_named("t", len_cons_src);
  check_status(
    "len(h :: t) == 1 + len(t) by eval+refl (no extra forall wrappers)",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* --- 5. fold_left defining equations (builtin) ----------------------- */

let fold_nil_src = {|theorem t = forall f -> forall acc -> fold_left([], f, acc) == acc proof eval fold_left([], f, acc) at 0 end; axiom refl_eq at 0 on acc == acc end in t|};

let test_fold_nil = () => {
  let (pm, proof, statics, _) = run(fold_nil_src);
  check_status(
    "fold_left([], f, acc) == acc by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* Attempt 1: one eval. Dump: LHS becomes the FixUnwrap residual
 *   (fun (xs',f',acc') -> case … (fix fold_left' -> …) …)(xs, f, f(acc, x))
 *   == fold_left(xs, f, f(acc, x))
 * Attempt 2: a second eval betas into `case xs`, still ≠ named fold_left.
 * Pin Incomplete. */
let fold_cons_src = {|theorem t = forall x -> forall xs -> forall f -> forall acc -> fold_left(x :: xs, f, acc) == fold_left(xs, f, f(acc, x)) proof eval fold_left(x :: xs, f, acc) at 0 end; axiom refl_eq at 0 on fold_left(xs, f, f(acc, x)) == fold_left(xs, f, f(acc, x)) end in t|};

let test_fold_cons = () => {
  let (pm, proof, statics, _) = run(fold_cons_src);
  check_status(
    "fold_left cons: FixUnwrap residual ≠ named fold_left (pinned Incomplete)",
    ProofMap.Incomplete,
    pm,
    proof,
    statics,
  );
};

let add1_src = {|let add1 = fun (acc, _) -> acc + 1 in theorem t = forall acc -> forall x -> add1((acc, x)) == acc + 1 proof eval add1((acc, x)) at 0 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in t|};

let test_add1 = () => {
  let (pm, proof, statics, _) = run_named("t", add1_src);
  check_status(
    "add1((acc, x)) == acc + 1 by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* --- 6. reverse defining equations (user-defined) -------------------- */

let rev_prelude = {|let rev = fun xs -> case xs | [] => [] | x :: xs => rev(xs) @ [x] end in |};

let rev_nil_src =
  rev_prelude
  ++ {|theorem t = rev([]) == [] proof eval rev([]) at 0 end; axiom refl_eq at 0 on [] == [] end in t|};

let test_rev_nil = () => {
  let (pm, proof, statics, _) = run_named("t", rev_nil_src);
  check_status(
    "rev([]) == [] by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

let rev_cons_src =
  rev_prelude
  ++ {|theorem t = forall x -> forall xs -> rev(x :: xs) == rev(xs) @ [x] proof eval rev(x :: xs) at 0 end; axiom refl_eq at 0 on rev(xs) @ [x] == rev(xs) @ [x] end in t|};

let test_rev_cons = () => {
  let (pm, proof, statics, _) = run_named("t", rev_cons_src);
  check_status(
    "rev(x :: xs) == rev(xs) @ [x] by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* --- 7. Nat encode/decode defining equations ------------------------- */

/* Attempt 1: `type Nat = rec n -> +Z + S(n)` shadows BaseTyp Nat
 * (TPatShadowsType) and FreeConstructor on of_int's Z/S; to_int still
 * proved. Attempt 2: avoid the BaseTyp name; keep rec spelling. */
let nat_prelude = {|type Nt = rec n -> +Z + S(n) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun k -> if k <= 0 then Z else S(of_int(k - 1)) end in |};

let to_int_z_src =
  nat_prelude
  ++ {|theorem t = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};

let test_to_int_z = () => {
  let (pm, proof, statics, _) = run_named("t", to_int_z_src);
  check_status(
    "to_int(Z) == 0 by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

let to_int_s_src =
  nat_prelude
  ++ {|theorem t = forall m -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in t|};

let test_to_int_s = () => {
  let (pm, proof, statics, _) = run_named("t", to_int_s_src);
  check_status(
    "to_int(S(m)) == 1 + to_int(m) by eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* Attempt 1: one eval. Conditional is visible, so this is likely stuck
 * at `if 0 <= 0 then Z else …`. Pin after dump. */
/* Attempt 1: one eval unfolds to `if 0 <= 0 then Z else S(fix...(0-1))`
 * and refl cannot find `Z == Z`. Attempt 2: eval the if. */
let of_int_0_eval_src =
  nat_prelude
  ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; eval (if 0 <= 0 then Z else $e) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_0_eval = () => {
  let (pm, proof, statics, _) = run_named("t", of_int_0_eval_src);
  check_status(
    "of_int(0) == Z by one eval+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* Attempt 2: extra evals of the residual comparison / if. */
let of_int_0_evals_src =
  nat_prelude
  ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; eval 0 <= 0 at 0 end; eval if true then Z else S(of_int(0 - 1)) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_0_evals = () => {
  let (pm, proof, statics, _) = run_named("t", of_int_0_evals_src);
  check_status(
    "of_int(0) == Z by FunAp + compare + if + refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

/* Attempt 3: closed assume, rewrite, refl. Channel 2 Evaluated if
 * big-step of of_int(0)==Z reaches true. */
let of_int_0_assume_src =
  nat_prelude
  ++ {|theorem t = of_int(0) == Z proof assume of_int(0) == Z => axiom assume at 0 on of_int(0) end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_0_assume = () => {
  let (pm, proof, statics, _) = run_named("t", of_int_0_assume_src);
  check_status(
    "of_int(0) == Z by assume+Evaluated+refl",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

let of_int_succ_src =
  nat_prelude
  ++ {|theorem t = forall k where k >= 0 -> of_int(1 + k) == S(of_int(k)) proof eval of_int(1 + k) at 0 end; axiom refl_eq at 0 on S(of_int(k)) == S(of_int(k)) end in t|};

let test_of_int_succ = () => {
  let (pm, proof, statics, _) = run_named("t", of_int_succ_src);
  check_status(
    "of_int(1 + k) == S(of_int(k)) for k >= 0",
    ProofMap.Proven,
    pm,
    proof,
    statics,
  );
};

let tests = (
  "Equivalence equations",
  [
    test_case("1 pointwise commute", `Quick, test_pointwise_commute),
    test_case("2 identical fun refl_eq", `Quick, test_identical_fun_refl),
    test_case("3 distinct fun Incomplete", `Quick, test_distinct_fun_incomplete),
    test_case("4a len nil", `Quick, test_len_nil),
    test_case("4b len cons", `Quick, test_len_cons),
    test_case("5a fold_left nil", `Quick, test_fold_nil),
    test_case("5b fold_left cons", `Quick, test_fold_cons),
    test_case("5c add1", `Quick, test_add1),
    test_case("6a rev nil", `Quick, test_rev_nil),
    test_case("6b rev cons", `Quick, test_rev_cons),
    test_case("7a to_int Z", `Quick, test_to_int_z),
    test_case("7b to_int S", `Quick, test_to_int_s),
    test_case("7c of_int 0 one eval", `Quick, test_of_int_0_eval),
    test_case("7c2 of_int 0 extra evals", `Quick, test_of_int_0_evals),
    test_case("7d of_int 0 assume", `Quick, test_of_int_0_assume),
    test_case("7e of_int succ", `Quick, test_of_int_succ),
  ],
);
