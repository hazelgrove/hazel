open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Reconnaissance suite for the "cheap" equivalence examples: defining
 * equations and pointwise commutation facts that should fall out of
 * `eval` + `axiom refl_eq` (plus `rewrite` for commuted arithmetic).
 *
 * These are FULL proofs, not hole lemmas. Each test pins the status the
 * current calculus actually produces, so movement in either direction
 * shows up here: a Proven case regressing, or an Incomplete case
 * quietly starting to work. Where a case is Incomplete the comment
 * records the observed friction and the outgoing goal we saw.
 *
 * Reuses the Test_ProofMap harness, like Test_Generalize / Test_Revert. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;

let run = (src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  (EvaluatorState.get_proof_map(state), proof_of(elab));
};

/* Compact surface rendering of an expression, for friction reports:
 * ProofMap.show_entry is a full Exp.show and unreadable in a failure
 * message. */
let render = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[], _);

let render_opt = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => render(e)
  | None => "<none>"
  };

let entry_of = (pm: ProofMap.t, proof: Proof.t): ProofMap.entry =>
  switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
  | Some(e) => e
  | None => Alcotest.fail("no proof-map entry for the theorem's proof")
  };

/* status + in/out goals + marks, for the message on a pin that moves. */
let dump = (src: string): string => {
  let (pm, proof) = run(src);
  let e = entry_of(pm, proof);
  ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof))
  ++ " | in: "
  ++ render_opt(e.incoming)
  ++ " | out: "
  ++ render_opt(e.outgoing)
  ++ " | marks: "
  ++ string_of_int(List.length(e.marks))
  ++ " | obligations: "
  ++ string_of_int(List.length(e.obligations));
};

/* Every `theorem t = ... in t` program carries a baseline `Free "t"`
 * mark: the theorem binder is introduced by the elaborator, not by
 * statics, so the trailing use of `t` reads as free. Verified against
 * the known-good Test_ProofMap sources. Filter it so a genuine type
 * error is still visible. */
let baseline_mark = (m: Mark.t): bool =>
  switch (m) {
  | Free("t") => true
  | _ => false
  };

let statics_marks = (src: string): list(Mark.t) => {
  let (map, _) = Test_ProofMap.statics_and_elab(parse_exp(src));
  Id.Map.fold((_, info: Info.t, acc) => Info.marks_of(info) @ acc, map, [])
  |> List.filter(m => !baseline_mark(m));
};

/* A case that "didn't elaborate" — a statics error before the prover
 * ever runs — is a different failure from an incomplete proof, so
 * assert cleanliness separately from status. */
let check_elaborates = (msg: string, src: string) =>
  switch (statics_marks(src)) {
  | [] => ()
  | ms =>
    Alcotest.fail(
      msg
      ++ ": expected clean statics, got "
      ++ String.concat(", ", List.map(Mark.show, ms)),
    )
  };

let check_status =
    (msg: string, expected: ProofMap.full_status, src: string): unit => {
  let (pm, proof) = run(src);
  let actual = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    msg ++ " (" ++ dump(src) ++ ")",
    true,
    actual == expected,
  );
};

/* Proven with a clean subtree and no pending obligations. */
let check_proven = (msg: string, src: string) => {
  check_elaborates(msg, src);
  check_status(msg, ProofMap.Proven, src);
};

/* --- 1. rewrite + refl on commuted arithmetic -------------------------- */

/* The forall binder is auto-peeled, so the proof opens directly on the
 * conclusion: rewrite the LHS to the RHS, then close by reflexivity. */
let src_commute_arith = {|theorem t = forall x -> x + 1 == 1 + x proof rewrite x + 1 with 1 + x at 0 end; axiom refl_eq at 0 on 1 + x == 1 + x end in t|};

let test_commute_arith = () =>
  check_proven("rewrite then refl proves x + 1 == 1 + x", src_commute_arith);

/* --- 2. refl_eq at identical function literals ------------------------- */

/* CompareFun is gone, so `==` at an arrow type type-checks inside a
 * theorem; poly_equal on syntactically identical closures is true, so
 * refl closes it outright. */
let src_refl_fun = {|theorem t = (fun x -> x + 1) == (fun x -> x + 1) proof axiom refl_eq at 0 on (fun x -> x + 1) == (fun x -> x + 1) end in t|};

let test_refl_fun = () =>
  check_proven(
    "refl_eq closes syntactically identical functions",
    src_refl_fun,
  );

/* --- 3. distinct function bodies: no funext --------------------------- */

/* Elaborates quietly (no CompareFun), but funext is not implemented, so
 * the hole passes the goal straight through:
 *   out: (fun x -> x + 1) == (fun x -> 1 + x)
 * i.e. the goal never becomes `true`. This is the pin for "funext is the
 * missing rule", not for a broken step. */
let src_funext = {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t|};

let test_funext_incomplete = () => {
  check_elaborates("function equality inside a theorem", src_funext);
  check_status(
    "distinct function bodies stay Incomplete (no funext)",
    ProofMap.Incomplete,
    src_funext,
  );
  /* The hole is a pass-through, not a dark node: no marks. */
  let (pm, proof) = run(src_funext);
  Alcotest.check(
    Alcotest.int,
    "the funext hole carries no marks",
    0,
    List.length(entry_of(pm, proof).marks),
  );
};

/* --- 4. user-defined len: nil and cons defining equations ------------- */

let len_def = {|let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in |};

let src_len_nil =
  len_def
  ++ {|theorem t = len([]) == 0 proof eval len([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};

/* The open cons case works too: `eval` reaches `1 + len(t)` under the
 * peeled binders h and t, and refl closes. The hidden auto-steps cover
 * the FixF unwrap and the case selection, so one visible `eval` is
 * enough. */
let src_len_cons =
  len_def
  ++ {|theorem t = forall h -> forall t -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in t|};

let test_len_nil = () => check_proven("len([]) == 0", src_len_nil);

let test_len_cons = () =>
  check_proven("len(h :: t) == 1 + len(t)", src_len_cons);

/* --- 5. fold_left defining equation + pointwise add1 ------------------ */

let src_fold_nil = {|theorem t = fold_left([], fun (acc, x) -> acc + x, 0) == 0 proof eval fold_left([], fun (acc, x) -> acc + x, 0) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};

/* Pointwise commutation of a user step function under two peeled
 * binders: `add1` is not recursive, so one beta step lands on the RHS. */
let src_add1_pointwise = {|let add1 = fun (acc, _) -> acc + 1 in theorem t = forall acc -> forall x -> add1((acc, x)) == acc + 1 proof eval add1((acc, x)) at 0 end; axiom refl_eq at 0 on acc + 1 == acc + 1 end in t|};

let test_fold_nil = () =>
  check_proven("fold_left nil defining equation", src_fold_nil);

let test_add1_pointwise = () =>
  check_proven("add1((acc, x)) == acc + 1 pointwise", src_add1_pointwise);

/* --- 6. user-defined rev --------------------------------------------- */

let rev_def = {|let rev = fun xs -> case xs | [] => [] | h :: t => rev(t) @ [h] end in |};

let src_rev_nil =
  rev_def
  ++ {|theorem t = rev([]) == [] proof eval rev([]) at 0 end; axiom refl_eq at 0 on [] == [] end in t|};

let src_rev_cons =
  rev_def
  ++ {|theorem t = forall h -> forall t -> rev(h :: t) == rev(t) @ [h] proof eval rev(h :: t) at 0 end; axiom refl_eq at 0 on rev(t) @ [h] == rev(t) @ [h] end in t|};

let test_rev_nil = () => check_proven("rev([]) == []", src_rev_nil);

let test_rev_cons = () =>
  check_proven("rev(h :: t) == rev(t) @ [h]", src_rev_cons);

/* --- 7. Nat: to_int / of_int ----------------------------------------- */

/* NOTE on the type declaration: the sum body must NOT be parenthesized.
 * `type MyNat = rec MyNat -> (+Z + S(MyNat)) in ...` leaves Z and S
 * FreeConstructor — only the unparenthesized form puts the constructors
 * in scope. (`Nat` itself is a base type, so the alias needs a fresh
 * name or it takes a TPatShadowsType mark.) */
let nat_def = {|type MyNat = rec MyNat -> +Z + S(MyNat) in let to_int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in let of_int = fun i -> if i <= 0 then Z else S(of_int(i - 1)) in |};

let src_nat_zero =
  nat_def
  ++ {|theorem t = to_int(Z) == 0 proof eval to_int(Z) at 0 end; axiom refl_eq at 0 on 0 == 0 end in t|};

let src_nat_succ =
  nat_def
  ++ {|theorem t = forall m -> to_int(S(m)) == 1 + to_int(m) proof eval to_int(S(m)) at 0 end; axiom refl_eq at 0 on 1 + to_int(m) == 1 + to_int(m) end in t|};

let test_nat_zero = () => check_proven("to_int(Z) == 0", src_nat_zero);

let test_nat_succ = () =>
  check_proven("to_int(S(m)) == 1 + to_int(m)", src_nat_succ);

/* of_int is guarded by an `if`, and one visible `eval` only gets as far
 * as the conditional. Observed outgoing:
 *
 *   (if 0 <= 0 then Z
 *    else S((fix of_int -> fun i' -> if i' <= 0 then Z
 *            else S(of_int(i' - 1)))(0 - 1))) == Z
 *
 * so the subsequent `axiom refl_eq ... on Z == Z` takes PatternNotFound.
 * `eval` is one dynamic step plus hidden auto-steps, and the hidden
 * steps do not carry the conditional. */
let src_of_int_zero_eval =
  nat_def
  ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_eval_incomplete = () => {
  check_elaborates("of_int by eval", src_of_int_zero_eval);
  check_status(
    "single eval on a guarded recursion stalls at the conditional",
    ProofMap.Incomplete,
    src_of_int_zero_eval,
  );
};

/* Chaining evals gets one step further — `0 <= 0` reduces, leaving
 *
 *   (if true then Z else S((fix of_int -> ...)(0 - 1))) == Z
 *
 * — but the else-branch now contains an unwrapped `fix`, which has no
 * surface syntax, so no third `eval <pattern>` can name the redex. This
 * is the FixUnwrap wall: the residual is unwriteable, not just long. */
let src_of_int_zero_chained =
  nat_def
  ++ {|theorem t = of_int(0) == Z proof eval of_int(0) at 0 end; eval 0 <= 0 at 0 end; eval if true then Z else S(of_int(0 - 1)) at 0 end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_chained_incomplete = () =>
  check_status(
    "chained evals cannot name the fix-unwrapped residual",
    ProofMap.Incomplete,
    src_of_int_zero_chained,
  );

/* The workaround from the closed-computation playbook: `assume` the
 * equation and cite it. The obligation is a closed computation, so the
 * checker discharges it as Evaluated and the theorem is fully Proven
 * (not ProvenModulo) even though one obligation was incurred. */
let src_of_int_zero_assume =
  nat_def
  ++ {|theorem t = of_int(0) == Z proof assume of_int(0) == Z => axiom assume at 0 on of_int(0) end; axiom refl_eq at 0 on Z == Z end in t|};

let test_of_int_assume_proven = () => {
  check_elaborates("of_int by assume", src_of_int_zero_assume);
  check_status(
    "assume + Evaluated discharge closes of_int(0) == Z",
    ProofMap.Proven,
    src_of_int_zero_assume,
  );
  let (pm, proof) = run(src_of_int_zero_assume);
  Alcotest.check(
    Alcotest.int,
    "the assume incurred exactly one obligation",
    1,
    List.length(entry_of(pm, proof).obligations),
  );
};

let tests = (
  "Equivalence equations",
  [
    test_case(
      "commuted arithmetic by rewrite + refl",
      `Quick,
      test_commute_arith,
    ),
    test_case("refl_eq at identical functions", `Quick, test_refl_fun),
    test_case(
      "distinct function bodies: no funext",
      `Quick,
      test_funext_incomplete,
    ),
    test_case("len nil defining equation", `Quick, test_len_nil),
    test_case("len cons defining equation", `Quick, test_len_cons),
    test_case("fold_left nil defining equation", `Quick, test_fold_nil),
    test_case("pointwise add1", `Quick, test_add1_pointwise),
    test_case("rev nil defining equation", `Quick, test_rev_nil),
    test_case("rev cons defining equation", `Quick, test_rev_cons),
    test_case("to_int Z defining equation", `Quick, test_nat_zero),
    test_case("to_int S defining equation", `Quick, test_nat_succ),
    test_case("of_int by eval stalls", `Quick, test_of_int_eval_incomplete),
    test_case(
      "of_int chained evals stall",
      `Quick,
      test_of_int_chained_incomplete,
    ),
    test_case(
      "of_int by assume + Evaluated",
      `Quick,
      test_of_int_assume_proven,
    ),
  ],
);
