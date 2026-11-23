open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* `from <selector> at <n> do <steps> end`: the steps act on one
 * occurrence of the selector, and their result is put back in the goal.
 * The rewrite steps themselves act only at the root of their goal. */

let outgoing = (pm: ProofMap.t, p: Proof.t): string =>
  switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
  | Some({outgoing: Some(e), _}) => print_exp(e)
  | _ => Alcotest.fail("no outgoing goal recorded\n" ++ dump(pm, p))
  };

/* The first step of a chain. */
let head = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Seq(first, _) => first
  | _ => p
  };

let focus_body = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Focus({body, _}) => body
  | _ => Alcotest.fail("not a focus: " ++ Proof.show(p))
  };

let has = (pred, pm, proof) => has_mark_kind(pm, proof, pred);

let nothing_to_step: ProofMark.t => bool =
  fun
  | NothingToStep => true
  | _ => false;

let test_at_selects_the_nth_match = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (1 + 1) * (1 + 1) == 4 proof from 1 + 1 at 2 do step end; ? in t",
    );
  check(
    string,
    "the second `1 + 1` is evaluated in place",
    "(1 + 1) * 2 == 4",
    outgoing(pm, head(proof)),
  );
};

let test_missing_occurrence_is_marked = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (1 + 1) * (1 + 1) == 4 proof from 1 + 1 at 3 do step end in t",
    );
  check(
    bool,
    "there is no third `1 + 1`",
    true,
    has(
      fun
      | PatternNotFound({ordinal: 3, _}) => true
      | _ => false,
      pm,
      proof,
    ),
  );
};

let test_steps_chain_inside_a_focus = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (2 + 3) * 2 == 10 proof from (2 + 3) * 2 do from 2 + 3 do step end; step end; step in t",
    );
  check_mark_free("a focus of two steps", pm, proof);
  check_proven("its result is lifted into the goal", pm, proof);
};

let test_eval_refuses_a_root_that_is_not_a_redex = () => {
  let (pm, proof) = run_named("t", "theorem t = 1 + 2 == 3 proof step in t");
  check(
    bool,
    "`1 + 2 == 3` can't step at its root",
    true,
    has(nothing_to_step, pm, proof),
  );
};

let test_obligations_belong_to_the_inner_step = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = 4 / 2 == 2 proof from 4 / 2 do algebra 2 end end; ? in t",
    );
  let focus = head(proof);
  let obligations = p =>
    switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
    | Some(entry) => List.length(entry.obligations)
    | None => Alcotest.fail("no entry")
    };
  check(int, "the focus incurs none", 0, obligations(focus));
  check(
    bool,
    "the rewrite incurs the divisor's",
    true,
    obligations(focus_body(focus)) > 0,
  );
};

/* Under a binder the variable is fresh: closed parts still evaluate, an
 * open part can't, and an outer fact about a same-named variable isn't
 * available. */
let test_eval_under_a_binder = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (fun x: Int -> 1 + 2) == (fun x: Int -> 3) proof from 1 + 2 do step end; rewrite refl end in t",
    );
  check_mark_free("a closed subterm of a lambda body steps", pm, proof);
  check_proven("and the lambdas are then equal", pm, proof);
};

let test_open_term_does_not_step = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (fun x: Int -> x + 0) == (fun x: Int -> x) proof from x + 0 do step end in t",
    );
  check(
    bool,
    "`x + 0` with `x` bound by the lambda can't step",
    true,
    has(nothing_to_step, pm, proof),
  );
};

/* Anything but a positive literal after `at` makes the focus a hole. */
let test_occurrence_is_a_positive_literal = () =>
  List.iter(
    ~f=
      at => {
        let (_, proof) =
          run_named(
            "t",
            "theorem t = 1 + 1 == 2 proof from 1 + 1 at "
            ++ at
            ++ " do step end in t",
          );
        check(
          bool,
          "`at " ++ at ++ "` is not a focus",
          false,
          switch (proof.term) {
          | Focus(_) => true
          | _ => false
          },
        );
      },
    ["0", "x", "?"],
  );

/* A selector that is just a definition's name finds it in the goal. */
let test_selector_naming_a_definition = () => {
  let (pm, proof) =
    run_named(
      "t",
      "let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in theorem t = rev([]) == [] proof from rev do step end; ? in t",
    );
  check(
    bool,
    "found\n" ++ dump(pm, proof),
    false,
    has(
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
      pm,
      proof,
    ),
  );
};

/* `step` on a focused redex contracts it whatever its argument is. */
let first_outgoing = (src: string): string => {
  let (pm, proof) = run_named("t", src);
  outgoing(pm, head(proof));
};

let refused = (src: string): bool => {
  let (pm, proof) = run_named("t", src);
  has(nothing_to_step, pm, proof);
};

let test_redexes_contract_at_any_argument = () => {
  check(
    string,
    "a lambda applied to a sum",
    "2 + 3 + 1 == 6",
    first_outgoing(
      "theorem t = (fun x -> x + 1)(2 + 3) == 6 proof from (fun x -> x + 1)(2 + 3) do step end; ? in t",
    ),
  );
  check(
    string,
    "a let",
    "2 + 3 + 1 == 6",
    first_outgoing(
      "theorem t = (let x = 2 + 3 in x + 1) == 6 proof from (let x = 2 + 3 in x + 1) do step end; ? in t",
    ),
  );
  check(
    string,
    "a named function",
    "2 + 3 + 1 == 6",
    first_outgoing(
      "let inc = fun x -> x + 1 in theorem t = inc(2 + 3) == 6 proof from inc(2 + 3) do step end; ? in t",
    ),
  );
  check(
    string,
    "a value argument",
    "5 + 1 == 6",
    first_outgoing(
      "theorem t = (fun x -> x + 1)(5) == 6 proof from (fun x -> x + 1)(5) do step end; ? in t",
    ),
  );
};

let test_some_redexes_wait_for_a_value = () => {
  check(
    bool,
    "a destructuring parameter needs its argument's shape",
    true,
    refused(
      "theorem t = (fun (a, b) -> a + b)(if true then (1, 2) else (3, 4)) == 3 proof from (fun (a, b) -> a + b)(if true then (1, 2) else (3, 4)) do step end; ? in t",
    ),
  );
  check(
    bool,
    "a recursive function unfolds by evaluation",
    true,
    refused(
      "let count = fun n -> if n <= 0 then 0 else count(n - 1) in theorem t = count(1 + 1) == 0 proof from count(1 + 1) do step end; ? in t",
    ),
  );
  check(
    bool,
    "a root that isn't a redex, with an operand that isn't a value",
    true,
    refused("theorem t = 2 + 3 == 5 proof step; ? in t"),
  );
};

/* A goal's variables are free, so the argument's must not be captured
 * by a binder in the body. */
let test_contraction_avoids_capture = () => {
  check(
    string,
    "a variable argument",
    "(fun y' -> y) == (fun z -> y)",
    first_outgoing(
      "theorem t = forall y: Int -> (fun x -> fun y -> x)(y) == (fun z -> y) proof from (fun x -> fun y -> x)(y) do step end; ? in t",
    ),
  );
  check(
    string,
    "an argument that isn't a value",
    "(fun y' -> y + (1 + 1)) == (fun z -> y)",
    first_outgoing(
      "theorem t = forall y: Int -> (fun x -> fun y -> x)(y + (1 + 1)) == (fun z -> y) proof from (fun x -> fun y -> x)(y + (1 + 1)) do step end; ? in t",
    ),
  );
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = forall y: Int -> (fun x -> fun y -> x)(y)(0) == 0 proof from (fun x -> fun y -> x)(y) do step end; step; ? in t",
    );
  check(
    bool,
    "so a false statement isn't proved",
    false,
    Poly.equal(ProofMap.full_status_of_proof(pm, proof), ProofMap.Proven),
  );
};

/* The argument has no value, but beta at any argument doesn't ask for
 * one, so this is provable. */
let test_contraction_drops_an_argument_with_no_value = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (fun x -> 0)(1 / 0) == 0 proof from (fun x -> 0)(1 / 0) do step end; step in t",
    );
  check(
    bool,
    "proven",
    true,
    Poly.equal(ProofMap.full_status_of_proof(pm, proof), ProofMap.Proven),
  );
};

/* Runs: a selector can name consecutive operands of an associative
 * operator, which the parser's grouping leaves without a node. */

let focus_outgoing = (theorem: string): string => {
  let (pm, proof) = run_named("t", theorem);
  outgoing(pm, head(proof));
};

let is_not_found =
  fun
  | ProofMark.PatternNotFound(_) => true
  | _ => false;

let test_a_run_is_focused = () =>
  check(
    string,
    "`2 + 3` is a run of `(1 + 2) + 3`",
    "1 + 5 == 6",
    focus_outgoing(
      "theorem t = 1 + 2 + 3 == 6 proof from 2 + 3 do step end; ? in t",
    ),
  );

let test_a_run_is_put_back_as_parsed = () =>
  check(
    string,
    "the chain reads as typed, without parentheses",
    "1 + 5 + 4 == 10",
    focus_outgoing(
      "theorem t = 1 + 2 + 3 + 4 == 10 proof from 2 + 3 do step end; ? in t",
    ),
  );

let test_a_chain_result_joins_the_chain = () =>
  check(
    string,
    "the result's operands become the chain's",
    "a + c + b + d == a + c + b + d",
    focus_outgoing(
      "theorem t = forall a: Int -> forall b: Int -> forall c: Int -> forall d: Int -> a + b + c + d == a + c + b + d proof from b + c do algebra c + b end end; ? in t",
    ),
  );

let test_a_parenthesized_chain_keeps_its_grouping = () =>
  check(
    string,
    "the chain is still the left operand of `*`",
    "(1 + 5) * 2 == 12",
    focus_outgoing(
      "theorem t = (1 + 2 + 3) * 2 == 12 proof from 2 + 3 do step end; ? in t",
    ),
  );

let test_a_selector_regroups_a_whole_chain = () =>
  check(
    string,
    "`1 + (2 + 3)` names all of `(1 + 2) + 3`",
    "1 + 5 == 6",
    focus_outgoing(
      "theorem t = 1 + 2 + 3 == 6 proof from 1 + (2 + 3) do from 2 + 3 do step end end; ? in t",
    ),
  );

let test_a_run_with_a_metavariable = () =>
  check(
    string,
    "`2 + $e` names `2 + 3`",
    "1 + 5 == 6",
    focus_outgoing(
      "theorem t = 1 + 2 + 3 == 6 proof from 2 + $e do step end; ? in t",
    ),
  );

let test_runs_are_numbered_after_subterms = () => {
  let at = n =>
    focus_outgoing(
      "theorem t = 1 + 1 + 1 + 1 == 4 proof from 1 + 1 at "
      ++ n
      ++ " do step end; ? in t",
    );
  check(string, "the subterm first", "2 + 1 + 1 == 4", at("1"));
  check(string, "then the runs, left to right", "1 + 2 + 1 == 4", at("2"));
  check(string, "overlapping runs count", "1 + 1 + 2 == 4", at("3"));
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = 1 + 1 + 1 + 1 == 4 proof from 1 + 1 at 4 do step end in t",
    );
  check(
    bool,
    "the subterm isn't counted again as a run",
    true,
    has(is_not_found, pm, proof),
  );
};

let test_runs_across_a_minus = () => {
  check(
    string,
    "`3 - 1` is a run of `(5 + 3) - 1`, and `5 + (3 - 1)` is equal",
    "5 + 2 == 7",
    focus_outgoing(
      "theorem t = 5 + 3 - 1 == 7 proof from 3 - 1 do step end; ? in t",
    ),
  );
  List.iter(
    ~f=
      ((theorem, why)) => {
        let (pm, proof) = run_named("t", theorem);
        check(bool, why, true, has(is_not_found, pm, proof));
      },
    [
      (
        "theorem t = 8 - 4 - 2 == 2 proof from 4 - 2 do step end in t",
        "`4 - 2` would be added, though `8 - 4 - 2` subtracts the 4",
      ),
      (
        "theorem t = 5 + 3 + 1 == 9 proof from 3 - 1 do step end in t",
        "`3 - 1` subtracts what `5 + 3 + 1` adds",
      ),
    ],
  );
};

let test_runs_of_right_grouped_operators = () => {
  check(
    string,
    "`&&` groups to the right",
    "false && true == false",
    focus_outgoing(
      "theorem t = true && false && true == false proof from true && false do step end; ? in t",
    ),
  );
  check(
    string,
    "and so does `@`",
    "[1, 2] @ [3] == [1, 2, 3]",
    focus_outgoing(
      "theorem t = [1] @ [2] @ [3] == [1, 2, 3] proof from [1] @ [2] do step end; ? in t",
    ),
  );
};

let test_no_run_of_a_non_associative_chain = () =>
  List.iter(
    ~f=
      ((theorem, why)) => {
        let (pm, proof) = run_named("t", theorem);
        check(bool, why, true, has(is_not_found, pm, proof));
      },
    [
      (
        "theorem t = 1. +. 2. +. 3. == 6. proof from 2. +. 3. do step end in t",
        "float addition rounds differently when regrouped",
      ),
      (
        "theorem t = 1 - 2 + 3 == 2 proof from 2 + 3 do step end in t",
        "`2 + 3` isn't a run of `(1 - 2) + 3`",
      ),
      (
        "theorem t = 2 * 3 + 4 == 10 proof from 3 + 4 do step end in t",
        "`3 + 4` isn't a run of `(2 * 3) + 4`",
      ),
    ],
  );

let test_a_run_under_a_binder = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (fun x: Int -> x + 1 + 2) == (fun x: Int -> x + 3) proof from 1 + 2 do step end; rewrite refl end in t",
    );
  check_mark_free("a closed run in a lambda body steps", pm, proof);
  check_proven("and the lambdas are then equal", pm, proof);
};

let test_a_run_proves_a_theorem = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = forall x: Int -> x + 2 + 3 == x + 5 proof from 2 + 3 do step end; rewrite refl end in t",
    );
  check_mark_free("no marks", pm, proof);
  check_proven("proven", pm, proof);
};

let tests = (
  "Evaluator.Focus",
  [
    test_case(
      "step contracts a focused redex whatever its argument",
      `Quick,
      test_redexes_contract_at_any_argument,
    ),
    test_case(
      "destructuring, recursion and other roots wait for a value",
      `Quick,
      test_some_redexes_wait_for_a_value,
    ),
    test_case(
      "a contraction avoids capture",
      `Quick,
      test_contraction_avoids_capture,
    ),
    test_case(
      "a contraction can drop an argument with no value",
      `Quick,
      test_contraction_drops_an_argument_with_no_value,
    ),
    test_case(
      "an occurrence is a positive literal",
      `Quick,
      test_occurrence_is_a_positive_literal,
    ),
    test_case(
      "at n selects the nth match",
      `Quick,
      test_at_selects_the_nth_match,
    ),
    test_case(
      "a missing occurrence is marked",
      `Quick,
      test_missing_occurrence_is_marked,
    ),
    test_case(
      "steps chain inside a focus and the result is lifted",
      `Quick,
      test_steps_chain_inside_a_focus,
    ),
    test_case(
      "eval refuses a root that is not a redex",
      `Quick,
      test_eval_refuses_a_root_that_is_not_a_redex,
    ),
    test_case(
      "obligations belong to the inner step",
      `Quick,
      test_obligations_belong_to_the_inner_step,
    ),
    test_case("eval under a binder", `Quick, test_eval_under_a_binder),
    test_case(
      "an open term does not step",
      `Quick,
      test_open_term_does_not_step,
    ),
    test_case(
      "a selector naming a definition finds it",
      `Quick,
      test_selector_naming_a_definition,
    ),
    test_case("a run is focused", `Quick, test_a_run_is_focused),
    test_case(
      "a run is put back as parsed",
      `Quick,
      test_a_run_is_put_back_as_parsed,
    ),
    test_case(
      "a chain result joins the chain",
      `Quick,
      test_a_chain_result_joins_the_chain,
    ),
    test_case(
      "a parenthesized chain keeps its grouping",
      `Quick,
      test_a_parenthesized_chain_keeps_its_grouping,
    ),
    test_case(
      "a selector regroups a whole chain",
      `Quick,
      test_a_selector_regroups_a_whole_chain,
    ),
    test_case(
      "a run with a metavariable",
      `Quick,
      test_a_run_with_a_metavariable,
    ),
    test_case(
      "runs are numbered after subterms",
      `Quick,
      test_runs_are_numbered_after_subterms,
    ),
    test_case("runs across a minus", `Quick, test_runs_across_a_minus),
    test_case(
      "runs of right-grouped operators",
      `Quick,
      test_runs_of_right_grouped_operators,
    ),
    test_case(
      "no run of a non-associative chain",
      `Quick,
      test_no_run_of_a_non_associative_chain,
    ),
    test_case("a run under a binder", `Quick, test_a_run_under_a_binder),
    test_case("a run proves a theorem", `Quick, test_a_run_proves_a_theorem),
  ],
);
