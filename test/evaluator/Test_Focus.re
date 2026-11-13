open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* `from <ordinal> <selector> do <steps> end`: the steps act on one
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

let test_ordinal_selects_the_nth_match = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (1 + 1) * (1 + 1) == 4 proof from 2nd 1 + 1 do eval end; ? in t",
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
      "theorem t = (1 + 1) * (1 + 1) == 4 proof from 3rd 1 + 1 do eval end in t",
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
      "theorem t = (2 + 3) * 2 == 10 proof from (2 + 3) * 2 do from 2 + 3 do eval end; eval end; eval in t",
    );
  check_mark_free("a focus of two steps", pm, proof);
  check_proven("its result is lifted into the goal", pm, proof);
};

let test_eval_refuses_a_root_that_is_not_a_redex = () => {
  let (pm, proof) = run_named("t", "theorem t = 1 + 2 == 3 proof eval in t");
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
      "theorem t = 4 / 2 == 2 proof from 4 / 2 do rewrite 2 end end; ? in t",
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
      "theorem t = (fun x: Int -> 1 + 2) == (fun x: Int -> 3) proof from 1 + 2 do eval end; axiom refl_eq end in t",
    );
  check_mark_free("a closed subterm of a lambda body steps", pm, proof);
  check_proven("and the lambdas are then equal", pm, proof);
};

let test_open_term_does_not_step = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = (fun x: Int -> x + 0) == (fun x: Int -> x) proof from x + 0 do eval end in t",
    );
  check(
    bool,
    "`x + 0` with `x` bound by the lambda can't step",
    true,
    has(nothing_to_step, pm, proof),
  );
};

let test_outer_fact_does_not_reach_under_a_binder = () => {
  let control = {|theorem t = forall b: Bool where b -> b && true == true proof from b do axiom where end end; ? in t|};
  let (pm, proof) = run_named("t", control);
  check_mark_free("control: the fact rewrites the outer `b`", pm, proof);
  let (pm, proof) =
    run_named(
      "t",
      {|theorem t = forall b: Bool where b -> (fun b: Bool -> b) == (fun b: Bool -> true) proof from b do axiom where end end in t|},
    );
  check(
    bool,
    "the `where` fact is about the outer `b`, not the lambda's",
    true,
    has(
      fun
      | UnknownEquality("where") => true
      | _ => false,
      pm,
      proof,
    ),
  );
};

let test_ordinals = () => {
  check(
    list(string),
    "spelled",
    ["1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st", "112th"],
    List.map(Token.Ordinal.to_string, [1, 2, 3, 4, 11, 12, 13, 21, 112]),
  );
  check(
    list(option(int)),
    "read back, and only in their own spelling",
    [Some(22), Some(101), None, None, None, None],
    List.map(
      Token.Ordinal.of_string,
      ["22nd", "101st", "2th", "0th", "01st", "st"],
    ),
  );
};

/* A selector that is just a definition's name finds it in the goal. */
let test_selector_naming_a_definition = () => {
  let (pm, proof) =
    run_named(
      "t",
      "let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in theorem t = rev([]) == [] proof from rev do eval end; ? in t",
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

let tests = (
  "Evaluator.Focus",
  [
    test_case("ordinals", `Quick, test_ordinals),
    test_case(
      "an ordinal selects the nth match",
      `Quick,
      test_ordinal_selects_the_nth_match,
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
      "an outer fact does not reach under a binder",
      `Quick,
      test_outer_fact_does_not_reach_under_a_binder,
    ),
    test_case(
      "a selector naming a definition finds it",
      `Quick,
      test_selector_naming_a_definition,
    ),
  ],
);
