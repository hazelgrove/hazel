open Alcotest;
open Language;
open Test_ProofMap.Harness;
open Poly;

/* The two splits. `cases e` gives each case the case equation
 * `e == pat` and no inductive hypothesis. `induction e` gives each case
 * its inductive hypotheses and no case equation, a computed `e` is first
 * generalized, and the facts that mention `e` are out of scope in its
 * cases. Nothing gives both: an inductive hypothesis kept beside a fact
 * tying the case to the scrutinee's actual value would prove too much. */

let tl = "let tl = fun xs -> case xs | [] => [] | _ :: t => t end in ";

/* The bodies of the root split's cases. */
let case_bodies = (proof: Proof.t): list(Proof.t) => {
  let rec split = (p: Proof.t) =>
    switch (p.term) {
    | Cases(_, cases)
    | Induction(_, cases) => List.map(~f=snd, cases)
    | Assume(_, _, body) => split(body)
    | _ => Alcotest.fail("no split at the root")
    };
  split(proof);
};

/* Printed without spaces, which the printer places loosely. */
let printed = (e: Exp.t): string =>
  print_exp(e) |> String.split(~on=' ') |> String.concat(~sep="");

/* The facts in scope in a case body, by name, with their statements. */
let facts = (pm: ProofMap.t, body: Proof.t): list((string, string)) =>
  switch (ProofMap.lookup(Proof.rep_id(body), pm)) {
  | Some({ctx, _}) =>
    SemanticCtx.facts(ctx)
    |> List.map(~f=((name, _, fact)) => (name, printed(fact)))
  | None => Alcotest.fail("no scope recorded for the case")
  };

let check_goal = (msg, expected, pm, body) =>
  check(string, msg, expected, printed(incoming_of(pm, body)));

let check_fact = (msg, ~name, expected, pm, body) =>
  check(
    option(string),
    msg,
    expected,
    List.Assoc.find(facts(pm, body), name, ~equal=String.equal),
  );

let both_cases = (src: string): (ProofMap.t, Proof.t, Proof.t) => {
  let (pm, proof) = run(src);
  switch (case_bodies(proof)) {
  | [nil, cons] => (pm, nil, cons)
  | _ => Alcotest.fail("expected two cases")
  };
};

let test_induction_has_no_case_equation = () => {
  let (pm, nil, cons) =
    both_cases(
      "theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => ? | y :: rest => ? end in t",
    );
  check_fact("no case equation for []", ~name="case_eq", None, pm, nil);
  check_fact("no case equation for ::", ~name="case_eq", None, pm, cons);
  check_fact(
    "the inductive hypothesis",
    ~name="ih_rest",
    Some("rest==rest"),
    pm,
    cons,
  );
};

let test_cases_has_no_inductive_hypothesis = () => {
  let (pm, _, cons) =
    both_cases(
      "theorem t = forall xs: [Int] -> xs == xs proof cases xs | [] => ? | y :: rest => ? end in t",
    );
  check_fact(
    "the case equation",
    ~name="case_eq",
    Some("xs==y::rest"),
    pm,
    cons,
  );
  check_fact("no inductive hypothesis", ~name="ih_rest", None, pm, cons);
};

/* With `xs == [1]` in scope beside the unconditional `ih_rest`, the cons
 * case could prove anything about `xs`. */
let test_induction_hides_facts_about_a_variable = () => {
  let src = "theorem t = forall xs: [Int] -> forall n: Int -> n == 1 ==> xs == [1] ==> xs == xs proof assume hn = n == 1 => assume xs == [1] => induction xs | [] => ? | y :: rest => ? end in t";
  let (pm, nil, cons) = both_cases(src);
  check_fact(
    "the fact about xs is out of scope",
    ~name="assume",
    None,
    pm,
    nil,
  );
  check_fact(
    "the fact about xs is out of scope",
    ~name="assume",
    None,
    pm,
    cons,
  );
  check_fact(
    "a fact that doesn't mention xs stays",
    ~name="hn",
    Some("n==1"),
    pm,
    cons,
  );
};

let test_cases_keeps_facts_about_a_variable = () => {
  let (pm, _, cons) =
    both_cases(
      "theorem t = forall xs: [Int] -> xs == [1] ==> xs == xs proof assume xs == [1] => cases xs | [] => ? | y :: rest => ? end in t",
    );
  check_fact(
    "the fact about xs stays",
    ~name="assume",
    Some("xs==[1]"),
    pm,
    cons,
  );
};

/* A `where` restriction on the statement's binder is a fact like any
 * other: about `xs`, so out of scope in an induction case on `xs`. */
let test_induction_hides_a_where_fact_about_the_scrutinee = () => {
  let (pm, nil, cons) =
    both_cases(
      "theorem t = forall xs: [Int] where xs == [1] -> xs == xs proof induction xs | [] => ? | y :: rest => ? end in t",
    );
  check_fact("out of scope for []", ~name="where", None, pm, nil);
  check_fact("out of scope for ::", ~name="where", None, pm, cons);
};

let test_cases_keeps_a_where_fact_about_the_scrutinee = () => {
  let (pm, _, cons) =
    both_cases(
      "theorem t = forall xs: [Int] where xs == [1] -> xs == xs proof cases xs | [] => ? | y :: rest => ? end in t",
    );
  check_fact(
    "the restriction stays",
    ~name="where",
    Some("xs==[1]"),
    pm,
    cons,
  );
};

/* A guarded arm's guard is about the arm's own variables, so an
 * induction case keeps it. */
let test_induction_keeps_an_arm_guard = () => {
  let (pm, proof) =
    run(
      "theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => ? | y :: rest where y > 0 => ? | y :: rest => ? end in t",
    );
  switch (case_bodies(proof)) {
  | [_, guarded, _] =>
    check_fact("the guard", ~name="where", Some("y>0"), pm, guarded)
  | _ => Alcotest.fail("expected three cases")
  };
};

/* Induction on a computed value is induction on a fresh variable in its
 * place. The cases' goals and hypotheses have the pattern in place of
 * `tl(xs)`, and nothing in scope says what `tl(xs)` is. */
let test_induction_generalizes_a_computed_scrutinee = () => {
  let (pm, nil, cons) =
    both_cases(
      tl
      ++ "theorem t = forall xs: [Int] -> tl(xs) == [] ==> tl(xs) == tl(xs) proof assume tl(xs) == [] => induction tl(xs) | [] => ? | y :: rest => ? end in t",
    );
  check_goal("the [] case's goal", "[]==[]", pm, nil);
  check_goal("the :: case's goal", "y::rest==y::rest", pm, cons);
  check_fact(
    "the inductive hypothesis",
    ~name="ih_rest",
    Some("rest==rest"),
    pm,
    cons,
  );
  check_fact("no case equation", ~name="case_eq", None, pm, nil);
  check_fact(
    "the fact about tl(xs) is out of scope",
    ~name="assume",
    None,
    pm,
    nil,
  );
};

let test_cases_on_a_computed_scrutinee = () => {
  let (pm, _, cons) =
    both_cases(
      tl
      ++ "theorem t = forall xs: [Int] -> tl(xs) == [] ==> tl(xs) == tl(xs) proof assume tl(xs) == [] => cases tl(xs) | [] => ? | y :: rest => ? end in t",
    );
  check_fact(
    "the case equation",
    ~name="case_eq",
    Some("tl(xs)==y::rest"),
    pm,
    cons,
  );
  check_fact("no inductive hypothesis", ~name="ih_rest", None, pm, cons);
  check_fact(
    "the fact about tl(xs) stays",
    ~name="assume",
    Some("tl(xs)==[]"),
    pm,
    cons,
  );
};

/* Induction on `[1]` gives the `::` case `ih_t: false`, the goal itself.
 * Sound only because the `[]` case has no case equation `[1] == []` to
 * close it with, so it stays open. */
let test_induction_on_a_literal_proves_nothing = () => {
  let (pm, proof) =
    run_named(
      "bad",
      "theorem bad = false proof induction [1] | [] => rewrite case_eq end | h :: t => rewrite ih_t end end in bad",
    );
  check(
    bool,
    "the [] case can't cite a case equation",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.UnknownEquality("case_eq") => true
      | _ => false,
    ),
  );
  check(
    bool,
    "not proven",
    false,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* A split's cases have to cover its scrutinee's type, so a split with
 * none is not exhaustive, unless the type has no values, where it proves
 * its goal. */
let empty_split = (src: string) => {
  let (pm, proof) = run(src);
  (
    ProofMap.marks_of(Proof.rep_id(proof), pm),
    ProofMap.full_status_of_proof(pm, proof),
  );
};

let test_empty_split_is_not_exhaustive = () =>
  List.iter(
    ~f=
      src =>
        check(
          bool,
          src,
          true,
          switch (empty_split(src)) {
          | ([ProofMark.InductionNotExhaustive], _) => true
          | _ => false
          },
        ),
    [
      "theorem t = forall b: Bool -> b == b proof cases b end in t",
      "theorem t = forall xs: [Int] -> xs == xs proof induction xs end in t",
    ],
  );

let test_empty_split_on_void_proves = () => {
  let (marks, status) =
    empty_split("theorem t = forall v: Void -> false proof cases v end in t");
  check(bool, "no marks", true, marks == []);
  check(bool, "proven", true, status == ProofMap.Proven);
};

/* A case pattern in parens or with an ascription is the scrutinee in
 * the case, not a strict part of it, so it is not one of its own
 * inductive hypotheses: that hypothesis would be the case's goal. */
let wrapped_case = (pat: string) => {
  let src =
    "theorem t = forall xs: [Int] -> xs == [] proof induction xs | [] => rewrite refl end | "
    ++ pat
    ++ " => from y :: t do rewrite ih end end; rewrite refl end end in t";
  let (pm, proof) = run(src);
  let hypotheses =
    switch (case_bodies(proof)) {
    | [_, cons] =>
      facts(pm, cons)
      |> List.map(~f=fst)
      |> List.filter(~f=name => String.is_prefix(name, ~prefix="ih"))
    | _ => Alcotest.fail("expected two cases")
    };
  (ProofMap.full_status_of_proof(pm, proof), hypotheses);
};

let test_wrapped_case_is_not_its_own_hypothesis = (pat, ()) => {
  let (status, hypotheses) = wrapped_case(pat);
  check(bool, "not proven", false, status == ProofMap.Proven);
  check(list(string), "the hypotheses", ["ih_t"], hypotheses);
};

let tests = (
  "CasesInduction",
  [
    test_case(
      "a split with no cases is not exhaustive",
      `Quick,
      test_empty_split_is_not_exhaustive,
    ),
    test_case(
      "a split with no cases on Void proves its goal",
      `Quick,
      test_empty_split_on_void_proves,
    ),
    test_case(
      "an induction case has no case equation",
      `Quick,
      test_induction_has_no_case_equation,
    ),
    test_case(
      "a cases case has no inductive hypothesis",
      `Quick,
      test_cases_has_no_inductive_hypothesis,
    ),
    test_case(
      "an induction case doesn't see facts about its scrutinee",
      `Quick,
      test_induction_hides_facts_about_a_variable,
    ),
    test_case(
      "a cases case keeps facts about its scrutinee",
      `Quick,
      test_cases_keeps_facts_about_a_variable,
    ),
    test_case(
      "an induction case hides a where fact about its scrutinee",
      `Quick,
      test_induction_hides_a_where_fact_about_the_scrutinee,
    ),
    test_case(
      "a cases case keeps a where fact about its scrutinee",
      `Quick,
      test_cases_keeps_a_where_fact_about_the_scrutinee,
    ),
    test_case(
      "an induction case keeps its arm's guard",
      `Quick,
      test_induction_keeps_an_arm_guard,
    ),
    test_case(
      "induction generalizes a computed scrutinee",
      `Quick,
      test_induction_generalizes_a_computed_scrutinee,
    ),
    test_case(
      "cases on a computed scrutinee",
      `Quick,
      test_cases_on_a_computed_scrutinee,
    ),
    test_case(
      "induction on a literal proves nothing",
      `Quick,
      test_induction_on_a_literal_proves_nothing,
    ),
    test_case(
      "a case pattern in parens is not its own hypothesis",
      `Quick,
      test_wrapped_case_is_not_its_own_hypothesis("(y :: t)"),
    ),
    test_case(
      "an ascribed case pattern is not its own hypothesis",
      `Quick,
      test_wrapped_case_is_not_its_own_hypothesis("(y :: t : [Int])"),
    ),
    test_case(
      "a case pattern in two wrappers keeps its inner hypotheses",
      `Quick,
      test_wrapped_case_is_not_its_own_hypothesis("((y :: t))"),
    ),
  ],
);
