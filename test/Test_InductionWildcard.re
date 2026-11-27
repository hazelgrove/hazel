open Alcotest;
open Language;
open Test_ProofMap.Harness;
open Poly;

/* A case pattern with a wildcard is checked like any other. It has no
 * expression to put in the scrutinee's place, so its case's goal keeps
 * the scrutinee as it is: it proves the goal for an arbitrary value,
 * which covers whatever the other cases don't. It gets no case equation
 * and no inductive hypotheses, which it couldn't state. */

/* The root split and its cases' bodies. */
let rec split = (p: Proof.t): (Proof.t, list(Proof.t)) =>
  switch (p.term) {
  | Cases(_, cases)
  | Induction(_, cases) => (p, List.map(~f=snd, cases))
  | Forall(_, body) => split(body)
  | _ => Alcotest.fail("no split at the root")
  };

let last = (xs: list('a)): 'a =>
  switch (List.last(xs)) {
  | Some(x) => x
  | None => Alcotest.fail("the split has no cases")
  };

/* Printed without spaces, which the printer places loosely. */
let printed = (e: Exp.t): string =>
  print_exp(e) |> String.split(~on=' ') |> String.concat(~sep="");

let fact_names = (pm: ProofMap.t, body: Proof.t): list(string) =>
  switch (ProofMap.lookup(Proof.rep_id(body), pm)) {
  | Some({ctx, _}) =>
    SemanticCtx.facts(ctx) |> List.map(~f=((n, _, _)) => n)
  | None => Alcotest.fail("no scope recorded for the case")
  };

let split_facts = (pm, body) =>
  fact_names(pm, body)
  |> List.filter(~f=name =>
       String.equal(name, "case_eq") || String.is_prefix(name, ~prefix="ih")
     );

let split_marks = (pm, proof) =>
  ProofMap.marks_of(Proof.rep_id(fst(split(proof))), pm);

/* A false theorem with a wildcard case doesn't prove: the wildcard case's
 * steps are written for a value it doesn't have. */
let check_false_theorem = (src: string, ()) => {
  let (pm, proof) = run(src);
  check(bool, "the split is unmarked", true, split_marks(pm, proof) == []);
  check(
    bool,
    "not proven",
    false,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* The reported proof: the `_` case is an ordinary unfinished case whose
 * goal is the theorem's, with nothing about `xs`. */
let test_induction_wildcard_case_is_open = () => {
  let (pm, proof) =
    run(
      "theorem app_nil = forall xs: [Int] -> xs @ [] == xs proof induction xs | [] => from [] @ [] do step end; rewrite refl end | _ => ? end in ?",
    );
  let (_, bodies) = split(proof);
  let wild = last(bodies);
  check(bool, "the split is exhaustive", true, split_marks(pm, proof) == []);
  check(
    string,
    "the goal keeps the scrutinee",
    "xs@[]==xs",
    printed(incoming_of(pm, wild)),
  );
  check(list(string), "no inductive hypotheses", [], split_facts(pm, wild));
  check_mark_free("the [] case", pm, List.hd_exn(bodies));
  check(
    bool,
    "incomplete",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Incomplete,
  );
};

let test_cases_wildcard_proves = () => {
  let (pm, proof) =
    run(
      "theorem t = forall b: Bool -> b == b proof cases b | true => rewrite refl end | _ => rewrite refl end end in t",
    );
  let wild = last(snd(split(proof)));
  check(
    string,
    "the goal keeps the scrutinee",
    "b==b",
    printed(incoming_of(pm, wild)),
  );
  check(list(string), "no case equation", [], split_facts(pm, wild));
  check_mark_free("the proof", pm, proof);
  check_proven("the proof", pm, proof);
};

/* A lone wildcard covers a type with infinitely many values. */
let test_lone_wildcard_is_exhaustive = () => {
  let (pm, proof) =
    run(
      "theorem t = forall n: Int -> n + 0 == n proof induction n | _ => ? end in t",
    );
  check(bool, "the split is unmarked", true, split_marks(pm, proof) == []);
};

let tests = (
  "InductionWildcard",
  [
    test_case(
      "an induction's wildcard case is an open case with its goal",
      `Quick,
      test_induction_wildcard_case_is_open,
    ),
    test_case(
      "a cases split with a wildcard last case proves",
      `Quick,
      test_cases_wildcard_proves,
    ),
    test_case(
      "a lone wildcard case covers an Int",
      `Quick,
      test_lone_wildcard_is_exhaustive,
    ),
    test_case(
      "false theorem: wildcard after a case that checks",
      `Quick,
      check_false_theorem(
        {|theorem w = forall b: Bool -> b == true proof induction b | true => from true == true do rewrite refl end end | _ => from true == true do rewrite refl end end end in true|},
      ),
    ),
    test_case(
      "false theorem: wildcard in a cons head",
      `Quick,
      check_false_theorem(
        {|let first = fun xs -> case xs | [] => true | h :: t => h end in theorem w = forall xs: [Bool] -> first(xs) == true proof induction xs | [] => from first([]) do step end; from true == true do rewrite refl end end | _ :: t => from first(true :: t) do step end; from true == true do rewrite refl end end end in true|},
      ),
    ),
    test_case(
      "false theorem: wildcard nested in a cons head",
      `Quick,
      check_false_theorem(
        {|let first = fun xs -> case xs | [] => true | [] :: rest => true | (h :: t) :: rest => h end in theorem w = forall xs: [[Bool]] -> first(xs) == true proof induction xs | [] => from first([]) do step end; from true == true do rewrite refl end end | [] :: rest => from first([] :: rest) do step end; from true == true do rewrite refl end end | (_ :: t) :: rest => from first((true :: t) :: rest) do step end; from true == true do rewrite refl end end end in true|},
      ),
    ),
    test_case(
      "false theorem: wildcard in a constructor payload",
      `Quick,
      check_false_theorem(
        {|type N = +Z + S((Bool, Bool)) in let first = fun n -> case n | Z => true | S((a, y)) => a end in theorem w = forall n: N -> first(n) == true proof induction n | Z => from first(Z) do step end; from true == true do rewrite refl end end | S((_, _)) => from first(S((true, true))) do step end; from true == true do rewrite refl end end end in true|},
      ),
    ),
    test_case(
      "false theorem: wildcard in an ascribed tuple",
      `Quick,
      check_false_theorem(
        {|theorem w = forall p: (Bool, Bool) -> p == (true, true) proof induction p | ((_, _) : (Bool, Bool)) => from (true, true) == (true, true) do rewrite refl end end end in true|},
      ),
    ),
  ],
);
