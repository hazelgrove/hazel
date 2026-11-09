open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* A wildcard anywhere in a case pattern refuses the whole split before
 * any case is checked. */
let check_refused = (src: string, ()) => {
  let (state, _, elab) = src |> parse_exp |> Test_ProofMap.eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let entry = Test_ProofMap.proof_entry(state, elab);
  check(
    bool,
    "only the wildcard mark",
    true,
    entry.marks == [ProofMark.InductionUnsupportedWildcard],
  );
  check(bool, "goal passes through", true, entry.incoming == entry.outgoing);
  check(
    bool,
    "not proven",
    true,
    ProofMap.full_status_of_proof(pm, Test_ProofMap.proof_of(elab))
    == ProofMap.Incomplete,
  );
  check(int, "no case is checked", 1, Id.Map.cardinal(pm));
};

let tests = (
  "InductionWildcard",
  [
    test_case(
      "after a case that checks",
      `Quick,
      check_refused(
        {|theorem w = forall b: Bool -> b == true proof induction b | true => from true == true do axiom refl_eq end end | _ => from true == true do axiom refl_eq end end end in true|},
      ),
    ),
    test_case(
      "in a cons head",
      `Quick,
      check_refused(
        {|let first = fun xs -> case xs | [] => true | h :: t => h end in theorem w = forall xs: [Bool] -> first(xs) == true proof induction xs | [] => from first([]) do eval end; from true == true do axiom refl_eq end end | _ :: t => from first(true :: t) do eval end; from true == true do axiom refl_eq end end end in true|},
      ),
    ),
    test_case(
      "nested in a cons head",
      `Quick,
      check_refused(
        {|let first = fun xs -> case xs | [] => true | [] :: rest => true | (h :: t) :: rest => h end in theorem w = forall xs: [[Bool]] -> first(xs) == true proof induction xs | [] => from first([]) do eval end; from true == true do axiom refl_eq end end | [] :: rest => from first([] :: rest) do eval end; from true == true do axiom refl_eq end end | (_ :: t) :: rest => from first((true :: t) :: rest) do eval end; from true == true do axiom refl_eq end end end in true|},
      ),
    ),
    test_case(
      "in a constructor payload",
      `Quick,
      check_refused(
        {|type N = +Z + S((Bool, Bool)) in let first = fun n -> case n | Z => true | S((a, y)) => a end in theorem w = forall n: N -> first(n) == true proof induction n | Z => from first(Z) do eval end; from true == true do axiom refl_eq end end | S((_, _)) => from first(S((true, true))) do eval end; from true == true do axiom refl_eq end end end in true|},
      ),
    ),
    test_case(
      "in an ascribed tuple",
      `Quick,
      check_refused(
        {|theorem w = forall p: (Bool, Bool) -> p == (true, true) proof induction p | ((_, _) : (Bool, Bool)) => from (true, true) == (true, true) do axiom refl_eq end end end in true|},
      ),
    ),
    test_case(
      "under a where guard",
      `Quick,
      check_refused(
        {|theorem w = forall b: Bool -> b == true proof induction b | _ where true => from true == true do axiom refl_eq end end | x => ? end in true|},
      ),
    ),
  ],
);
