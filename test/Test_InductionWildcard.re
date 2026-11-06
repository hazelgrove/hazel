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
        {|theorem w = forall b: Bool -> b == true proof induction b | true => axiom refl_eq at 0 on true == true end | _ => axiom refl_eq at 0 on true == true end end in true|},
      ),
    ),
    test_case(
      "in a cons head",
      `Quick,
      check_refused(
        {|let first = fun xs -> case xs | [] => true | h :: t => h end in theorem w = forall xs: [Bool] -> first(xs) == true proof induction xs | [] => eval first([]) at 0 end; axiom refl_eq at 0 on true == true end | _ :: t => eval first(true :: t) at 0 end; axiom refl_eq at 0 on true == true end end in true|},
      ),
    ),
    test_case(
      "nested in a cons head",
      `Quick,
      check_refused(
        {|let first = fun xs -> case xs | [] => true | [] :: rest => true | (h :: t) :: rest => h end in theorem w = forall xs: [[Bool]] -> first(xs) == true proof induction xs | [] => eval first([]) at 0 end; axiom refl_eq at 0 on true == true end | [] :: rest => eval first([] :: rest) at 0 end; axiom refl_eq at 0 on true == true end | (_ :: t) :: rest => eval first((true :: t) :: rest) at 0 end; axiom refl_eq at 0 on true == true end end in true|},
      ),
    ),
    test_case(
      "in a constructor payload",
      `Quick,
      check_refused(
        {|type N = +Z + S((Bool, Bool)) in let first = fun n -> case n | Z => true | S((a, y)) => a end in theorem w = forall n: N -> first(n) == true proof induction n | Z => eval first(Z) at 0 end; axiom refl_eq at 0 on true == true end | S((_, _)) => eval first(S((true, true))) at 0 end; axiom refl_eq at 0 on true == true end end in true|},
      ),
    ),
    test_case(
      "in an ascribed tuple",
      `Quick,
      check_refused(
        {|theorem w = forall p: (Bool, Bool) -> p == (true, true) proof induction p | ((_, _) : (Bool, Bool)) => axiom refl_eq at 0 on (true, true) == (true, true) end end in true|},
      ),
    ),
    test_case(
      "under a where guard",
      `Quick,
      check_refused(
        {|theorem w = forall b: Bool -> b == true proof induction b | _ where true => axiom refl_eq at 0 on true == true end | x => ? end in true|},
      ),
    ),
  ],
);
