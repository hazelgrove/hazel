open Language;
open Test_ProofMap.Harness;

/* Each case pins a bug as it behaves today, named by its id in the
 * prover's bug list. When a fix changes the behaviour the case fails:
 * move its program to the regression tests of the area it belongs to,
 * asserting the right verdict, and mark the bug fixed. */

let status_of = (name, src): string => {
  let (pm, proof) = run_named(name, src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | Proven => "Proven"
  | ProvenModulo(_) => "ProvenModulo"
  | Refuted => "Refuted"
  | Incomplete => "Incomplete"
  };
};

let pinned = (id, what, ~theorem="bad", ~now, src) =>
  Alcotest.test_case(id ++ ": " ++ what, `Quick, () =>
    Alcotest.check(
      Alcotest.string,
      id ++ " still behaves as filed",
      now,
      status_of(theorem, src),
    )
  );

let tests = (
  "ProverKnownBugs",
  [
    pinned(
      "PROVER-IND-COMPUTED",
      "induction on a computed scrutinee gets inductive hypotheses, so false is proven",
      ~now="Proven",
      "theorem bad = false proof induction [1] | [] => contradiction case_eq end | h :: t => contradiction ih end end in bad",
    ),
    pinned(
      "PROVER-IND-CONTEXT-FACTS",
      "a case keeps facts about the scrutinee next to an unconditional ih",
      ~now="Proven",
      "theorem bad = forall xs: [Int] where xs == [1] -> false proof induction xs | [] => contradiction where with xs = [] end | h :: t => contradiction ih end end in bad",
    ),
    pinned(
      "PROVER-SHADOWED-FACT",
      "a fact about an outer x discharges a goal about a new x",
      ~now="Proven",
      "theorem bad = forall x: Int -> x == 0 ==> (forall x: Int -> x == 0) proof assume x == 0 => forall x => assume x == 0 as h => from x do axiom h end end; eval in bad",
    ),
    pinned(
      "PROVER-FUNEXT-CAPTURE",
      "a funext binder free in the functions is captured",
      ~now="Proven",
      "theorem bad = forall y: Int -> (fun z: Int -> y) == (fun z: Int -> z) proof funext y => axiom refl_eq end in bad",
    ),
    pinned(
      "PROVER-REWRITE-UNCHECKED",
      "the checker accepts any rewrite replacement",
      ~now="Proven",
      "theorem bad = 1 == 2 proof rewrite true end in bad",
    ),
    pinned(
      "PROVER-ASSUME-REFUTED",
      "assume reports its body's result as the implication's, so a true implication reads Refuted",
      ~theorem="t",
      ~now="Refuted",
      "theorem t = 1 == 2 ==> false proof assume 1 == 2 => ? in t",
    ),
    pinned(
      "PROVER-ASSUME-IN-FOCUS",
      "assume inside a focus replaces A ==> B by something not equal to it",
      ~now="Proven",
      "theorem bad = forall x: Int -> (!(x == 0) || x == 1) == false proof from !(x == 0) || x == 1 do axiomrev impl_def end; assume x == 0 as h => from x do axiom h end end; eval end; eval in bad",
    ),
    pinned(
      "PROVER-FLOAT-REFL",
      "refl_eq holds for every Float, though nan == nan is false",
      ~now="Proven",
      "theorem refl_f = forall x: Float -> x == x proof axiom refl_eq end in theorem bad = false proof revert refl_f with x = nan => from $e == $e do eval end; eval in bad",
    ),
    pinned(
      "MAIN-CITATION-STATUS",
      "a theorem citing an unproven lemma reads plain Proven",
      ~now="Proven",
      "theorem lemma = 1 == 2 proof ? in theorem bad = 1 == 2 proof from 1 do axiom lemma end end; eval in bad",
    ),
    Alcotest.test_case(
      "PROVER-ASSUME-PARENS: assume doesn't see an implication through parens",
      `Quick,
      () => {
        let (pm, proof) =
          run_named(
            "t",
            "theorem t = forall x: Int -> x == x ==> (forall y: Int -> (y == x ==> y == x)) proof assume x == x => forall y => assume y == x => ? in t",
          );
        Alcotest.check(
          Alcotest.int,
          "PROVER-ASSUME-PARENS still behaves as filed",
          1,
          List.length(
            List.filter(
              Obligation.is_pending,
              ProofMap.obligations_of_proof(pm, proof),
            ),
          ),
        );
      },
    ),
    Alcotest.test_case(
      "PROVER-NAT-NO-IH: a user type named Nat gets no inductive hypothesis",
      `Quick,
      () => {
        let proof_of = ty =>
          "type "
          ++ ty
          ++ " = +Z+S("
          ++ ty
          ++ ") in let add = fun (a, b) -> case a | Z => b | S(p) => S(add((p, b))) end in "
          ++ "theorem t = forall n: "
          ++ ty
          ++ " -> add((n, Z)) == n proof induction n "
          ++ "| Z => from add((Z, Z)) do eval end; axiom refl_eq end "
          ++ "| S(p) => from add((S(p), Z)) do eval end; from add((p, Z)) do axiom ih end end; axiom refl_eq end end in t";
        Alcotest.check(
          Alcotest.string,
          "the same proof over Nt",
          "Proven",
          status_of("t", proof_of("Nt")),
        );
        Alcotest.check(
          Alcotest.string,
          "PROVER-NAT-NO-IH still behaves as filed",
          "Incomplete",
          status_of("t", proof_of("Nat")),
        );
      },
    ),
  ],
);
