open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* What a proof can establish and use about `!!`, "is standard". */

let proven = (src: string): bool => {
  let (pm, proof) = run_named("t", src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | Proven => true
  | ProvenModulo(_)
  | Refuted
  | Incomplete => false
  };
};

let named_binder_facts = [
  test_case("forall (x : T !! h) names the fact !!x", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall (x : Bool !! h) => rewrite h end in t",
      ),
    )
  ),
  test_case("a function binder's fact says it is total", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall f : (Int -> Int) -> !!f proof generalize f => forall (f : (Int -> Int) !! hf) => rewrite hf end in t",
      ),
    )
  ),
  test_case("the fact follows a renamed binder", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall (y !! h) => rewrite h end in t",
      ),
    )
  ),
  test_case("without a name, the forall step gives no fact", `Quick, () =>
    check(
      bool,
      "not proven",
      false,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall x => ? in t",
      ),
    )
  ),
];

let tests = ("Evaluator.Standardness", named_binder_facts);
