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

let parse_exp = Test_Evaluator_Prelude.parse_exp;

let statics_marks = (src): list(Mark.t) => {
  let (_, statics, _) = src |> parse_exp |> eval_with_proof;
  Id.Map.fold((_, info, acc) => Info.marks_of(info) @ acc, statics, []);
};

let tests = (
  "ProverKnownBugs",
  [
    Alcotest.test_case(
      "PROVER-STATICS-FOCUS-BINDERS: a focus body can't name a goal binder its selector doesn't",
      `Quick,
      () => {
        let src = "theorem t = (fun x: Int -> x + 0) == (fun x: Int -> x + (x - x)) proof from 0 do algebra x - x end end in t";
        let (pm, proof) = run_named("t", src);
        Alcotest.check(
          Alcotest.bool,
          "the checker binds `x` in the body",
          true,
          Option.is_none(find_marked_sub(pm, proof)),
        );
        Alcotest.check(
          Alcotest.bool,
          "PROVER-STATICS-FOCUS-BINDERS still behaves as filed",
          true,
          List.mem(statics_marks(src), Mark.Free("x"), ~equal=Poly.equal),
        );
      },
    ),
  ],
);
