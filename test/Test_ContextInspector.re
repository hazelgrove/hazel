open Alcotest;
open Language;

/* How many built-in axioms the context inspector lists with the cursor on
   a term of `src` that `at` picks. */
let axioms_listed = (src: string, ~at: Info.t => bool): int => {
  let (state, statics, _) =
    Test_ProofMap.Harness.eval_with_proof(
      Test_Evaluator_Prelude.parse_exp(src),
    );
  let ci =
    switch (List.find(~f=at, Id.Map.bindings(statics) |> List.map(~f=snd))) {
    | Some(ci) => ci
    | None => fail("no term at the cursor")
    };
  let axioms =
    List.map(~f=(a: Ctx.theorem_entry) => a.name, Statics.initial_theorems);
  Web.ContextInspector.entries(
    ~axioms=
      Web.ContextInspector.lists_axioms(
        ~proof_map=EvaluatorState.get_proof_map(state),
        ci,
      ),
    Info.ctx_of(ci),
  )
  |> List.count(
       ~f=
         fun
         | Ctx.TheoremEntry({name, _}) =>
           List.mem(axioms, name, ~equal=String.equal)
         | _ => false,
     );
};

let var = (x, ci: Info.t) =>
  switch (ci) {
  | InfoExp({user_term: {term: Var(y), _}, _}) => String.equal(x, y)
  | _ => false
  };

let proof_step: Info.t => bool =
  fun
  | InfoProof(_) => true
  | _ => false;

let all = List.length(Statics.initial_theorems);

let cited = "let x = 1 in theorem t = x == x proof rewrite refl in 0";

let tests = (
  "ContextInspector",
  [
    test_case("a program without proofs lists no axioms", `Quick, () =>
      check(
        int,
        "axioms listed",
        0,
        axioms_listed("let x = 1 in x", ~at=var("x")),
      )
    ),
    test_case("a proof step lists every axiom", `Quick, () =>
      check(int, "axioms listed", all, axioms_listed(cited, ~at=proof_step))
    ),
    test_case("a theorem's statement lists no axioms", `Quick, () =>
      check(int, "axioms listed", 0, axioms_listed(cited, ~at=var("x")))
    ),
    test_case("the fact a proof cites lists every axiom", `Quick, () =>
      check(
        int,
        "axioms listed",
        all,
        axioms_listed(cited, ~at=var("refl")),
      )
    ),
  ],
);
