open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* The theorem namespace a proof is checked in is built during evaluation.
 * Theorem scoping is lexical, so at each theorem it has to be the
 * namespace the statics give the proof: the same theorems, shadowed and
 * captured the same way. */

/* Each theorem's proof root, with the citable facts in its scope as
 * (name, captured, shadowed): as the checker scoped it, and as the
 * statics did. The builtin axioms are left out. */
let scopes = (src: string) => {
  let (state, statics, elab) = src |> parse_exp |> eval_with_proof;
  let proof_map = EvaluatorState.get_proof_map(state);
  let facts = (~captured=[], ctx) =>
    ProofCtx.of_theorem_ctx(~builtins=[], ~captured, ctx)
    |> List.map(~f=(e: ProofCtx.entry) =>
         (e.name, e.is_captured, e.is_shadowed)
       );
  let proofs = ref([]);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Theorem({term: Var(name), _}, _, proof, _) =>
            proofs := [(name, Proof.rep_id(proof)), ...proofs^]
          | _ => ()
          };
          continue(e);
        },
      elab,
    );
  List.rev_map(
    ~f=
      ((name, id)) => {
        let checked =
          ProofMap.lookup(id, proof_map)
          |> Option.map(~f=(entry: ProofMap.entry) =>
               facts(
                 ~captured=entry.ctx.captured,
                 SemanticCtx.get_ctx(entry.ctx),
               )
             );
        let static =
          Statics.Map.lookup(id, statics)
          |> Option.map(~f=info => facts(Info.ctx_of(info)));
        (name, checked, static);
      },
    proofs^,
  );
};

let fact = triple(string, bool, bool);

let check_scopes =
    (src, ~expected: list((string, list((string, bool, bool))))) => {
  let sorted = l => List.sort(~compare=Poly.compare, l);
  let scopes = scopes(src);
  let as_scoped_by = side =>
    List.map(
      ~f=((name, checked, static)) => (name, side(checked, static)),
      scopes,
    )
    |> sorted;
  let expected =
    List.map(~f=((name, facts)) => (name, Some(facts)), expected)
    |> sorted;
  let scope = list(pair(string, option(list(fact))));
  check(
    scope,
    "as the statics scope them",
    expected,
    as_scoped_by((_, static) => static),
  );
  check(
    scope,
    "as the checker scopes them",
    expected,
    as_scoped_by((checked, _) => checked),
  );
};

let tests = (
  "TheoremNamespace",
  [
    test_case("later theorems see earlier ones, innermost first", `Quick, () =>
      check_scopes(
        "let x = 1 in theorem a = x == 1 proof ? in theorem b = x + 0 == 1 proof ? in theorem c = true proof ? in 0",
        ~expected=[
          ("a", []),
          ("b", [("a", false, false)]),
          ("c", [("b", false, false), ("a", false, false)]),
        ],
      )
    ),
    test_case("a theorem in a definition is out of scope after it", `Quick, () =>
      check_scopes(
        "let y = (theorem a = 1 == 1 proof ? in 5) in theorem b = true proof ? in y",
        ~expected=[("a", []), ("b", [])],
      )
    ),
    test_case("a theorem of the same name shadows", `Quick, () =>
      check_scopes(
        "theorem a = 1 == 1 proof ? in theorem a = 2 == 2 proof ? in theorem c = true proof ? in 0",
        ~expected=[
          ("a", []),
          ("a", [("a", false, false)]),
          ("c", [("a", false, false), ("a", false, true)]),
        ],
      )
    ),
    test_case("rebinding a variable a theorem names captures it", `Quick, () =>
      check_scopes(
        "let x = 1 in let z = 0 in theorem a = x == 1 proof ? in let z = 3 in theorem b = true proof ? in let x = 2 in theorem c = true proof ? in 0",
        ~expected=[
          ("a", []),
          ("b", [("a", false, false)]),
          ("c", [("b", false, false), ("a", true, false)]),
        ],
      )
    ),
  ],
);
