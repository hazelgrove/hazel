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

/* `t` holds of this `f`, and not of `f_rebound`. */
let t_about_f = "let f = fun x: Int -> x + 1 in theorem t = f(1) == 2 proof from f(1) do step end; from 1 + 1 do step end; step in ";
let f_rebound = "let f = fun x: Int -> x + 5 in ";

/* The ways a step can cite `t`: by name, as a `have` premise it
 * discharges, through an alias, and spelled out. Each puts `f(1) == 2`
 * into the goal as an antecedent. */
let citations = [
  "revert t",
  "have f(1) == 2 proof ? => revert have",
  "alias h = t => revert h",
  "revert f(1) == 2",
];

/* Theorem `u = goal`, proved by citing `t` and evaluating the antecedent,
 * with `f(1)` stepping to `1 + k`. With `f` rebound the antecedent is
 * false, so this proves any goal. */
let citing_t = (~k, ~goal, citation) =>
  Printf.sprintf(
    "theorem u = %s proof %s => from f(1) do step end; from 1 + %d do step end; from %d == 2 do step end; step in u",
    goal,
    citation,
    k,
    1 + k,
  );

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
    test_case(
      "rebinding a variable a theorem names only in a let's definition captures it",
      `Quick,
      () =>
      check_scopes(
        "let x = 1 in theorem a = (let x = x in x) == 1 proof ? in let x = 2 in theorem c = true proof ? in 0",
        ~expected=[("a", []), ("c", [("a", true, false)])],
      )
    ),
    test_case(
      "a theorem is citable while its names keep their meaning", `Quick, () =>
      List.iter(
        citations,
        ~f=citation => {
          let (pm, proof) =
            run_named(
              "u",
              t_about_f ++ citing_t(~k=1, ~goal="true", citation),
            );
          check_proven(citation, pm, proof);
        },
      )
    ),
    test_case(
      "a theorem about a name the program has rebound can't be cited",
      `Quick,
      () =>
      List.iter(
        citations,
        ~f=citation => {
          let (pm, proof) =
            run_named(
              "u",
              t_about_f
              ++ f_rebound
              ++ citing_t(~k=5, ~goal="false", citation),
            );
          switch (ProofMap.full_status_of_proof(pm, proof)) {
          | Proven =>
            Alcotest.fail(
              citation ++ " proves `false` from `t`\n" ++ dump(pm, proof),
            )
          | ProvenModulo(_)
          | Refuted
          | Incomplete => ()
          };
        },
      )
    ),
    test_case(
      "citing a theorem about a rebound name is marked with that name",
      `Quick,
      () => {
        let captured =
          ProofMark.CitesCaptured({
            theorem: "t",
            rebound: ["f"],
          });
        List.iter(
          [
            "from f(1) do rewrite t end end",
            "revert t => ?",
            "alias h = t => ?",
            "revert f(1) == 2 => ?",
          ],
          ~f=proof => {
            let (pm, p) =
              run_named(
                "u",
                t_about_f
                ++ f_rebound
                ++ "theorem u = f(1) == 2 proof "
                ++ proof
                ++ " in u",
              );
            Alcotest.check(
              Alcotest.bool,
              proof ++ " is marked\n" ++ dump(pm, p),
              true,
              has_mark_kind(pm, p, Poly.equal(captured)),
            );
          },
        );
      },
    ),
    test_case(
      "the mark lists every rebound name the theorem mentions, and no other",
      `Quick,
      () => {
        let (pm, p) =
          run_named(
            "u",
            "let f = fun x: Int -> x in let g = fun x: Int -> x in let k = 0 in "
            ++ "theorem t = f(1) == g(1) proof ? in "
            ++ "let g = fun x: Int -> x + 1 in let k = 1 in let f = fun x: Int -> x + 2 in "
            ++ "theorem u = false proof revert t => ? in u",
          );
        Alcotest.check(
          Alcotest.bool,
          "marked\n" ++ dump(pm, p),
          true,
          has_mark_kind(
            pm,
            p,
            Poly.equal(
              ProofMark.CitesCaptured({
                theorem: "t",
                rebound: ["f", "g"],
              }),
            ),
          ),
        );
      },
    ),
    test_case(
      "the message names the theorem and the rebound variables",
      `Quick,
      () => {
        let globals = Web.Globals.Model.init();
        let message = rebound =>
          Web.ProofMarkView.message(
            ~globals,
            CitesCaptured({
              theorem: "t",
              rebound,
            }),
          )
          |> List.map(
               ~f=
                 fun
                 | Virtual_dom.Vdom.Node.Text(s) => s
                 | _ => "",
             )
          |> String.concat;
        Alcotest.check(
          Alcotest.string,
          "one variable",
          "`t` is about an earlier `f`; `f` has been rebound since `t` was stated.",
          message(["f"]),
        );
        Alcotest.check(
          Alcotest.string,
          "two variables",
          "`t` is about earlier `f`, `g`; they have been rebound since `t` was stated.",
          message(["f", "g"]),
        );
      },
    ),
  ],
);
