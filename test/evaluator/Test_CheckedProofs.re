open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;
open Poly;

/* Reusing a checked proof across evaluations. A theorem's proof is
 * reused unless the theorem, the provenance of a variable it names, or the
 * theorems in scope change; a change to its body, which holds every later
 * theorem, doesn't recheck it.
 *
 * Edits keep ids, as the editor's do, so the second evaluation finds the
 * first's entries (see Test_Evaluator_Incremental). */

let eval = (~prev=IncrEval.empty, exp: Exp.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=CoreSettings.on.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let (_, state) =
    Evaluator.evaluate(~prev, ~eval_info, ~env=Builtins.env_init, elab);
  (state, elab);
};

/* `exp` with the int literal `from` replaced by `to_` in theorem `name`'s
 * statement and proof only, keeping every id. */
let edit_theorem = (name: string, ~from: int, ~to_: int, exp: Exp.t): Exp.t => {
  let replace_int =
    Exp.map_term(~f_exp=(continue, e: Exp.t) =>
      switch (e.term) {
      | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) => {
          ...e,
          term: Atom(Int(Bigint.of_int(to_))),
        }
      | _ => continue(e)
      }
    );
  Exp.map_term(
    ~f_exp=
      (continue, e: Exp.t) =>
        switch (e.term) {
        | Theorem({term: Var(x), _} as p, goal, proof, body) when x == name =>
          let edited =
            replace_int({
              ...e,
              term: Theorem(p, goal, proof, Exp.temp(EmptyHole)),
            });
          switch (edited.term) {
          | Theorem(p, goal, proof, _) => {
              ...e,
              term: Theorem(p, goal, proof, continue(body)),
            }
          | _ => continue(e)
          };
        | _ => continue(e)
        },
    exp,
  );
};

/* The proof map entry of theorem `name`'s proof. */
let root = (name, (state, elab)) =>
  switch (
    ProofMap.lookup(
      Proof.rep_id(proof_of_named(name, elab)),
      EvaluatorState.get_proof_map(state),
    )
  ) {
  | Some(entry) => entry
  | None => fail("no proof map entry for " ++ name)
  };

let reused = (name, ~before, ~after) =>
  phys_equal(root(name, before), root(name, after));

/* How many steps of theorem `name`'s proof are marked. */
let marks = (name, (state, elab)) => {
  let pm = EvaluatorState.get_proof_map(state);
  let rec count = (p: Proof.t) =>
    (ProofMap.marks_of(Proof.rep_id(p), pm) == [] ? 0 : 1)
    + List.fold_left(~f=(n, c) => n + count(c), ~init=0, Proof.children(p));
  count(proof_of_named(name, elab));
};

let test_unchanged_is_reused = () => {
  let exp =
    parse_exp(
      "theorem a = 1 + 1 == 2 proof step in theorem b = 2 + 2 == 4 proof step in 0",
    );
  let before = eval(exp);
  let after = eval(~prev=fst(before).incr_eval, exp);
  check(bool, "a is reused", true, reused("a", ~before, ~after));
  check(bool, "b is reused", true, reused("b", ~before, ~after));
};

let test_body_change_keeps_earlier_theorem = () => {
  let exp =
    parse_exp(
      "theorem a = 1 + 1 == 2 proof step in theorem b = 2 + 2 == 4 proof step in 0",
    );
  let before = eval(exp);
  let after =
    eval(
      ~prev=fst(before).incr_eval,
      edit_theorem("b", ~from=4, ~to_=5, exp),
    );
  check(
    bool,
    "a, whose body changed, is reused",
    true,
    reused("a", ~before, ~after),
  );
  check(bool, "b is checked again", false, reused("b", ~before, ~after));
};

let discharges = (name, (state, elab)) =>
  ProofMap.obligations_of_proof(
    EvaluatorState.get_proof_map(state),
    proof_of_named(name, elab),
  )
  |> List.map(~f=(ob: Obligation.t) =>
       Obligation.show_discharge(ob.discharge)
     );

/* `k` is named only in the proof. */
let test_definition_named_in_proof = () => {
  let exp =
    parse_exp(
      "let k = 3 in theorem a = 2 == 2 proof have k == 1 proof ? => ? in 0",
    );
  let discharge = discharges("a");
  let before = eval(exp);
  check(
    list(string),
    "k == 1 doesn't hold",
    ["Pending", "Pending"],
    discharge(before),
  );
  let after =
    eval(
      ~prev=fst(before).incr_eval,
      Test_Evaluator_Incremental.replace_int_lit(~from=3, ~to_=1, exp),
    );
  check(
    list(string),
    "k == 1 holds",
    ["Evaluated", "Pending"],
    discharge(after),
  );
};

/* `b` is inside `a`'s node, which evaluation reuses if nothing it depends
 * on changed, `b`'s proof included. */
let test_definition_named_in_nested_proof = () => {
  let exp =
    parse_exp(
      "let k = 3 in theorem a = 2 == 2 proof ? in theorem b = 2 == 2 proof have k == 1 proof ? => ? in 0",
    );
  let before = eval(exp);
  check(
    list(string),
    "k == 1 doesn't hold",
    ["Pending", "Pending"],
    discharges("b", before),
  );
  let after =
    eval(
      ~prev=fst(before).incr_eval,
      Test_Evaluator_Incremental.replace_int_lit(~from=3, ~to_=1, exp),
    );
  check(
    list(string),
    "k == 1 holds",
    ["Evaluated", "Pending"],
    discharges("b", after),
  );
};

/* `b` cites `a`, which isn't a variable, so no co-ctx has it. */
let test_cited_theorem_changes = () => {
  let exp =
    parse_exp(
      "theorem a = 40 + 2 == 42 proof ? in theorem b = 40 + 2 == 42 proof from 40 + 2 do rewrite a end end in 0",
    );
  let before = eval(exp);
  check(int, "b's rewrite applies", 0, marks("b", before));
  let after =
    eval(
      ~prev=fst(before).incr_eval,
      edit_theorem("a", ~from=2, ~to_=3, exp),
    );
  check(int, "b's rewrite no longer applies", 1, marks("b", after));
};

/* `exp` with theorem `name`'s proof a hole, keeping every id. */
let drop_proof = (name: string, exp: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e: Exp.t) =>
        switch (e.term) {
        | Theorem({term: Var(x), _} as p, goal, proof, body) when x == name => {
            ...e,
            term:
              Theorem(
                p,
                goal,
                {
                  ...proof,
                  term: EmptyHole,
                },
                continue(body),
              ),
          }
        | _ => continue(e)
        },
    exp,
  );

/* Citing an unfinished theorem incurs its statement as an obligation, so a
 * proof is checked again when the verdict of a theorem it cites changes. */
let test_cited_verdict_changes = () => {
  let exp =
    parse_exp(
      "theorem a = 1 == 1 proof rewrite refl end in theorem b = 1 == 1 proof from 1 do rewrite a end end; rewrite refl end in 0",
    );
  let before = eval(exp);
  check(list(string), "a is proven", [], discharges("b", before));
  let after = eval(~prev=fst(before).incr_eval, drop_proof("a", exp));
  check(
    list(string),
    "a is unfinished, so citing it incurs its statement",
    ["Evaluated"],
    discharges("b", after),
  );
};

let tests = (
  "CheckedProofs",
  [
    test_case(
      "an unchanged theorem's proof is reused",
      `Quick,
      test_unchanged_is_reused,
    ),
    test_case(
      "a change in a theorem's body doesn't recheck it",
      `Quick,
      test_body_change_keeps_earlier_theorem,
    ),
    test_case(
      "a proof is rechecked when a definition only it names changes",
      `Quick,
      test_definition_named_in_proof,
    ),
    test_case(
      "a proof inside a reused node is rechecked when a definition only it names changes",
      `Quick,
      test_definition_named_in_nested_proof,
    ),
    test_case(
      "a proof is rechecked when a theorem it cites changes",
      `Quick,
      test_cited_theorem_changes,
    ),
    test_case(
      "a proof is rechecked when the verdict of a theorem it cites changes",
      `Quick,
      test_cited_verdict_changes,
    ),
  ],
);
