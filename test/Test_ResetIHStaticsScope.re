open Alcotest;
open Language;

/* R-reset-list-step-0116-5, statics side: the induction body makes the
 * checker's `ih` NAME visible iff the shared candidate helper
 * (InductiveHypotheses.get_inductive_hypotheses, aliased from ProofHacks
 * for the checker) yields at least one candidate for that case. Name only
 * (prop: None). Each program is statics-valid apart from the hypothesis
 * slot under test; ends `in 0`. Statics only, no evaluation. */

let observe = s => print_endline("OBSERVED " ++ s);

let statics_errors = Test_ResetListIHGesture.statics_errors;

let free_hyp = (label, src): bool => {
  let t0 = Sys.time();
  let (st, _) =
    Test_ProofMap.statics_and_elab(Test_Evaluator_Prelude.parse_exp(src));
  observe(Printf.sprintf("%s CHECKPOINT after statics_and_elab (cpu %.2fs)", label, Sys.time() -. t0));
  let errs = statics_errors(st);
  observe(Printf.sprintf("%s CHECKPOINT after error classification (cpu %.2fs)", label, Sys.time() -. t0));
  observe(label ++ " statics errors: [" ++ String.concat(", ", errs) ++ "]");
  List.mem("FreeHypothesis", errs);
};

let known = (label, src) =>
  check(bool, label ++ ": `ih` is a hypothesis in scope", false, free_hyp(label, src));

let unknown = (label, src) =>
  check(bool, label ++ ": `ih` stays out of scope", true, free_hyp(label, src));

/* App checker on `src`: for every proof node, its constructor, whether the
   app accessor has an incoming goal for it, and its mark names (no
   expression or Info is stringified). Returns the axiom-ih node's marks. */
let node_kind = (p: Proof.t): string =>
  switch (p.term) {
  | Induction(_) => "Induction"
  | AxiomStep(_) => "AxiomStep"
  | EvalStep(_) => "EvalStep"
  | Seq(_) => "Seq"
  | EmptyHole => "Hole"
  | _ => "other"
  };

let l1_trace = (label, src) => {
  let a = Test_ExistentialsAppContext.app_of_source(src);
  let nodes = Test_ResetExistentials.proof_nodes(a.proof);
  List.iter(
    (n: Proof.t) =>
      observe(
        Printf.sprintf(
          "%s node %s incoming=%s marks=[%s]",
          label,
          node_kind(n),
          switch (ProofMap.lookup(Proof.rep_id(n), a.accessor)) {
          | Some({incoming: Some(_), _}) => "present"
          | Some({incoming: None, _}) => "MISSING"
          | None => "no-entry"
          },
          String.concat(
            "; ",
            List.map(
              Test_ResetListIHGesture.mark_tag,
              ProofMap.marks_of(Proof.rep_id(n), a.accessor),
            ),
          ),
        ),
      ),
    nodes,
  );
  observe(label ++ " app status " ++ a.status);
  nodes
  |> List.filter(Test_ResetListIHGesture.is_ih_axiom)
  |> List.concat_map((n: Proof.t) =>
       List.map(
         Test_ResetListIHGesture.mark_tag,
         ProofMap.marks_of(Proof.rep_id(n), a.accessor),
       )
     );
};

let list_lemma = {|let len = fun (xs : [Int]) -> case xs | [] => 0 | _ :: t => 1 + len(t) end in
theorem t = forall xs : [Int] -> len(xs) == len(xs) proof induction xs
|}
;

let tests = (
  "ResetIHStaticsScope",
  [
    /* 0 */ test_case("positive: list h :: t", `Quick, () =>
      known("cons", list_lemma ++ {|| [] => ? | h :: t => axiom ih at 0 on len(t) == len(t) end end in 0|})
    ),
    /* 1 */ test_case("positive: _ :: t", `Quick, () =>
      known("wild-head", list_lemma ++ {|| [] => ? | _ :: t => axiom ih at 0 on len(t) == len(t) end end in 0|})
    ),
    /* 2 */ test_case("positive: Matt's shadowing x :: xs", `Quick, () =>
      known("shadow", list_lemma ++ {|| [] => ? | x :: xs => axiom ih at 0 on len(xs) == len(xs) end end in 0|})
    ),
    /* 3 */ test_case("positive: recursive ADT S(m)", `Quick, () =>
      known(
        "adt",
        {|type Nt = +Z+S(Nt) in
let pos = fun e -> case e | Z => true | S(b) => pos(b) end in
theorem t = forall e : Nt -> pos(e) == pos(e) proof induction e
| Z => ?
| S(m) => axiom ih at 0 on pos(m) == pos(m) end
end in 0|},
      )
    ),
    /* 4 */ test_case("negative: [] case", `Quick, () =>
      unknown("nil", list_lemma ++ {|| [] => axiom ih at 0 on len([]) end | h :: t => ? end in 0|})
    ),
    /* 5 */ test_case("negative: Bool true case", `Quick, () =>
      unknown(
        "bool",
        {|theorem t = forall b : Bool -> b == b proof induction b
| true => axiom ih at 0 on true end
| false => ?
end in 0|},
      )
    ),
    /* 6 */ test_case("negative: wildcard case", `Quick, () =>
      unknown("wild", list_lemma ++ {|| _ => axiom ih at 0 on len(xs) end end in 0|})
    ),
    /* 7 */ test_case("negative: bare variable case", `Quick, () =>
      unknown("barevar", list_lemma ++ {|| ys => axiom ih at 0 on len(ys) end end in 0|})
    ),
    /* 8 */ test_case("negative: nullary Z case", `Quick, () =>
      unknown(
        "nullary",
        {|type Nt = +Z+S(Nt) in
let pos = fun e -> case e | Z => true | S(b) => pos(b) end in
theorem t = forall e : Nt -> pos(e) == pos(e) proof induction e
| Z => axiom ih at 0 on pos(Z) end
| S(m) => ?
end in 0|},
      )
    ),
    /* 9 */ test_case("negative: non-recursive payload", `Quick, () =>
      unknown(
        "nonrec",
        {|type Box = +B(Int) in
let get = fun b -> case b | B(n) => n end in
theorem t = forall b : Box -> get(b) == get(b) proof induction b
| B(n) => axiom ih at 0 on get(b) end
end in 0|},
      )
    ),
    /* 10 */ test_case("negative: untyped scrutinee", `Quick, () =>
      unknown(
        "untyped",
        {|theorem t = forall q -> q == q proof induction q
| h :: t => axiom ih at 0 on q end
end in 0|},
      )
    ),
    /* 11 */ test_case("nested: inner Bool induction inside a cons case sees the outer ih", `Quick, () =>
      known(
        "nested",
        list_lemma
        ++ {|| [] => ? | h :: t => induction h == 0
  | true => axiom ih at 0 on len(t) == len(t) end
  | false => ?
  end
end in 0|},
      )
    ),
    /* 12 */ test_case("generalized: ih name visible after generalize", `Quick, () =>
      known(
        "generalized",
        {|let len = fun (xs : [Int]) -> case xs | [] => 0 | _ :: t => 1 + len(t) end in
theorem t = forall xs : [Int] -> forall k : Int -> len(xs) + k == k + len(xs) proof generalize k => induction xs
| [] => ?
| h :: t => axiom ih at 0 on len(t) end
end in 0|},
      )
    ),
    /* 13 (L1, fail-closed). By the time the induction runs, the leading
       foralls are already peeled, so `h`, `xs` and `t` are free Vars of the
       goal. The case goal is `replace_exp(goal, xs := h :: t)` with the
       pattern's variables [h; t] as the blacklist, and the `Var(x) when
       List.mem(x, blacklist_vars)` arm raises BlacklistVarFound on the
       goal's outer `h`/`t`: a syntactic name clash with the pattern, not
       capture under an inner binder. The case goal is refused, so the
       expected verdict is MissingIncoming on axiom ih (the Induction and []
       nodes keep their incoming). This pins only this clash. Source reading
       suggests statics and the checker can still disagree on fresh goal
       nodes or an incomplete co_ctx (where `uses_blacklist_var` sees an
       empty co_ctx); that path also fails closed but is NOT exercised here,
       so this is not a claim that every capture is closed. (The first L1
       fixture had no clashing names; its MissingIncoming was not this
       mechanism.) */
    test_case("fail-closed: pattern-name clash drops the case goal and ih; statics shows the name", `Quick, () => {
      let src = {|let len = fun (xs : [Int]) -> case xs | [] => 0 | _ :: t => 1 + len(t) end in
theorem th = forall h : Int -> forall xs : [Int] -> forall t : Int -> len(xs) + h + t == t + h + len(xs) proof induction xs
| [] => ?
| h :: t => axiom ih at 0 on len(t) end
end in 0|};
      check(bool, "L1: statics shows the ih name", false, free_hyp("L1-capture", src));
      let tags = l1_trace("L1-capture", src);
      check(bool, "L1: axiom ih is marked MissingIncoming (case goal refused by the blacklist clash)", true, List.mem("MissingIncoming", tags));
    }),
    /* 13b twin: same shape, outer variables renamed `k`/`u`, so nothing in
       the goal clashes with `h :: t`. The case goal forms and the checker
       resolves `ih`: no MissingIncoming and no UnknownEquality(ih). (Raw
       receipt a5: the remaining mark is PatternNotFound; `len(t)` does not
       occur literally in the case goal `len(h :: t) + k + u == ...`.) */
    test_case("fail-closed twin: no name clash, the checker resolves ih", `Quick, () => {
      let src = {|let len = fun (xs : [Int]) -> case xs | [] => 0 | _ :: t => 1 + len(t) end in
theorem th = forall k : Int -> forall xs : [Int] -> forall u : Int -> len(xs) + k + u == u + k + len(xs) proof induction xs
| [] => ?
| h :: t => axiom ih at 0 on len(t) end
end in 0|};
      check(bool, "L1-twin: statics shows the ih name", false, free_hyp("L1-twin", src));
      let tags = l1_trace("L1-twin", src);
      check(bool, "L1-twin: no MissingIncoming on axiom ih", false, List.mem("MissingIncoming", tags));
      check(bool, "L1-twin: no UnknownEquality(ih) on axiom ih", false, List.mem("UnknownEquality(ih)", tags));
    }),
    /* 15 */ test_case("helper parity: the checker's alias is the moved function", `Quick, () =>
      check(
        bool,
        "ProofHacks.get_inductive_hypotheses == InductiveHypotheses.get_inductive_hypotheses",
        true,
        ProofHacks.get_inductive_hypotheses
        === InductiveHypotheses.get_inductive_hypotheses,
      )
    ),
  ],
);
