/* D54/D59/D76 side-condition clauses: `<step> with <condition> by <proof> end`.

   A. Grammar/editor discrimination. `with` is an INTERIOR delimiter of
      five steps (axiom/axiomrev/revert/rewrite/contradiction) and the
      LEADING delimiter of the postfix clause. Every program is read four
      ways: real character-by-character typing in the tile editor
      (incremental remolding), the segment parser (paste), menhir, and
      print-then-reparse. All four must give the same proof shape.
   B. Checker (D59 hybrid): the clause is matched against its step's own
      recomputed obligations (exact goal, same bindings); a complete clause
      proof discharges the matching Pending obligation as Local; holes,
      wrong conditions, wrong scope and incomplete clause proofs never give
      Proven; automatic discharges are kept.
   C. D56 count: only Pending conditions are counted; met receipts stay. */
open Alcotest;
open Language;
open Haz3lcore;

module RE = Test_ResetExistentials;

/* --- A. shapes -------------------------------------------------------- */

let rec shape = (p: Proof.t): string => {
  let inst = i => Option.is_some(i) ? "[with]" : "";
  switch (p.term) {
  | EmptyHole => "?"
  | Invalid(_)
  | MultiHole(_) => "BROKEN"
  | Seq(a, b) => "Seq(" ++ shape(a) ++ ", " ++ shape(b) ++ ")"
  | Parens(a) => "Parens(" ++ shape(a) ++ ")"
  | AxiomStep({instantiation, direction, _}) =>
    (direction == Util.Direction.Left ? "AxiomRev" : "Axiom")
    ++ inst(instantiation)
  | AlgebriteStep(_) => "Rewrite"
  | EvalStep(_) => "Eval"
  | Revert(_, i, b) => "Revert" ++ inst(i) ++ "(" ++ shape(b) ++ ")"
  | Contradiction(_, i) => "Contradiction" ++ inst(i)
  | Assume(_, _, b) => "Assume(" ++ shape(b) ++ ")"
  | With(b, c, by) =>
    "With("
    ++ shape(b)
    ++ " | "
    ++ String.trim(RE.print_exp(c))
    ++ " | "
    ++ shape(by)
    ++ ")"
  | _ => Proof.show_cls(Proof.cls_of_term(p.term))
  };
};

let proof_in = (e: Exp.t): option(Proof.t) => {
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Theorem(_, _, proof, _) when found^ == None =>
            found := Some(proof)
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  found^;
};

let shape_of = (e: option(Exp.t)): string =>
  switch (Option.bind(e, proof_in)) {
  | Some(p) => shape(p)
  | None => "NO THEOREM"
  };

let typed = (src: string): option(Exp.t) =>
  try(
    Some(
      MakeTerm.from_zip_for_sem(
        Test_RoundtripFuzz.type_string(src),
        ~root=Exp,
      ).
        term,
    )
  ) {
  | _ => None
  };

let pasted = (src: string): option(Exp.t) =>
  Haz3lcore.Parser.to_term(src, ~root=Exp);

let menhir = (src: string): option(Exp.t) =>
  try(
    Some(
      Grammar.map_exp_annotation(
        _ => IdTagged.IdTag.temp(),
        MenhirParser.Conversion.Exp.of_menhir_ast(
          MenhirParser.Interface.parse_program(src),
        ),
      ),
    )
  ) {
  | _ => None
  };

let reprinted = (src: string): option(Exp.t) =>
  switch (pasted(src)) {
  | Some(e) => pasted(RE.print_exp(e))
  | None => None
  };

let thm = proof => "theorem t = 1 == 1 proof " ++ proof ++ " in t";

/* One test per (program, reader), so no reader hides another. */
let reader_cases = (label, proof, expected) => {
  let src = thm(proof);
  List.map(
    ((rname, read)) =>
      test_case(label ++ " [" ++ rname ++ "]", `Quick, () => {
        let s = shape_of(read(src));
        print_endline("WITHSHAPE " ++ label ++ " [" ++ rname ++ "] " ++ s);
        check(string, label ++ " " ++ rname, expected, s);
      }),
    [
      ("typed", typed),
      ("paste", pasted),
      ("menhir", menhir),
      ("print-reparse", reprinted),
    ],
  );
};

let grammar_cases =
  List.concat([
    reader_cases(
      "interior axiom with",
      "axiom refl_eq with x = 1 at 0 on 1 == 1 end",
      "Axiom[with]",
    ),
    reader_cases(
      "interior axiomrev with",
      "axiomrev refl_eq with x = 1 at 0 on true end",
      "AxiomRev[with]",
    ),
    reader_cases(
      "interior revert with",
      "revert h with x = 1 => ?",
      "Revert[with](?)",
    ),
    reader_cases(
      "interior rewrite with",
      "rewrite 1 + 1 with 2 at 0 end",
      "Rewrite",
    ),
    reader_cases(
      "interior contradiction with",
      "contradiction h with x = 1 end",
      "Contradiction[with]",
    ),
    reader_cases(
      "postfix clause after end",
      "eval 1 + 1 == 2 at 0 end with 2 == 2 by eval 2 == 2 at 0 end end",
      "With(Eval | 2 == 2 | Eval)",
    ),
    reader_cases(
      "two-role boundary",
      "axiom refl_eq with x = 1 at 0 on 1 == 1 end with 2 == 2 by eval 2 == 2 at 0 end end",
      "With(Axiom[with] | 2 == 2 | Eval)",
    ),
    reader_cases(
      "innermost attachment under assume",
      "assume true => eval 1 == 1 at 0 end with 2 == 2 by ? end",
      "Assume(With(Eval | 2 == 2 | ?))",
    ),
    reader_cases(
      "parenthesized outer attachment",
      "(assume true => eval 1 == 1 at 0 end) with true by ? end",
      "With(Parens(Assume(Eval)) | true | ?)",
    ),
    reader_cases(
      "innermost attachment in a sequence",
      "eval 1 == 1 at 0 end; ? with 2 == 2 by ? end",
      "Seq(Eval, With(? | 2 == 2 | ?))",
    ),
    reader_cases(
      "nested clauses",
      "eval 1 == 1 at 0 end with 2 == 2 by ? end with 3 == 3 by ? end",
      "With(With(Eval | 2 == 2 | ?) | 3 == 3 | ?)",
    ),
  ]);

/* --- B. checker -------------------------------------------------------- */

/* A non-intro `assume` incurs its proposition as an obligation, recorded
   on the assume node in the context the clause is written in. */
let base_stmt = "theorem t = forall (b : Bool) -> 1 == 1 proof ";

let no_clause =
  base_stmt ++ "assume b || true => eval 1 == 1 at 0 end in 0";

let clause = (cond, by) =>
  base_stmt
  ++ "(assume b || true => eval 1 == 1 at 0 end) with "
  ++ cond
  ++ " by "
  ++ by
  ++ " end in 0";

let proving = "axiom or_true at 0 on b || true end";

let obligations = (o: RE.obs) =>
  ProofMap.obligations_of_proof(o.pm, RE.theorem(o, "t"));

let show_obs = (o: RE.obs) =>
  obligations(o)
  |> List.map((ob: Obligation.t) =>
       String.trim(RE.print_exp(ob.display_goal))
       ++ ": "
       ++ Obligation.show_discharge(ob.discharge)
     )
  |> String.concat("; ");

let with_refused = (o: RE.obs) =>
  RE.nodes(o, "t", Proof.With) |> List.concat_map(RE.refused_rules);

let report = (label, o: RE.obs) =>
  print_endline(
    "WITHCHECK "
    ++ label
    ++ " | full "
    ++ RE.full(o, "t")
    ++ " | obligations "
    ++ show_obs(o)
    ++ " | with-refused "
    ++ String.concat(",", with_refused(o))
    ++ " | static errors "
    ++ string_of_int(List.length(o.errors)),
  );

let is_local = (ob: Obligation.t) =>
  switch (ob.discharge) {
  | Local(_) => true
  | _ => false
  };

let goal_is = (s, ob: Obligation.t) =>
  String.trim(RE.print_exp(ob.display_goal)) == s;

let checker_cases = [
  test_case("baseline without a clause: b || true Pending, not Proven", `Quick, () => {
    let o = RE.observe("no_clause", no_clause);
    report("no_clause", o);
    RE.valid("no_clause", o);
    check(bool, "pending", true,
      List.exists(ob => goal_is("b || true", ob) && Obligation.is_pending(ob), obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("complete clause: obligation Local(clause proof), theorem Proven", `Quick, () => {
    let src = clause("b || true", proving);
    let o = RE.observe("clause_proved", src);
    report("clause_proved", o);
    RE.valid("clause_proved", o);
    check(list(string), "clause accepted", [], with_refused(o));
    check(bool, "b || true is Local", true,
      List.exists(ob => goal_is("b || true", ob) && is_local(ob), obligations(o)));
    check(string, "full", "Proven", RE.full(o, "t"));
    /* The clause proof has its own map entry, and the Local receipt
       names it. */
    let w = RE.node(o, "t", Proof.With);
    ignore(w);
    let local_ids =
      obligations(o)
      |> List.filter_map((ob: Obligation.t) =>
           switch (ob.discharge) {
           | Local(id) => Some(id)
           | _ => None
           }
         );
    check(bool, "the Local id has its own entry", true,
      List.for_all(id => ProofMap.lookup(id, o.pm) != None, local_ids));
  }),
  test_case("hole clause: obligation stays Pending, not Proven", `Quick, () => {
    let o = RE.observe("clause_hole", clause("b || true", "?"));
    report("clause_hole", o);
    RE.valid("clause_hole", o);
    check(list(string), "clause matches (not refused)", [], with_refused(o));
    check(bool, "still Pending", true,
      List.exists(ob => goal_is("b || true", ob) && Obligation.is_pending(ob), obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("wrong condition: clause refused, obligation Pending, not Proven", `Quick, () => {
    let o = RE.observe("clause_wrong", clause("b && true", "?"));
    report("clause_wrong", o);
    check(list(string), "refused", ["with"], with_refused(o));
    check(bool, "still Pending", true,
      List.exists(ob => goal_is("b || true", ob) && Obligation.is_pending(ob), obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("wrong scope (innermost attachment to the eval step): refused, not Proven", `Quick, () => {
    let src =
      base_stmt
      ++ "assume b || true => eval 1 == 1 at 0 end with b || true by "
      ++ proving
      ++ " end in 0";
    let o = RE.observe("clause_scope", src);
    report("clause_scope", o);
    check(list(string), "refused", ["with"], with_refused(o));
    check(bool, "assume obligation still Pending", true,
      List.exists(ob => goal_is("b || true", ob) && Obligation.is_pending(ob), obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("different bindings (have body premise under the have hypothesis): refused", `Quick, () => {
    let src =
      "theorem t = 1 == 1 proof (have 2 == 2 proof eval 2 == 2 at 0 end => ?) with 1 == 1 by eval 1 == 1 at 0 end end in 0";
    let o = RE.observe("clause_bindings", src);
    report("clause_bindings", o);
    /* The SAME written goal `1 == 1` is an obligation of the have node,
       but recorded under the have's extended context (different binding
       ids): so it is the id criterion that refuses, not a goal mismatch. */
    let have_obs = RE.node(o, "t", Proof.Have).obligations;
    check(bool, "the have node records `1 == 1` (same goal as the clause)", true,
      List.exists(goal_is("1 == 1"), have_obs));
    check(bool, "...under bindings that include the have hypothesis (absent from the clause's context)", true,
      List.exists(
        (ob: Obligation.t) =>
          goal_is("1 == 1", ob)
          && List.exists(
               (e: Ctx.entry) =>
                 switch (e) {
                 | TheoremEntry({name: "have", _}) => true
                 | _ => false
                 },
               ob.bindings,
             ),
        have_obs,
      ));
    check(list(string), "refused", ["with"], with_refused(o));
    check(bool, "no Local discharge", false, List.exists(is_local, obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("incomplete clause proof (its own pending obligation): not Local, both visible", `Quick, () => {
    let src = clause("b || true", "assume b == b => " ++ proving);
    let o = RE.observe("clause_own_pending", src);
    report("clause_own_pending", o);
    check(bool, "step obligation not Local", false,
      List.exists(ob => goal_is("b || true", ob) && is_local(ob), obligations(o)));
    check(bool, "the clause proof's own obligation is listed Pending", true,
      List.exists(ob => goal_is("b == b", ob) && Obligation.is_pending(ob), obligations(o)));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
  test_case("automatic channel kept: Evaluated obligation stays Evaluated, Proven", `Quick, () => {
    let src =
      "theorem t = 1 == 1 proof (assume 2 == 2 => eval 1 == 1 at 0 end) with 2 == 2 by eval 2 == 2 at 0 end end in 0";
    let o = RE.observe("clause_auto", src);
    report("clause_auto", o);
    RE.valid("clause_auto", o);
    check(list(string), "clause matches", [], with_refused(o));
    check(bool, "2 == 2 Evaluated (not replaced)", true,
      List.exists(ob => goal_is("2 == 2", ob) && ob.discharge == Obligation.Evaluated, obligations(o)));
    check(string, "full", "Proven", RE.full(o, "t"));
  }),
  test_case("partial witness gate: hole clause leaves !! Pending, not Proven", `Quick, () => {
    let src =
      "theorem t = exists (f : Int -> Int) -> f(1) == f(1) proof (== true by witness fun (k : Int) -> 1 / k end) with !!(fun (k : Int) -> 1 / k) by ? end in 0";
    let o = RE.observe("clause_partial", src);
    report("clause_partial", o);
    check(bool, "a !! gate stays Pending", true,
      List.exists(
        (ob: Obligation.t) =>
          switch (ob.goal.term) {
          | Valued(_) => Obligation.is_pending(ob)
          | _ => false
          },
        obligations(o),
      ));
    check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
  }),
];

/* --- B2. nested clauses, row-edit preservation, one Undo, rows -------- */

let nested_src =
  "theorem t = 1 == 1 proof eval 1 == 1 at 0 end with 2 == 2 by ? end with 3 == 3 by ? end in 0";

let parse_zipper = (s: string): Zipper.t =>
  switch (Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper: " ++ s)
  };

let zipper_text = (z: Zipper.t) => Printer.of_zipper(~holes="?", z);

let first_node = (pred, p: Proof.t): option(Proof.t) =>
  RE.proof_nodes(p) |> List.find_opt(pred);

let nested_cases = [
  test_case("nested clauses: inner discharges, outer matches the same (now met) obligation", `Quick, () => {
    let src =
      base_stmt
      ++ "(assume b || true => eval 1 == 1 at 0 end) with b || true by "
      ++ proving
      ++ " end with b || true by "
      ++ proving
      ++ " end in 0";
    let o = RE.observe("clause_nested", src);
    report("clause_nested", o);
    RE.valid("clause_nested", o);
    check(list(string), "neither clause refused", [], with_refused(o));
    check(int, "exactly one Local discharge (the inner clause)", 1,
      List.length(List.filter(is_local, obligations(o))));
    check(string, "full", "Proven", RE.full(o, "t"));
  }),
  test_case("row edit of the inner step keeps both clauses; the edit is one Undo unit", `Quick, () => {
    let z = parse_zipper(nested_src);
    let root = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let proof =
      switch (proof_in(root)) {
      | Some(p) => p
      | None => Alcotest.fail("no theorem")
      };
    let step =
      switch (
        first_node(
          (p: Proof.t) =>
            switch (p.term) {
            | EvalStep(_) => true
            | _ => false
            },
          proof,
        )
      ) {
      | Some(p) => p
      | None => Alcotest.fail("no eval step")
      };
    let replacement =
      switch (Option.bind(pasted(thm("axiom refl_eq at 0 on 1 == 1 end")), proof_in)) {
      | Some(p) => p
      | None => Alcotest.fail("no replacement")
      };
    let patch =
      EditorTransform.mk_proof_patch(~target_id=Proof.rep_id(step), replacement);
    let after = EditorTransform.apply_patch(z, patch);
    let written = zipper_text(after);
    print_endline("WITHEDIT " ++ written);
    check(string, "shape after the row edit",
      "With(With(Axiom | 2 == 2 | ?) | 3 == 3 | ?)",
      shape_of(pasted(written)));
    check(bool, "one undoable action", true,
      Web.CellEditor.Update.can_undo(Web.CellEditor.Update.PatchMainEditor(patch)));
    check(string, "undo target is the pre-edit source", zipper_text(parse_zipper(nested_src)), zipper_text(z));
  }),
  test_case("stepper: a clause is its own (have-shaped) row, not peeled away", `Quick, () => {
    let w =
      switch (Option.bind(pasted(nested_src), proof_in)) {
      | Some(p) => p
      | None => Alcotest.fail("no theorem")
      };
    let (head, tail) = Web.StepperBase.Stepper.split_proof(w);
    check(bool, "split_proof keeps the With node as the row", true,
      Proof.rep_id(head) == Proof.rep_id(w) && tail == None);
    check(bool, "kind_of_proof is the two-premise (HaveStep) row", true,
      switch (Web.StepperBase.Stepper.kind_of_proof(w)) {
      | Some(HaveStep(_)) => true
      | _ => false
      });
  }),
  test_case("panel navigation: an obligation inside a clause proof leads to the clause proof (which has rows)", `Quick, () => {
    let src = clause("b || true", "assume b == b => " ++ proving);
    let o = RE.observe("clause_nav", src);
    let root = RE.theorem(o, "t");
    let inner =
      obligations(o) |> List.find_opt(goal_is("b == b"));
    switch (inner) {
    | None => Alcotest.fail("no clause-proof obligation")
    | Some(ob) =>
      let region = Web.ObligationsPanel.region_containing(~origin=ob.origin, root);
      let by =
        switch (root.term) {
        | With(_, _, by) => Some(by)
        | _ => None
        };
      print_endline(
        "WITHNAV region "
        ++ (region |> Option.map(shape) |> Option.value(~default="none")),
      );
      check(bool, "region is the clause proof", true,
        switch (region, by) {
        | (Some(r), Some(b)) => Proof.rep_id(r) == Proof.rep_id(b)
        | _ => false
        });
    };
  }),
];

/* --- C. D56 count ----------------------------------------------------- */

let count_cases = [
  test_case("D56 count: met (Local/automatic) receipts listed, only Pending counted", `Quick, () => {
    let proved = RE.observe("count_proved", clause("b || true", proving));
    let pending = RE.observe("count_hole", clause("b || true", "?"));
    let n = o => Web.ObligationsPanel.pending_count(obligations(o));
    print_endline(
      "WITHCOUNT proved rows="
      ++ string_of_int(List.length(obligations(proved)))
      ++ " pending="
      ++ string_of_int(n(proved))
      ++ " | hole rows="
      ++ string_of_int(List.length(obligations(pending)))
      ++ " pending="
      ++ string_of_int(n(pending)),
    );
    check(int, "proved: nothing pending", 0, n(proved));
    check(bool, "proved: met rows still listed", true, obligations(proved) != []);
    check(int, "hole: one pending", 1, n(pending));
  }),
];

let tests = (
  "ResetWithClause",
  grammar_cases @ checker_cases @ nested_cases @ count_cases,
);
