/* Reset-existentials-implementation, FR-exists-1: the step picker's
   Witness button and inline editor.

   Native checks of the UI's own logic (no browser):
   - the button opens a BLANK editor and toggles closed;
   - typing actions are not undo units; the commit is one PatchMainEditor
     (one Undo);
   - the commit writes exactly `== true by witness <v> end` over the hole,
     the source reads back, and the checker presents the instantiated
     body as the next goal;
   - the pre-commit verdict is honest: wrong type, static error, empty,
     untyped binder and a stale (non-exists) goal offer no commit. */
open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Haz3lcore;

module MS = Web.MissingStep;
module SB = Web.StepperBase;
module RE = Test_ResetExistentials;

let sq9 = "exists (n : Int) -> n * n == 9";
let hole_src = "theorem w = " ++ sq9 ++ " proof ? in 0";

let parse_zipper = (s: string): Zipper.t =>
  switch (Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper: " ++ s)
  };

let theorem_proof = (e: Exp.t): Proof.t => {
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Theorem(_, _, p, _) when Option.is_none(found^) => found := Some(p)
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  switch (found^) {
  | Some(p) => p
  | None => Alcotest.fail("no theorem proof")
  };
};

/* The commit, exactly as StepperBase emits it for a MissingStep row
   backed by a hole: one ProofPatch replacing the hole. */
let commit_patch = (~src: string, v: string) => {
  let z = parse_zipper(src);
  let root = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let target_id = Proof.rep_id(theorem_proof(root));
  let patch =
    EditorTransform.mk_proof_patch(
      ~target_id,
      Proof.fresh(SB.Stepper.witness_term(~exp=parse_exp(v))),
    );
  (z, patch, EditorTransform.apply_patch(z, patch));
};

let text = (z: Zipper.t) => Printer.of_zipper(~holes="?", z);

let contains = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

/* Verdict for a witness typed as `v_src` against `goal_src`. */
let verdict = (~goal_src: string, v_src: string) => {
  let binder = MS.WitnessCheck.goal_binder(Some(parse_exp(goal_src)));
  let v = parse_exp(v_src);
  let (info, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      v,
    );
  let (label, result) =
    MS.WitnessCheck.verdict(
      ~binder,
      ~has_errors=Statics.Map.error_ids(info) != [],
      ~v_ty=Statics.Map.ty_of(Exp.rep_id(v), info),
      v,
    );
  let shown =
    switch (result) {
    | Ok(_) => "OK"
    | Error(msg) => msg
    };
  print_endline(
    "WITNESSUI verdict goal=" ++ goal_src ++ " v=" ++ v_src ++ " => " ++ label ++ " | " ++ shown,
  );
  (label, result);
};

let is_ok = r =>
  switch (r) {
  | Ok(_) => true
  | Error(_) => false
  };

let msg_of = r =>
  switch (r) {
  | Ok(_) => "OK"
  | Error(m) => m
  };

/* F1: the stepper's row projection of a written proof (same calculate
   the app runs; proof map from the big-step checker with statics). */
let rows_after = (src: string) => {
  let e = parse_exp(src);
  let (state, _, elab) = Test_ProofMap.eval_with_proof(e);
  let proof =
    switch (Test_ProofMap.find_theorem_proof(elab)) {
    | Some(p) => p
    | None => Alcotest.fail("no proof")
    };
  let pm = EvaluatorState.get_proof_map(state);
  Test_StepperBase.test_calculate(
    ~exp=parse_exp(sq9),
    ~ctx=Builtins.ctx_init(Some(Operators.default_mode)),
    ~ana=IdTagged.FreshGrammar.Typ.bool(),
    ~proof=Util.Calc.NewValue(proof),
    ~proof_map=Util.Calc.NewValue(pm),
    Test_StepperBase.mk_missing_step(),
  );
};

/* F2: the APP's evaluation path (WorkerServer.evaluate_sync, as
   EvalResult calls it on the main thread), not a direct
   Evaluator.evaluate(~statics). */
let worker_proof_map = (src: string): (ProofMap.t, Proof.t) => {
  let (statics, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      parse_exp(src),
    );
  let eval_info_map =
    EvalInfo.of_info_map(
      ~probe_all=false,
      ~targets=Sample.no_targets,
      statics,
    );
  switch (
    WorkerServer.evaluate_sync({
      expr: elab,
      eval_info_map,
      prev: IncrEval.empty,
    })
  ) {
  | Ok((_, state)) => (
      EvaluatorState.get_proof_map(state),
      theorem_proof(elab),
    )
  | Error(_) => Alcotest.fail("worker evaluation error")
  };
};

let full3 =
  "theorem w = "
  ++ sq9
  ++ " proof\n== true by witness 3 end;\neval 3 * 3 at 0 end;\neval 9 == 9 at 0 end\nin 0";

let tests = (
  "ResetExistentialWitnessUI",
  [
    test_case("Witness button opens a blank editor; toggles closed", `Quick, () => {
      let opened =
        MS.Update.update(
          ~settings=Web.Settings.Model.init,
          MS.Update.ToggleWitness,
          MS.Model.init,
        ).
          model;
      switch (opened.open_box) {
      | WitnessOpen({editor, _}) =>
        check(
          string,
          "blank (nothing typed: an empty hole only)",
          "",
          Printer.of_zipper(~holes="", editor.editor.state.zipper),
        )
      | _ => Alcotest.fail("Witness box not open")
      };
      let closed =
        MS.Update.update(
          ~settings=Web.Settings.Model.init,
          MS.Update.ToggleWitness,
          opened,
        ).
          model;
      check(
        bool,
        "closed again",
        true,
        switch (closed.open_box) {
        | NoneOpen => true
        | _ => false
        },
      );
      check(bool, "toggle is not an undo unit", false, MS.Update.can_undo(MS.Update.ToggleWitness));
    }),
    test_case("commit: one source edit, readback, instantiated body is the next goal", `Quick, () => {
      let (before, patch, after) = commit_patch(~src=hole_src, "3");
      let written = text(after);
      print_endline("WITNESSUI committed: " ++ written);
      check(bool, "writes the witness step", true, contains(written, "== true by witness 3 end"));
      check(bool, "one undoable action", true, Web.CellEditor.Update.can_undo(Web.CellEditor.Update.PatchMainEditor(patch)));
      check(string, "undo target unchanged (the pre-commit zipper)", text(parse_zipper(hole_src)), text(before));
      let o = RE.observe("ui-commit", written);
      RE.valid("ui-commit", o);
      let w = RE.node(o, "w", Proof.RelEqBy);
      check(string, "instantiated body", "3 * 3 == 9", RE.out(w));
      check(string, "not proven by the witness alone", "Incomplete", RE.full(o, "w"));
    }),
    test_case("bad witness: committed text, false body is not a refutation", `Quick, () => {
      let (_, _, after) = commit_patch(~src=hole_src, "2");
      let o = RE.observe("ui-bad", text(after));
      check(string, "instantiated body", "2 * 2 == 9", RE.out(RE.node(o, "w", Proof.RelEqBy)));
      check(bool, "not Proven", false, RE.full(o, "w") == "Proven");
      check(bool, "not Refuted", false, RE.full(o, "w") == "Refuted");
    }),
    test_case("F1 row projection: the witness row's next row is P[v]", `Quick, () => {
      let (_, _, after) = commit_patch(~src=hole_src, "3");
      switch (rows_after(text(after))) {
      | SB.NextStep({step_kind: WitnessStep(_), next_step, _}) =>
        switch (next_step) {
        | SB.MissingStep(m, _) =>
          check(
            string,
            "next row goal",
            "3 * 3 == 9",
            m.full_exp |> Util.Calc.get_saved_opt |> Option.map(RE.print_exp)
            |> Option.value(~default="-"),
          )
        | _ => Alcotest.fail("expected a missing-step row for P[v]")
        }
      | _ => Alcotest.fail("expected NextStep(WitnessStep) row")
      };
    }),
    test_case("F1 full flow rows: witness 3 then two evals finishes", `Quick, () => {
      let rec last = (ns: SB.next_step) =>
        switch (ns) {
        | SB.NextStep({next_step, _}) => last(next_step)
        | other => other
        };
      /* Every chain ends in the trailing picker row (`MissingStep(_,
         Finished)`); a completed proof's trailing row has goal `true`. */
      switch (rows_after(full3) |> last) {
      | SB.MissingStep(m, SB.Finished) =>
        check(
          string,
          "trailing row goal after witness 3, eval, eval",
          "true",
          m.full_exp
          |> Util.Calc.get_saved_opt
          |> Option.map(RE.print_exp)
          |> Option.value(~default="-"),
        )
      | SB.MissingStep(_, _) => Alcotest.fail("trailing row not terminal")
      | SB.Finished => Alcotest.fail("no trailing row")
      | SB.NextStep(_) => Alcotest.fail("unreachable")
      };
    }),
    test_case("F2 APP PATH (WorkerServer.evaluate_sync): witness 3 + evals Proven, witness 2 not", `Quick, () => {
      let (pm, proof) = worker_proof_map(full3);
      List.iter(
        (n: Proof.t) =>
          switch (ProofMap.lookup(Proof.rep_id(n), pm)) {
          | Some(e) =>
            print_endline(
              "WITNESSUI worker node "
              ++ Proof.show_cls(Proof.cls_of_term(n.term))
              ++ " out: "
              ++ (e.outgoing |> Option.map(RE.print_exp) |> Option.value(~default="-"))
              ++ " | marks: "
              ++ String.concat("; ", List.map(ProofMark.show, e.marks)),
            )
          | None => ()
          },
        RE.proof_nodes(proof),
      );
      check(
        string,
        "witness 3 + evals via the app path",
        "Proven",
        ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof)),
      );
      let (pm2, proof2) =
        worker_proof_map(
          "theorem w = " ++ sq9 ++ " proof == true by witness 2 end; eval 2 * 2 at 0 end; eval 4 == 9 at 0 end in 0",
        );
      check(
        bool,
        "witness 2 not Proven via the app path",
        false,
        ProofMap.full_status_of_proof(pm2, proof2) == ProofMap.Proven,
      );
    }),
    test_case("honest pre-commit verdicts", `Quick, () => {
      let (label, r) = verdict(~goal_src=sq9, "3");
      check(string, "label", "Witness for n : Int", label);
      check(bool, "3 offered", true, is_ok(r));
      let (_, r) = verdict(~goal_src=sq9, "true");
      check(string, "wrong type", "The witness has type Bool; the binder needs Int.", msg_of(r));
      let (_, r) = verdict(~goal_src=sq9, "?");
      check(bool, "empty not offered", false, is_ok(r));
      let (_, r) = verdict(~goal_src=sq9, "nope + 1");
      check(string, "static error", "The witness has a static error.", msg_of(r));
      let (_, r) = verdict(~goal_src="exists n -> n == 1", "1");
      check(bool, "untyped binder not offered", false, is_ok(r));
      let (_, r) = verdict(~goal_src="9 == 9", "3");
      check(
        string,
        "stale goal",
        "This goal is no longer an `exists`; the witness cannot be used here.",
        msg_of(r),
      );
    }),
  ],
);
