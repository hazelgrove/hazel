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
        check(string, "blank", "", text(editor.editor.state.zipper))
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
