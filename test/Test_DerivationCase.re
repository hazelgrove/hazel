/* Reproduces the crash that happens when typing a case expression inside
   a derivation: "Assertion failed" at Tile.reassemble when typing the
   second `|`. */

open Alcotest;
open Haz3lcore;
open Language;
open Action;

let default_settings = {
  ...Language.CoreSettings.off,
  statics: true,
  assist: true,
  deep_reassociate: true,
};

/* Simulate the web editor's full update+calculate cycle by driving the
   Editor.Update pipeline rather than Perform.go alone. This gives us the
   same TyDi buffer clearing, update, and calculate sequence the web runs
   on each character, which is what exercises CachedSyntax.mk and
   Dump.to_segment (the paths most likely to hit Tile.reassemble). */
let init_model = (~root, z: Zipper.t): Editor.Model.t =>
  Editor.Model.mk(z, ~root);

let stitch = (x: Language.Exp.t): Language.Exp.t => x;

let update_once =
    (~settings, a: Action.t, model: Editor.Model.t): Editor.Model.t => {
  let old_statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch,
      ~root=model.root,
      model.state.zipper,
    );
  let old_dynamics = Language.Dynamics.Map.empty;
  let result =
    Editor.Update.update(~settings, a, old_statics, old_dynamics, model);
  let model =
    switch (result) {
    | Ok(m) => m
    | Error(err) =>
      Alcotest.failf(
        "Editor.Update.update failed: %s",
        Action.Failure.show(err),
      )
    };
  let new_statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch,
      ~root=model.root,
      model.state.zipper,
    );
  Editor.Update.calculate(
    ~settings,
    ~autoprobe_mode=false,
    ~is_edited=true,
    new_statics,
    Language.Dynamics.Map.empty,
    model,
  );
};

let perform_chars_editor =
    (~root, ~settings=default_settings, chars: string, z: Zipper.t)
    : Editor.Model.t => {
  let model = init_model(~root, z);
  Token.to_list(chars)
  |> List.fold_left(
       (m, c) => update_once(~settings, Action.Insert(c), m),
       model,
     );
};

let test_drv_case_rule_insert = () => {
  /* Incrementally type "case x | A => 1 | B => 2 end" in Drv(Exp).
     The reported crash is when typing the second `|`. */
  let z = Zipper.init();
  let _z =
    perform_chars_editor(~root=Drv(Exp), "case x | A => 1 | B => 2 end", z);
  check(bool, "no crash while typing drv case", true, true);
};

let test_exp_case_rule_insert = () => {
  /* Same characters but in Exp sort. The user reports this works fine. */
  let z = Zipper.init();
  let _z = perform_chars_editor(~root=Exp, "case x | A => 1 | B => 2 end", z);
  check(bool, "no crash while typing exp case", true, true);
};

let test_drv_case_in_entail = () => {
  /* Drv entailment context around the case. */
  let z = Zipper.init();
  let _z =
    perform_chars_editor(
      ~root=Drv(Exp),
      "[] |- case x | A => 1 | B => 2 end",
      z,
    );
  check(bool, "no crash", true, true);
};

let test_drv_case_without_head = () => {
  /* Per the user: "the second `|`" — try just "case | A => 1 |". */
  let z = Zipper.init();
  let _z = perform_chars_editor(~root=Drv(Exp), "case | A => 1 |", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_with_pat_reassoc = () => {
  /* Try typing more things with a pattern on the rule. */
  let z = Zipper.init();
  let _z = perform_chars_editor(~root=Drv(Exp), "case x | A => 1 |", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_with_cons_rule = () => {
  /* The derivation mode supports `::` cons. Try a ::-pattern rule. */
  let z = Zipper.init();
  let _z =
    perform_chars_editor(
      ~root=Drv(Exp),
      "case l | [] => 0 | x :: xs => 1 end",
      z,
    );
  check(bool, "no crash", true, true);
};

let test_drv_case_with_prop_body = () => {
  /* Body is a propositional connective — Drv-specific behavior. */
  let z = Zipper.init();
  let _z =
    perform_chars_editor(
      ~root=Drv(Exp),
      "case x | A => A /\\ B | B => B end",
      z,
    );
  check(bool, "no crash", true, true);
};

let mk_zipper_drv = (code: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Drv(Exp), code)) {
  | Some(z) => z
  | None => Alcotest.failf("Parser.to_zipper failed for %S", code)
  };

let test_drv_case_type_after_entail = () => {
  /* Start from `[] |- ` pre-parsed, caret at end, then type the case. */
  let z = mk_zipper_drv("[] |- ");
  let _z =
    perform_chars_editor(~root=Drv(Exp), "case x | A => 1 | B => 2 end", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_type_into_empty_parsed = () => {
  /* Parse empty code with a hole, then type the case. */
  let z = mk_zipper_drv("");
  let _z =
    perform_chars_editor(~root=Drv(Exp), "case x | A => 1 | B => 2 end", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_after_absurd = () => {
  /* Some derivations might start with existing content — simulate with
     a judgment that already exists. */
  let z = mk_zipper_drv("[] |- 1");
  let _z =
    perform_chars_editor(~root=Drv(Exp), " case x | A => 1 | B => 2 end", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_no_spaces = () => {
  let z = Zipper.init();
  let _z = perform_chars_editor(~root=Drv(Exp), "case x|A=>1|B=>2 end", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_mid_rule_body = () => {
  /* Parse `case x | A => 1 end`, position caret right after `1`, then
     type ` | B => 2`. The crash trigger was the 2nd `|`. */
  let z = mk_zipper_drv("case x | A => 1 end");
  /* Move caret to right after `1`, before ` end`. Easier via Parser + Move. */
  /* Move left from end: skip `end`, space -> 4 tokens via byToken */
  let z =
    List.fold_left(
      (z, _) =>
        switch (Haz3lcore.Move.local(ByToken, Left, z)) {
        | Some(z) => z
        | None => z
        },
      z,
      [(), ()],
    );
  let _z = perform_chars_editor(~root=Drv(Exp), " | B => 2", z);
  check(bool, "no crash in mid-rule", true, true);
};

let test_drv_rule_only_then_second_bar = () => {
  /* Start from `case x | A => 1` and append ` |`. */
  let z = mk_zipper_drv("case x | A => 1");
  let _z = perform_chars_editor(~root=Drv(Exp), " |", z);
  check(bool, "no crash", true, true);
};

let test_drv_rule_only_then_second_bar_no_space = () => {
  let z = mk_zipper_drv("case x | A => 1");
  let _z = perform_chars_editor(~root=Drv(Exp), "|", z);
  check(bool, "no crash", true, true);
};

let test_drv_case_with_dollar_var = () => {
  /* Derivation mode allows $-abbreviations as quoted vars. Using one in
     a case might trigger different tokenization. */
  let z = Zipper.init();
  let _z =
    perform_chars_editor(~root=Drv(Exp), "case $x | A => 1 | B => 2 end", z);
  check(bool, "no crash", true, true);
};

let test_drv_nested_case = () => {
  let z = Zipper.init();
  let _z =
    perform_chars_editor(
      ~root=Drv(Exp),
      "case x | A => case y | C => 1 | D => 2 end | B => 3 end",
      z,
    );
  check(bool, "no crash", true, true);
};

let test_drv_case_in_let = () => {
  let z = Zipper.init();
  let _z =
    perform_chars_editor(
      ~root=Drv(Exp),
      "let f = fun x -> case x | A => 1 | B => 2 end in f",
      z,
    );
  check(bool, "no crash", true, true);
};

let prettyprint_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  hole_tiles: false,
  project_tables: false,
};

let prettyprint_exp = (exp: Language.Exp.t): unit => {
  let seg = ExpToSegment.exp_to_segment(~settings=prettyprint_settings, exp);
  let _ = PrettySegment.prettify(seg);
  ();
};

/* Pretty-print a case expression parsed from source. This is the path the
   editor uses via EvalResult.re → CodeWithStatics.mk_from_exp → ExpToSegment. */
let test_prettyprint_incomplete_case = () => {
  let input = "case 0 | [] => 0 | ";
  switch (Parser.to_term(input, ~root=Exp)) {
  | Some(exp) => prettyprint_exp(exp)
  | None => Alcotest.fail("Failed to parse: " ++ input)
  };
  check(bool, "no crash", true, true);
};

let test_prettyprint_complete_case = () => {
  let input = "case 0 | [] => 0 | _ => 1 end";
  switch (Parser.to_term(input, ~root=Exp)) {
  | Some(exp) => prettyprint_exp(exp)
  | None => Alcotest.fail("Failed to parse: " ++ input)
  };
  check(bool, "no crash", true, true);
};

/* Simulate evaluator stripping multi-ids: construct a Match term with only
   ONE id (rep_id) and pretty-print it. This is the path HACK[Matt] pad_ids
   was designed for. */
let test_prettyprint_single_id_match = () => {
  let input = "case 0 | [] => 0 | _ => 1 end";
  switch (Parser.to_term(input, ~root=Exp)) {
  | Some(exp) =>
    let rep = Exp.rep_id(exp);
    prettyprint_exp(IdTagged.fast_copy(rep, exp));
  | None => Alcotest.fail("Failed to parse: " ++ input)
  };
  check(bool, "no crash with single-id Match", true, true);
};

/* Regression test: Match with duplicate ids in its annotation. Before the
   pad_ids fix, this would assign the same id to the case and every rule
   tile, colliding in Segment.reassemble and failing the shards-monotonic
   assertion in Tile.reassemble. */
let test_prettyprint_duplicate_ids_match = () => {
  let input = "case 0 | [] => 0 | _ => 1 end";
  switch (Parser.to_term(input, ~root=Exp)) {
  | Some(exp) =>
    let rep = Exp.rep_id(exp);
    let (term, _) = IdTagged.unwrap(exp);
    prettyprint_exp(IdTagged.mk_internal([rep, rep, rep], term));
  | None => Alcotest.fail("Failed to parse: " ++ input)
  };
  check(bool, "no crash with duplicate-id Match", true, true);
};

/* Regression test for the user-reported crash. The Drv(Exp) Case form in
   ExpToSegment previously reused the case id for both Rule tiles, causing
   the same kind of id collision during pretty-printing. */
let test_prettyprint_drv_case = () => {
  let input = "case x | A => 1 | B => 2 end";
  switch (Parser.to_term(input, ~root=Drv(Exp))) {
  | Some(exp) => prettyprint_exp(exp)
  | None => Alcotest.fail("Failed to parse: " ++ input)
  };
  check(bool, "no crash pretty-printing Drv case", true, true);
};

let test_drv_case_insert_bar_after_complete_case = () => {
  /* Parse complete rule, move caret before the final `end`, then type
     ` | B => 2`. The test was previously passing; this tries various
     positions to find the failing sequence. */
  let z = mk_zipper_drv("case x | A => 1 end");
  /* Move to just before "end" by jumping. This mimics clicking before end. */
  let seg = Zipper.unselect_and_zip(z);
  /* Find the "end" piece id and jump there. */
  let find_end_id = (seg: Segment.t): option(Id.t) =>
    List.find_map(
      fun
      | Piece.Tile({label: ["case", "end"], id, _} as _t: Tile.t) =>
        Some(id)
      | _ => None,
      seg,
    );
  let id_opt = find_end_id(seg);
  let z =
    switch (id_opt) {
    | Some(id) =>
      switch (
        Haz3lcore.Move.jump_to_side_of_id(Util_web.Direction.Left, z, id)
      ) {
      | Some(z) => z
      | None => z
      }
    | None => z
    };
  /* Now caret is at start of "end". Move one token left to be before end. */
  let z =
    switch (Haz3lcore.Move.local(ByToken, Left, z)) {
    | Some(z) => z
    | None => z
    };
  let _z = perform_chars_editor(~root=Drv(Exp), "| B => 2 ", z);
  check(bool, "no crash", true, true);
};

let tests = [
  (
    "DerivationCase",
    [
      test_case(
        "Type full case in Exp sort",
        `Quick,
        test_exp_case_rule_insert,
      ),
      test_case(
        "Type full case in Drv(Exp) sort",
        `Quick,
        test_drv_case_rule_insert,
      ),
      test_case(
        "Drv case inside entailment",
        `Quick,
        test_drv_case_in_entail,
      ),
      test_case(
        "Drv case missing scrutinee",
        `Quick,
        test_drv_case_without_head,
      ),
      test_case(
        "Drv case stop after 2nd bar",
        `Quick,
        test_drv_case_with_pat_reassoc,
      ),
      test_case(
        "Drv case with cons pattern",
        `Quick,
        test_drv_case_with_cons_rule,
      ),
      test_case(
        "Drv case with prop body",
        `Quick,
        test_drv_case_with_prop_body,
      ),
      test_case(
        "Drv case after parsed entail",
        `Quick,
        test_drv_case_type_after_entail,
      ),
      test_case(
        "Drv case into parsed empty",
        `Quick,
        test_drv_case_type_into_empty_parsed,
      ),
      test_case(
        "Drv case after parsed judgment",
        `Quick,
        test_drv_case_after_absurd,
      ),
      test_case("Drv case no spaces", `Quick, test_drv_case_no_spaces),
      test_case(
        "Drv case add rule in middle",
        `Quick,
        test_drv_case_mid_rule_body,
      ),
      test_case(
        "Drv case parsed single rule then bar",
        `Quick,
        test_drv_rule_only_then_second_bar,
      ),
      test_case(
        "Drv case parsed single rule then bar no-space",
        `Quick,
        test_drv_rule_only_then_second_bar_no_space,
      ),
      test_case("Drv case with $-var", `Quick, test_drv_case_with_dollar_var),
      test_case("Drv nested case", `Quick, test_drv_nested_case),
      test_case("Drv case in let", `Quick, test_drv_case_in_let),
      test_case(
        "Drv insert bar after complete case",
        `Quick,
        test_drv_case_insert_bar_after_complete_case,
      ),
      test_case(
        "Pretty-print complete case (sanity)",
        `Quick,
        test_prettyprint_complete_case,
      ),
      test_case(
        "Pretty-print incomplete case",
        `Quick,
        test_prettyprint_incomplete_case,
      ),
      test_case(
        "Pretty-print Match with single id (eval path)",
        `Quick,
        test_prettyprint_single_id_match,
      ),
      test_case(
        "Pretty-print Match with duplicate ids (repro)",
        `Quick,
        test_prettyprint_duplicate_ids_match,
      ),
      test_case(
        "Pretty-print Drv case expression",
        `Quick,
        test_prettyprint_drv_case,
      ),
    ],
  ),
];
