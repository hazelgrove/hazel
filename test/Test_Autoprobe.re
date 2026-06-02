/**
 * Tests for auto-probe target selection (ProbePerform.current_toplevel_def).
 *
 * Each test specifies an input program containing a `¦` caret marker and
 * the expected text of the probed expression (or "<none>" if no probe
 * should be placed).
 *
 * Probe-selection rule (see ProbePerform.toplevel_def_body_id):
 *   Walk the cursor's ancestor chain outermost-to-innermost:
 *     - Let(p, def, body): cursor in body → continue; otherwise probe def.
 *     - Seq(e1, e2): cursor in e1 or e2 → continue; cursor on `;` → probe e1.
 *     - TyAlias(p, ty, body): cursor in body → continue; otherwise no probe.
 *     - non-chain ancestor → probe it (the enclosing bare expression).
 *   If the walk falls through, apply the same rules to the cursor's piece.
 *   Test/HintedTest bodies are unwrapped at the end (probe the condition,
 *   not the unit result).
 *
 * Caret targeting: tries `Indicated.index` first, then walks left through
 * secondaries to find a meaningful piece, then falls back to the cursor's
 * containing zipper ancestor.
 */
open Alcotest;
open Haz3lcore;
open Language;
open Action;

let caret_char = "¦";

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Token.to_list |> List.map(c => Action.Insert(c));

let mv_l = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go(
      ~settings=Language.CoreSettings.off,
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    );
  List.fold_left(
    (z: Zipper.t, a: Action.t) =>
      switch (perform(a, z)) {
      | Ok(z) => z
      | Error(err) =>
        Alcotest.fail("Failed on action: " ++ Action.Failure.show(err))
      },
    zip,
    actions,
  );
};

/* Split input at the caret marker, build the program then move the caret
 * back to the marker position. Mirrors Test_Indication. */
let mk = (init: string): list(Action.t) => {
  let rec split =
          (before: list(string), rest: list(string))
          : (list(string), list(string)) =>
    switch (rest) {
    | [] => Alcotest.fail("No caret in: " ++ init)
    | [hd, ...tl] =>
      hd == caret_char
        ? (List.rev(before), tl) : split([hd, ...before], tl)
    };
  let (before, after) = split([], Token.to_list(init));
  let s = Token.of_list(before @ after);
  string_to_ltr_actions(s) @ mv_l(List.length(after));
};

/* Convert a probed id to its source text via TermData + Printer. */
let probed_str = (z: Zipper.t): string => {
  let root_segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term, _} = MakeTerm.go(root_segment);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  switch (ProbePerform.current_toplevel_def(info_map, z)) {
  | None => "<none>"
  | Some(id) =>
    let syntax = CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z);
    switch (TermData.segment(id, syntax.term_data)) {
    | Some(seg) =>
      Printer.of_segment(~holes=" ", ~indent="", ~is_single_line=false, seg)
    | None => "<id not in term_data: " ++ Id.to_string(id) ++ ">"
    };
  };
};

let auto = (~name, ~input, ~probed) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = mk(input) |> perform(Zipper.init());
      check(
        testable(Fmt.string, String.equal),
        probed,
        probed,
        probed_str(z),
      );
    },
  );

/* ==================================================================
 * TEST SUITES
 * ================================================================== */

let basic_let_tests = [
  auto(
    ~name="cursor in let body (single line) probes body",
    ~input="let x = 5 in ¦x + 1",
    ~probed="x + 1",
  ),
  auto(
    ~name="cursor in def probes def",
    ~input="let x = ¦5 in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor on pat probes def",
    ~input="let ¦x = 5 in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor on `in` keyword probes def",
    ~input="let x = 5 ¦in x + 1",
    ~probed="5",
  ),
  auto(
    ~name="cursor after `in ` then space then body probes body",
    ~input="let x = 5 in ¦ x + 1",
    ~probed="x + 1",
  ),
];

let nested_let_tests = [
  auto(
    ~name="cursor in innermost body of nested let probes that body",
    ~input="let x = 5 in let y = 2 in ¦x + y",
    ~probed="x + y",
  ),
  auto(
    ~name="cursor in middle let's def probes that def",
    ~input="let x = 5 in let y = ¦2 in x + y",
    ~probed="2",
  ),
  auto(
    ~name="cursor on `let` of second let probes second let's def",
    ~input="let x = 5 in ¦let y = 2 in x + y",
    ~probed="2",
  ),
];

let seq_tests = [
  auto(
    ~name="cursor in e1 of seq probes e1",
    ~input="¦foo(); bar()",
    ~probed="foo()",
  ),
  auto(
    ~name="cursor on `;` probes e1",
    ~input="foo()¦; bar()",
    ~probed="foo()",
  ),
  auto(
    ~name="cursor in e2 of seq probes e2",
    ~input="foo(); ¦bar()",
    ~probed="bar()",
  ),
];

let bare_expression_tests = [
  auto(
    ~name="cursor on bare expression probes it",
    ~input="¦1 + 2",
    ~probed="1 + 2",
  ),
];

/* Function-definition sugar: `let f(args) = body` desugars (in statics) to
 * `let f = fun args -> body` while reusing the surface Let's id. That reuse
 * duplicates the Let in the cursor's ancestor chain, which used to make
 * auto-probe target the function body even when the cursor was in the let
 * body. Guards the dedup_adjacent workaround in
 * ProbePerform.toplevel_def_body_id. */
let function_sugar_tests = [
  auto(
    ~name="sugar: cursor in let body probes let body (not the function body)",
    ~input="let f(x: Int): Int = x + 1 in ¦f(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar: cursor in function body probes function body",
    ~input="let f(x: Int): Int = ¦x + 1 in f(5)",
    ~probed="x + 1",
  ),
  auto(
    ~name="sugar inside let-body call probes let body",
    ~input="let f(x: Int): Int = x + 1 in f¦(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar no return type: cursor in let body probes let body",
    ~input="let f(x: Int) = x + 1 in ¦f(5)",
    ~probed="f(5)",
  ),
  auto(
    ~name="sugar no return type: cursor in function body probes function body",
    ~input="let f(x: Int) = ¦x + 1 in f(5)",
    ~probed="x + 1",
  ),
];

let tyalias_tests = [
  auto(
    ~name="cursor in tyalias body probes body (transparent)",
    ~input="type T = Int in ¦x + 1",
    ~probed="x + 1",
  ),
  auto(
    ~name="cursor in tyalias type does not probe",
    ~input="type T = ¦Int in x + 1",
    ~probed="<none>",
  ),
];

let tests = [
  ("Autoprobe.BasicLet", basic_let_tests),
  ("Autoprobe.NestedLet", nested_let_tests),
  ("Autoprobe.Seq", seq_tests),
  ("Autoprobe.FunctionSugar", function_sugar_tests),
  ("Autoprobe.BareExpression", bare_expression_tests),
  ("Autoprobe.TyAlias", tyalias_tests),
];
