open Alcotest;
open Haz3lcore;

/* Completion-triggered local re-indentation (LocalReformat, gated by
   CoreSettings.auto_reindent). Tests drive the editor with the setting
   on and check the resulting text. */

let settings = {
  ...Test_Editing.default_settings,
  Language.CoreSettings.auto_reindent: true,
};

let type_all = (s: string): list(Action.t) =>
  s
  |> String.to_seq
  |> List.of_seq
  |> List.map(c => Action.Insert(String.make(1, c)));

let text_of = (z: Zipper.t): string =>
  Printer.of_segment(~holes="?", ~refractors=[], Zipper.unselect_and_zip(z));

let run = (actions: list(Action.t)): string =>
  Test_Editing.perform(~settings, Zipper.init(), actions) |> text_of;

let reformat_tests = [
  test_case(
    "closing paren re-indents multiline contents",
    `Quick,
    () => {
      /* ( newline 1 + 2 — unindented because typed flat — then ) */
      let got = run(type_all("(\n1\n+ 2)"));
      check(
        string,
        "child shifted to continuation indent",
        "(\n  1\n  + 2)",
        got,
      );
    },
  ),
  test_case(
    "dropped `in` re-indents the definition",
    `Quick,
    () => {
      /* let x = <newline> 1 + 2 <newline> then complete with in;
         the definition lines were typed flat */
      let got = run(type_all("let x =\n1\n+ 2\nin 3"));
      check(
        string,
        "definition shifted under let",
        "let x =\n  1\n  + 2\nin 3",
        got,
      );
    },
  ),
  test_case(
    "already-canonical content is a no-op",
    `Quick,
    () => {
      /* enter-indent already puts the line at +2 (continuation), so
         completing the paren must not double-shift */
      let flat = run(type_all("(1 + 2)"));
      check(string, "inline unchanged", "(1 + 2)", flat);
    },
  ),
  test_case(
    "setting off leaves indentation alone",
    `Quick,
    () => {
      /* enter-indent still fires (it is not this setting), but the
         continuation line stays un-fixed after the paren completes */
      let got =
        Test_Editing.perform(Zipper.init(), type_all("(\n1\n+ 2)"))
        |> text_of;
      check(string, "no fix when disabled", "(\n  1\n+ 2)", got);
    },
  ),
  test_case(
    "unsettled child is preserved as typed",
    `Quick,
    () => {
      /* the let never gets its `in`, so the child is unsettled and no
         per-line re-derivation happens: the as-typed indentation — the
         user's completion signal — is preserved (delta here is 0) */
      let got = run(type_all("(\nlet y =\n1\n3)"));
      check(
        string,
        "unsettled child preserved as typed",
        "(\n  let y =\n    1\n   3)",
        got,
      );
    },
  ),
];

let tests = [("LocalReformat", reformat_tests)];
