/* Checks that language claims made by the agent prompts in
   [CompositionCore/prompt_factory/] stay true: reserved words really are
   unusable as identifiers, and documented syntax parses statics-clean.
   If one of these fails, fix the prompt copy along with the code. */
open Alcotest;
open Haz3lcore;
open Language;

let parse = (code: string): option(Zipper.t) =>
  Parser.to_zipper(~root=Exp, code);

let statics_errors = (z: Zipper.t): list(string) =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
    ),
  )
  |> ErrorPrint.all;

let squish = (s: string): string =>
  Util.StringUtil.replace(Util.StringUtil.regexp("[\\s]+"), s, "");

/* A word is usable as an identifier iff `let w = 1 in w + 1` parses,
   roundtrips (no token expanded into a form), and has no static errors. */
let diagnose = (word: string): option(string) => {
  let prog = "let " ++ word ++ " = 1 in " ++ word ++ " + 1";
  switch (parse(prog)) {
  | None => Some("parse failure")
  | Some(z) =>
    let printed = Printer.of_zipper(~holes="?", z);
    if (squish(printed) != squish(prog)) {
      Some("token expands: " ++ String.trim(printed));
    } else {
      switch (statics_errors(z)) {
      | [] => None
      | errs => Some("static errors: " ++ String.concat("; ", errs))
      };
    };
  };
};

/* Words the prompt warns are reserved (HazelSyntaxNotes). */
let reserved = [
  "case",
  "debug",
  "eval",
  "fix",
  "forall",
  "fun",
  "hide",
  "hint",
  "if",
  "in",
  "let",
  "module",
  "pause",
  "test",
  "theorem",
  "type",
  "typfun",
  "use",
  "proof_object",
  "of_jdmt",
  "of_ctx",
  "of_prop",
  "of_alfa_exp",
  "of_alfa_typ",
  "of_alfa_pat",
  "of_alfa_tpat",
];

/* Near-miss words that must stay usable, so the warning doesn't overreach. */
let usable = [
  "end",
  "then",
  "else",
  "rec",
  "poly",
  "val",
  "with",
  "of",
  "match",
  "filter",
  "evaluate",
  "run_tests",
];

let check_reserved = () =>
  List.iter(
    w =>
      switch (diagnose(w)) {
      | Some(_) => ()
      | None => failf("`%s` is usable as an identifier; unlist it", w)
      },
    reserved,
  );

let check_usable = () =>
  List.iter(
    w =>
      switch (diagnose(w)) {
      | None => ()
      | Some(why) => failf("`%s` unusable as an identifier: %s", w, why)
      },
    usable,
  );

let tests = [
  (
    "PromptFactory",
    [
      test_case("reserved words are unusable", `Quick, check_reserved),
      test_case("non-reserved near-misses are usable", `Quick, check_usable),
    ],
  ),
];
