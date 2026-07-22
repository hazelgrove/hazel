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

/* A documented program must parse, roundtrip, and be statics-clean. */
let assert_clean = (prog: string): unit =>
  switch (parse(prog)) {
  | None => failf("failed to parse: %s", prog)
  | Some(z) =>
    let printed = Printer.of_zipper(~holes="?", z);
    if (squish(printed) != squish(prog)) {
      failf("token expansion changed program: %s", prog);
    };
    switch (statics_errors(z)) {
    | [] => ()
    | errs =>
      failf("static errors in %s: %s", prog, String.concat("; ", errs))
    };
  };

/* Function-definition sugar documented as preferred in the prompts
   (HazelSyntaxNotes, Eg_RecFib): `let f(x, y) = body in ...`, with or
   without annotations, recursive or not. */
let fn_sugar_clean = () =>
  List.iter(
    assert_clean,
    [
      "let add(x, y) = x + y in add(1, 2)",
      "let add(x: Int, y: Int): Int = x + y in add(1, 2)",
      "let fact(n) = if n < 2 then 1 else n * fact(n - 1) in fact(5)",
      "let fact(n: Int): Int = if n < 2 then 1 else n * fact(n - 1) in fact(5)",
      "let f() = 3 in f()",
      /* explicit form, unannotated recursion (prompt rule 4) */
      "let fact = fun n -> if n < 2 then 1 else n * fact(n - 1) in fact(5)",
      /* Eg_RecFib few-shot program */
      "let fib(n: Int): Int =\n  if n <= 0\n    then 0\n    else if n == 1\n      then 1\n      else fib(n - 1) + fib(n - 2)\nin\ntest fib(0) == 0 end;\ntest fib(1) == 1 end;\ntest fib(5) == 5 end;\ntest fib(10) == 55 end;\nfib(10)",
      "let fib(n: Int): Int = n in\nlet map_fib(ns: [Int]): [Int] =\n  case ns\n  | [] => []\n  | hd :: tl => fib(hd) :: map_fib(tl)\n  end\nin\ntest map_fib([]) == [] end;\nmap_fib([1, 2, 3, 4, 5])",
    ],
  );

let fn_sugar_evaluates = () => {
  Test_Evaluator_Prelude.parse_and_evaluate_test(
    "120",
    "let fact(n) = if n < 2 then 1 else n * fact(n - 1) in fact(5)",
  );
  Test_Evaluator_Prelude.parse_and_evaluate_test(
    "3",
    "let add(x, y) = x + y in add(1, 2)",
  );
};

let tests = [
  (
    "PromptFactory",
    [
      test_case("reserved words are unusable", `Quick, check_reserved),
      test_case("non-reserved near-misses are usable", `Quick, check_usable),
      test_case(
        "fn-definition sugar is statics-clean",
        `Quick,
        fn_sugar_clean,
      ),
      test_case("fn-definition sugar evaluates", `Quick, fn_sugar_evaluates),
    ],
  ),
];
