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
  Util_web.StringUtil.replace(Util_web.StringUtil.regexp("[\\s]+"), s, "");

/* A program is identifier-safe iff it parses, roundtrips (no token
   expanded into a form), and has no static errors. */
let diagnose_prog = (prog: string): option(string) =>
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

/* Probe a word in let-binding and fun-parameter positions. */
let diagnose = (word: string): option(string) => {
  switch (diagnose_prog("let " ++ word ++ " = 1 in " ++ word ++ " + 1")) {
  | Some(why) => Some("let-form: " ++ why)
  | None =>
    switch (diagnose_prog("(fun " ++ word ++ " -> " ++ word ++ " + 1)(1)")) {
    | Some(why) => Some("fun-form: " ++ why)
    | None => None
    }
  };
};

/* Does the token expand into a form in type position? (statics ignored:
   a free lowercase type variable errors without being an expansion.) */
let expands_in_typ = (word: string): bool => {
  let prog = "type T = " ++ word ++ " in 1";
  switch (parse(prog)) {
  | None => true
  | Some(z) =>
    let printed = Printer.of_zipper(~holes="?", z);
    squish(printed) != squish(prog);
  };
};

/* Words that misparse as identifiers today (leading delimiters expand).
   The prompt (HazelSyntaxNotes) lists these plus the soft set below. */
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

/* Delimiter tokens (Form.delims) that nonetheless parse as identifiers:
   the molder is sort-aware, so trailing delimiters (end/then/else/with),
   Typ-sort keywords (rec/poly), and Drv/ALFA proof-language delimiters
   (val/valid/consistent/matched_*) all fall back to variables in
   expression/pattern position. If one hardens, move it to [reserved]
   and re-tier the prompt warning. */
let soft_reserved = [
  "consistent",
  "else",
  "end",
  "matched_arrow",
  "matched_prod",
  "matched_sum",
  "poly",
  "proof_of",
  "rec",
  "then",
  "val",
  "valid",
  "with",
];

/* Reserved in type positions only (backs the prompt's second tier). */
let typ_reserved = ["rec", "poly"];
let typ_usable = ["val", "valid", "consistent"];

/* Near-miss words that must stay usable, so the warning doesn't overreach. */
let usable = [
  "of",
  "match",
  "switch",
  "do",
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
    soft_reserved @ usable,
  );

let check_typ_position = () => {
  List.iter(
    w =>
      expands_in_typ(w)
        ? () : failf("`%s` no longer expands in type position", w),
    typ_reserved,
  );
  List.iter(
    w =>
      expands_in_typ(w)
        ? failf("`%s` expands in type position; re-tier the warning", w) : (),
    typ_usable,
  );
};

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

/* Parse + statics only (big samples may deliberately end in a hole). */
let assert_parses_clean = (prog: string): unit => {
  let head = String.sub(prog, 0, min(60, String.length(prog)));
  switch (parse(prog)) {
  | None => failf("failed to parse sample starting: %s", head)
  | Some(z) =>
    switch (statics_errors(z)) {
    | [] => ()
    | errs =>
      failf(
        "static errors in sample starting %s: %s",
        head,
        String.concat("; ", errs),
      )
    }
  };
};

/* Language snippets documented across the prompt files. */
let doc_examples_clean = () =>
  List.iter(
    assert_clean,
    [
      /* HazelSyntaxNotes: mod builtin, float ops, pipeline */
      "int_mod(7, 2)",
      "1.5 +. 2.5 ==. 4.0",
      "let inc(x) = x + 1 in 1 |> inc",
      /* HazelSyntaxNotes: polymorphic map */
      "let map : poly A -> poly B -> ((A -> B), [A]) -> [B] =\n  typfun A -> typfun B -> fun f, xs ->\n    case xs\n    | [] => []\n    | hd :: tl => f(hd) :: map@<A>@<B>(f, tl)\n    end\nin\nmap@<Int>@<Bool>(fun n -> n > 1, [1, 2, 3])",
      /* HazelSyntaxNotes: modules */
      "let m = { let x = 1; let y = true } in m.x",
      "let m = { type T = Int; let x = 5 : T } in m.x",
      "module M = { let x = 1; let y = 2 } in M.x + M.y",
      "let m = { module Inner = { let z = 42 }; let r = Inner.z } in m.r",
      "let outer = { let inner = { let a = 42 } } in outer.inner.a",
      "module M = { type T = Int } in\nlet x : M.T = 6 in x",
      /* HazelSyntaxNotes: mutual recursion via tuple binding */
      "let (even : Int -> Bool, odd : Int -> Bool) =\n  (fun n -> if n == 0 then true else odd(n - 1),\n   fun n -> if n == 0 then false else even(n - 1))\nin\neven(4)",
      /* HazelDocumentation: implicit recursive type alias */
      "type MyList = Nil + Cons(Int, MyList) in\nlet x : MyList = Cons(1, Cons(2, Cons(3, Nil))) in x",
      /* CompositionPrompt: projector concrete syntax */
      "let speed = ^^slider(60) in speed",
    ],
  );

/* Fenced ``` blocks inside a prompt string. */
let fenced_blocks = (s: string): list(string) => {
  let parts = Util_web.StringUtil.plain_split(s, "```");
  List.filteri((i, _) => i mod 2 == 1, parts);
};

/* The complete sample programs shipped in HazelDocumentation. */
let big_samples_clean = () => {
  List.iter(
    s => List.iter(assert_parses_clean, fenced_blocks(s)),
    [
      HazelDocumentation.sample_tic_tac_toe_program,
      HazelDocumentation.sample_emoji_paint,
    ],
  );
  let poly_doc =
    HazelDocumentation.polymorphism_documentation
    |> String.split_on_char('\n')
    |> List.filter(l =>
         Util_web.StringUtil.plain_search("polymorphismDocumentation", l, 0)
         < 0
       )
    |> String.concat("\n");
  assert_parses_clean(poly_doc);
};

let tests = [
  (
    "PromptFactory",
    [
      test_case("reserved words are unusable", `Quick, check_reserved),
      test_case("non-reserved near-misses are usable", `Quick, check_usable),
      test_case("type-position keywords", `Quick, check_typ_position),
      test_case(
        "fn-definition sugar is statics-clean",
        `Quick,
        fn_sugar_clean,
      ),
      test_case("fn-definition sugar evaluates", `Quick, fn_sugar_evaluates),
      test_case(
        "documented examples are statics-clean",
        `Quick,
        doc_examples_clean,
      ),
      test_case(
        "shipped sample programs are statics-clean",
        `Quick,
        big_samples_clean,
      ),
    ],
  ),
];
