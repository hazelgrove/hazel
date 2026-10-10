/* How many times statics analyzes each expression, over the corpus.

   A one-pass checker analyzes every expression once, so its time is
   linear in the program. A rule that re-analyzes a subterm breaks that,
   and re-analyses nested in one another multiply: #2603 removed passes
   that made nested let-bound functions cost 4^depth, and #2651 a module
   definition analyzed twice. Each was found by profiling a slide that had
   become slow. This test finds the next one when it is introduced: it
   counts analyses with Statics.pass_counts (what `hazel analyze
   --count-passes` prints), and fails when a program needs more than its
   budget.

   Three multipliers are known, each x3, and they multiply when nested
   (issue #2609, finding 10):
   - a recursive let: its definition takes a probe and two more passes;
   - a let that shadows a name already in scope, such as a program's own
     `find` over the builtin: #2603's one-pass shortcut is skipped;
   - a label access on an unannotated lambda parameter, `fun e -> e.label`.
   The corpus's B2T2 tables programs do all three at once, which is why
   they have budgets of their own below. The Known cases pin each
   multiplier at x3: when one is fixed, its case fails, and its budget and
   the corpus budgets come down with it.

   On a failure, `hazel analyze --count-passes <program>` prints the
   histogram and the most-analyzed source lines. */
open Alcotest;
open Haz3lcore;
open Language;

/* Analyses of each expression of [term], by its id: the most any one
   expression took, and the total over the number of expressions.
   uexp_to_info_map, not Statics.mk, which memoizes on the term. */
type passes = {
  most: int,
  total: int,
  expressions: int,
};

let passes = (term: Exp.t): passes => {
  let counts = Hashtbl.create(4096);
  Statics.pass_counts := Some(counts);
  Fun.protect(
    ~finally=() => Statics.pass_counts := None,
    () =>
      ignore(
        Statics.uexp_to_info_map(
          ~ctx=Builtins.ctx_init(Some(Operators.default_mode)),
          ~ancestors=[],
          term,
          Id.Map.empty,
        ),
      ),
  );
  Hashtbl.fold(
    (_, (n, _), p) =>
      {
        most: max(p.most, n),
        total: p.total + n,
        expressions: p.expressions + 1,
      },
    counts,
    {
      most: 0,
      total: 0,
      expressions: 0,
    },
  );
};

/* Parsed as the app and `hazel analyze` load a program. */
let term_of_text = (src: string): option(Exp.t) =>
  PersistentZipper.parse_text(~source="statics-passes", ~root=Exp, src)
  |> Option.map(z => MakeTerm.from_zip_for_sem(z, ~root=Exp).term);

/* Every .hz program under hazel-programs but mega/, with its path.
   Paths resolve from the repo root (run_node.sh) or one level down. */
let corpus = (): list((string, string)) => {
  let read = path =>
    switch (open_in_bin(path)) {
    | ic =>
      let s = really_input_string(ic, in_channel_length(ic));
      close_in(ic);
      Some(s);
    | exception _ => None
    };
  let rec find = dir =>
    switch (Sys.readdir(dir)) {
    | entries =>
      Array.to_list(entries)
      |> List.concat_map(entry => {
           let path = Filename.concat(dir, entry);
           switch (Sys.is_directory(path)) {
           | true => entry == "mega" ? [] : find(path)
           | false => Filename.check_suffix(entry, ".hz") ? [path] : []
           | exception _ => []
           };
         })
    | exception _ => []
    };
  let root =
    Sys.file_exists("hazel-programs")
      ? "hazel-programs" : "../hazel-programs";
  find(root)
  |> List.sort(compare)
  |> List.filter_map(path => Option.map(src => (path, src), read(path)));
};

/* KNOWN MULTIPLIERS: each alone, and two nested. */

let analyzed = (src: string): passes =>
  switch (term_of_text(src)) {
  | Some(term) => passes(term)
  | None => fail("did not parse: " ++ src)
  };

let known = (name, src, expected, why) =>
  test_case(name, `Quick, () =>
    check(
      int,
      why ++ " -- if this went down, lower the budgets too",
      expected,
      analyzed(src).most,
    )
  );

let known_cases = [
  test_case("functions, data and lets: once each", `Quick, () =>
    check(
      int,
      "a one-pass checker",
      1,
      analyzed(
        "let g = fun (h : Int -> Bool) -> h(1) in\n"
        ++ "let my_find = fun (xs, p) -> case xs | [] => false | x :: _ => p(x) end in\n"
        ++ "let r = (label = \"a\", value = 1) in\n"
        ++ "let p = fun (e : (label = String, value = Int)) -> e.label == \"a\" in\n"
        ++ "g(fun e -> e == 1) && my_find([1, 2], fun e -> e == 1) && p(r)",
      ).
        most,
    )
  ),
  test_case(
    "let-bound functions nested 6 deep: once each (#2603)",
    `Quick,
    () => {
      let rec nest = d =>
        d == 0
          ? "x"
          : Printf.sprintf(
              "(let f%d = fun x -> %s in f%d(1))",
              d,
              nest(d - 1),
              d,
            );
      check(
        int,
        "no 4^depth",
        1,
        analyzed("let x = 1 in " ++ nest(6)).most,
      );
    },
  ),
  /* The module literal node itself is visited twice, but its members once
     each: the second visit does not descend, so it adds one analysis and
     multiplies nothing. */
  test_case(
    "a module definition: its members once (#2651)",
    `Quick,
    () => {
      let p = analyzed("module M = { let a = 1; let b = a + 1 } in M.b");
      check(
        int,
        "module M = def: one extra visit, to the literal only",
        1,
        p.total - p.expressions,
      );
    },
  ),
  known(
    "a recursive let: x3",
    "let f : Int -> Int = fun n -> if n <= 0 then 0 else f(n - 1) in f(3)",
    3,
    "a recursive definition takes a probe and two more passes",
  ),
  known(
    "a let shadowing a builtin: x3",
    "let find = fun (xs, p) -> case xs | [] => false | x :: _ => p(x) end in\n"
    ++ "find([1, 2], fun e -> e == 1)",
    3,
    "shadowing skips #2603's one-pass shortcut",
  ),
  known(
    "a label on an unannotated parameter: x3",
    "let r = (label = \"a\", value = 1) in\n"
    ++ "let p = fun e -> e.label == \"a\" in p(r)",
    3,
    "label inference re-analyzes the access",
  ),
  known(
    "recursive lets nested 3 deep: 3^3",
    "let x = 1 in (let r0 : Int -> Int = fun x -> if x <= 0 then 0 else r0(x - 1) + "
    ++ "(let r1 : Int -> Int = fun x -> if x <= 0 then 0 else r1(x - 1) + "
    ++ "(let r2 : Int -> Int = fun x -> if x <= 0 then 0 else r2(x - 1) + x in r2(2)) in r1(2)) in r0(2))",
    27,
    "nested recursive lets multiply",
  ),
];

/* THE CORPUS: every program under hazel-programs (but mega/). */

/* The most analyses of one expression a program may need. Five covers
   everything outside the B2T2 tables programs today; a new x2 anywhere
   common pushes a x3 program to 6 and fails here. */
let default_most = 5;

/* Programs over the default, with why. All are B2T2 tables programs: a
   `fun e -> e.label == c` (x3) passed to the program's own `find`, which
   shadows the builtin (x3), some inside a recursive function (x3 more). */
let budgets = [
  ("example-programs-phackingheterogeneous.hz", 12),
  ("example-programs-phackinghomogeneous.hz", 12),
  ("example-programs-dot-product.hz", 9),
  ("example-programs-groupbyretentive.hz", 9),
  ("example-programs-groupbysubtractive.hz", 9),
  ("table-api-aggregate-count.hz", 9),
  ("table-api-aggregate-pivottable.hz", 9),
  ("table-api-ordering-orderby.hz", 9),
  ("table-api-utilities-find.hz", 9),
  ("table-api-utilities-flatten.hz", 9),
  ("table-api-utilities-groupbyretentive.hz", 9),
  ("tables.hz", 9),
  ("table-api-missing-values-completecases.hz", 6),
];

/* Analyses per expression, over a whole program: today at most 2.08
   (table-api-aggregate-pivottable.hz). A pass added to a common rule
   raises every program's ratio, even where no one expression passes its
   budget. */
let max_ratio = 2.5;

let budget_of = (path: string): int =>
  switch (List.assoc_opt(Filename.basename(path), budgets)) {
  | Some(n) => n
  | None => default_most
  };

let corpus_case = () => {
  let programs = corpus();
  check(bool, "the corpus was found", true, List.length(programs) > 100);
  let over =
    List.filter_map(
      ((path, src)) =>
        switch (term_of_text(src)) {
        | None => Some(path ++ ": did not parse")
        | Some(term) =>
          let p = passes(term);
          let ratio =
            float_of_int(p.total) /. float_of_int(max(p.expressions, 1));
          let budget = budget_of(path);
          if (p.most > budget) {
            Some(
              Printf.sprintf(
                "%s: an expression analyzed %dx, budget %dx",
                path,
                p.most,
                budget,
              ),
            );
          } else if (ratio > max_ratio) {
            Some(
              Printf.sprintf(
                "%s: %.2f analyses per expression, budget %.2f",
                path,
                ratio,
                max_ratio,
              ),
            );
          } else {
            None;
          };
        },
      programs,
    );
  check(
    list(string),
    "over budget (see `hazel analyze --count-passes` for the lines)",
    [],
    over,
  );
};

let tests = (
  "StaticsPasses",
  known_cases @ [test_case("the corpus, within budget", `Slow, corpus_case)],
);
