open Alcotest;
open Haz3lcore;

/* Test helpers */

let segment_testable =
  testable(Fmt.using(Segment.show, Fmt.string), Segment.equal);

let string_testable = testable(Fmt.string, String.equal);

/* Parse a string to a segment */
let parse_segment = (s: string): option(Segment.t) => {
  switch (Parser.to_zipper(s)) {
  | exception _ => None
  | None => None
  | Some(z) => Some(Zipper.unselect_and_zip(~erase_buffer=true, z))
  };
};

let must_parse = (s: string): Segment.t => {
  switch (parse_segment(s)) {
  | Some(seg) => seg
  | None => fail("Failed to parse: " ++ s)
  };
};

/* Print segment to string with holes shown as ? */
let print_seg = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", ~refractors=Id.Map.empty, seg);

/* Count incomplete tiles recursively */
let count_incomplete_deep = (seg: Segment.t): int =>
  Segment.incomplete_tiles_deep(seg) |> List.length;

/* === TEST INFRASTRUCTURE ===
 *
 * We test completion with two modes:
 * - insert_separators=true: Adds spaces where tokens would jam together (readable)
 * - insert_separators=false: No added whitespace (minimal, for semantics)
 *
 * Most tests use the readable version. A few tests verify both versions
 * produce structurally equivalent results.
 */

type completion_test = {
  name: string,
  input: string,
  expected: string,           /* Expected output with insert_separators=true */
  expected_no_sep: option(string), /* Expected without separators, if different */
};

/* Helper to create test that doesn't differ with/without separators */
let test = (~name, ~input, ~expected): completion_test => {
  name,
  input,
  expected,
  expected_no_sep: None,
};

/* Helper for tests where the no-separator version differs */
let test_sep = (~name, ~input, ~expected, ~expected_no_sep): completion_test => {
  name,
  input,
  expected,
  expected_no_sep: Some(expected_no_sep),
};

/* === PHASE 1 TESTS: Current Behavior / Baseline ===
 *
 * These verify our test infrastructure works with already-complete syntax.
 */

let baseline_tests = [
  test(~name="complete let unchanged", ~input="let x = 1 in x", ~expected="let x = 1 in x"),
  test(~name="variable unchanged", ~input="x", ~expected="x"),
  test(~name="binary op unchanged", ~input="1 + 2", ~expected="1 + 2"),
  test(~name="complete fun unchanged", ~input="fun x -> x", ~expected="fun x -> x"),
  test(~name="complete parens unchanged", ~input="(1 + 2)", ~expected="(1 + 2)"),
];

/* === PHASE 2 TESTS: Single Incomplete Tile (Trailing) ===
 *
 * Each test has one incomplete tile missing trailing delimiter(s).
 * The completion should:
 * 1. Add the missing delimiter(s)
 * 2. Add holes where the delimiter expects content
 * 3. Optionally add separator whitespace for readability
 */

let trailing_single_tests = [
  /* Let expressions */
  test_sep(
    ~name="let missing in",
    ~input="let x = 1",
    ~expected="let x = 1 in ?",
    ~expected_no_sep="let x = 1in?",
  ),
  test_sep(
    ~name="let missing = and in",
    ~input="let x",
    ~expected="let x = ? in ?",
    ~expected_no_sep="let x=?in?",
  ),

  /* Functions */
  test_sep(
    ~name="fun missing arrow",
    ~input="fun x",
    ~expected="fun x -> ?",
    ~expected_no_sep="fun x->?",
  ),

  /* Parentheses - no hole needed, convex-convex */
  test(
    ~name="open paren",
    ~input="(1",
    ~expected="(1)",
  ),
  test(
    ~name="open paren with expr",
    ~input="(1 + 2",
    ~expected="(1 + 2)",
  ),

  /* List literals - no hole needed */
  test(
    ~name="open bracket",
    ~input="[1",
    ~expected="[1]",
  ),
  test(
    ~name="open bracket multi",
    ~input="[1, 2",
    ~expected="[1, 2]",
  ),

  /* If expressions */
  test_sep(
    ~name="if missing else",
    ~input="if true then 1",
    ~expected="if true then 1 else ?",
    ~expected_no_sep="if true then 1else?",
  ),
  test_sep(
    ~name="if missing then and else",
    ~input="if true",
    ~expected="if true then ? else ?",
    ~expected_no_sep="if truethen?else?",
  ),

  /* Case expressions - end is convex, no hole */
  test_sep(
    ~name="case missing end",
    ~input="case x | A => 1",
    ~expected="case x | A => 1 end",
    ~expected_no_sep="case x | A => 1end",
  ),

  /* Type alias */
  test_sep(
    ~name="type missing in",
    ~input="type t = Int",
    ~expected="type t = Int in ?",
    ~expected_no_sep="type t = Intin?",
  ),
];

/* === PHASE 3 TESTS: Multiple Incomplete Tiles ===
 *
 * When multiple tiles are incomplete, we complete from the outside in
 * (or left to right at the same level), then recurse into children.
 */

let multi_incomplete_tests = [
  /* Nested: let containing incomplete fun */
  test_sep(
    ~name="let with incomplete fun inside",
    ~input="let f = fun x",
    ~expected="let f = fun x -> ? in ?",
    ~expected_no_sep="let f = fun x->?in?",
  ),

  /* Sibling: inner let inside outer let's definition */
  test_sep(
    ~name="nested lets",
    ~input="let x = let y = 1",
    ~expected="let x = let y = 1 in ? in ?",
    ~expected_no_sep="let x = let y = 1in?in?",
  ),

  /* Parens containing incomplete let */
  test_sep(
    ~name="let inside open paren",
    ~input="(let x = 1",
    ~expected="(let x = 1 in ?)",
    ~expected_no_sep="(let x = 1in?)",
  ),

  /* Fun inside fun - outer is complete if arrow is there */
  test_sep(
    ~name="incomplete fun inside complete fun",
    ~input="fun x -> fun y",
    ~expected="fun x -> fun y -> ?",
    ~expected_no_sep="fun x -> fun y->?",
  ),

  /* Deeply nested */
  test_sep(
    ~name="three levels deep",
    ~input="let x = (let y = fun z",
    ~expected="let x = (let y = fun z -> ? in ?) in ?",
    ~expected_no_sep="let x = (let y = fun z->?in?)in?",
  ),

  /* Multiple at same level - two open parens */
  test(
    ~name="two open parens",
    ~input="((1",
    ~expected="((1))",
  ),
];

/* === PHASE 4 TESTS: Linebreak Sensitivity ===
 *
 * Linebreaks signal user intent about form boundaries.
 * Following Dump.re: stop completion at linebreaks.
 */

let linebreak_tests = [
  /* Linebreak stops the let body from consuming y */
  test_sep(
    ~name="let then linebreak then var",
    ~input="let x = 1\ny",
    ~expected="let x = 1 in ?\ny",
    ~expected_no_sep="let x = 1in?\ny",
  ),

  /* Blank line (double linebreak) definitely stops */
  test_sep(
    ~name="let then blank line then var",
    ~input="let x = 1\n\ny",
    ~expected="let x = 1 in ?\n\ny",
    ~expected_no_sep="let x = 1in?\n\ny",
  ),

  /* Multiple lets with linebreaks */
  test_sep(
    ~name="two lets on separate lines",
    ~input="let x = 1\nlet y = 2",
    ~expected="let x = 1 in ?\nlet y = 2 in ?",
    ~expected_no_sep="let x = 1in?\nlet y = 2in?",
  ),
];

/* === PHASE 5 TESTS: Leading Delimiters (Future) ===
 *
 * Leading delimiters (`)` without `(`) - design TBD.
 * For now, just verify they don't crash.
 */

let leading_tests = [
  /* These might stay incomplete or be handled specially */
  test(
    ~name="unmatched close paren",
    ~input="1)",
    ~expected="1)",  /* TBD - might change */
  ),
];

/* === PHASE 6 TESTS: Middle Delimiters (Future) ===
 *
 * Missing middle delimiter (e.g., `let x in 1` without `=`).
 * Heuristic: insert before the next present shard.
 */

let middle_tests = [
  test_sep(
    ~name="let missing equals",
    ~input="let x in 1",
    ~expected="let x = ? in 1",
    ~expected_no_sep="let x=?in 1",
  ),
];

/* === TEST RUNNERS ===
 *
 * For now, we only run baseline tests until completion is implemented.
 * The test cases above serve as documentation and specification.
 */

let run_baseline_tests =
  baseline_tests
  |> List.map(({name, input, expected, _}) =>
       test_case(name, `Quick, () => {
         let seg = must_parse(input);
         let output = print_seg(seg);
         check(string_testable, name, expected, output);
       })
     );

/* TODO: Enable once CanonicalCompletion.complete_segment is implemented
let run_completion_tests = (tests: list(completion_test), ~insert_separators: bool) =>
  tests
  |> List.map(({name, input, expected, expected_no_sep}) => {
       let expected_output =
         if (insert_separators) {
           expected;
         } else {
           Option.value(expected_no_sep, ~default=expected);
         };
       test_case(name, `Quick, () => {
         let seg = must_parse(input);
         let result = CanonicalCompletion.complete_segment_deep(~insert_separators, seg);
         let output = print_seg(result.completed_seg);
         check(string_testable, name, expected_output, output);

         // Verify no incomplete tiles remain
         check(int, "no incomplete", 0, count_incomplete_deep(result.completed_seg));
       });
     });
*/

let tests: list((string, list(Alcotest.test_case(unit)))) = [
  ("CanonicalCompletion: baseline", run_baseline_tests),
  /* TODO: Enable as implementation progresses:
     ("CanonicalCompletion: trailing (with sep)", run_completion_tests(trailing_single_tests, ~insert_separators=true)),
     ("CanonicalCompletion: trailing (no sep)", run_completion_tests(trailing_single_tests, ~insert_separators=false)),
     ("CanonicalCompletion: multi-incomplete", run_completion_tests(multi_incomplete_tests, ~insert_separators=true)),
     ("CanonicalCompletion: linebreaks", run_completion_tests(linebreak_tests, ~insert_separators=true)),
  */
];
