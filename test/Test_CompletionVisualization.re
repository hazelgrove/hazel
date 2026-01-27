open Alcotest;
open Haz3lcore;

/* Test infrastructure for completion visualization mockups.
 *
 * Format:
 *   - Middle dot (·) marks insertion points
 *   - `// ...` after 4 spaces shows what will be inserted at that line
 *   - One dot per insertion point (even if multiple completions there)
 *
 * Example:
 *   let x = 1·    // in ?
 */

let string_testable = testable(Fmt.string, String.equal);

/* Parse a string to a segment */
let must_parse = (s: string): Segment.t => {
  switch (Parser.to_zipper(s)) {
  | exception _ => failwith("Failed to parse: " ++ s)
  | None => failwith("Failed to parse: " ++ s)
  | Some(z) => Zipper.unselect_and_zip(~erase_buffer=true, z)
  };
};

/* Generate completion visualization mockup */
let visualize = (input: string): string => {
  let seg = must_parse(input);
  CompletionVisualization.mockup(seg);
};

/* === TEST CASES === */

type viz_test = {
  name: string,
  input: string,
  expected: string,
};

let test = (~name, ~input, ~expected): viz_test => {
  name,
  input,
  expected,
};

/* Phase 1: Simple single-line cases */
let simple_tests = [
  /* No completion needed - unchanged */
  test(
    ~name="complete let unchanged",
    ~input="let x = 1 in x",
    ~expected="let x = 1 in x",
  ),
  test(~name="variable unchanged", ~input="x", ~expected="x"),
  /* Simple trailing delimiters */
  test(
    ~name="let missing in",
    ~input="let x = 1",
    ~expected={|let x = 1·    // in ?|},
  ),
  test(
    ~name="fun missing arrow",
    ~input="fun x",
    /* x fills the pattern, so no preceding hole before -> */
    ~expected={|fun x·    // -> ?|},
  ),
  test(~name="open paren", ~input="(1 + 2", ~expected={|(1 + 2·    // )|}),
  test(~name="open bracket", ~input="[1, 2", ~expected={|[1, 2·    // ]|}),
  test(
    ~name="if missing else",
    ~input="if true then 1",
    ~expected={|if true then 1·    // else ?|},
  ),
  test(
    ~name="case missing end",
    ~input="case x | A => 1",
    ~expected={|case x | A => 1·    // end|},
  ),
];

/* Phase 2: Nested/multiple completions */
let nested_tests = [
  /* Both fun and let complete at same position */
  test(
    ~name="let with incomplete fun inside",
    ~input="let f = fun x",
    ~expected={|let f = fun x·    // -> ? in ?|},
  ),
  /* Nested lets */
  test(
    ~name="nested lets",
    ~input="let x = let y = 1",
    ~expected={|let x = let y = 1·    // in ? in ?|},
  ),
  /* Parens containing incomplete let.
   * Note: space between completions in offside is formatting choice. */
  test(
    ~name="let inside open paren",
    ~input="(let x = 1",
    ~expected={|(let x = 1·    // in ? )|},
  ),
  /* Multiple open parens */
  test(~name="two open parens", ~input="((1", ~expected={|((1·    // ) )|}),
];

/* Phase 2b: Complex cases - multiple insertion points, same line.
 * Note: All completions on a line are grouped into ONE offside comment
 * at the end of the line, ordered left-to-right. Dots show WHERE,
 * the offside shows WHAT (all of it). */
let complex_tests = [
  /* Two separate insertion points on same line:
   * - First let `let x = 1` inside complete paren needs `in ?`
   * - Second let `let y = 2` inside incomplete paren needs `in ?`
   * - Second paren needs `)`
   * All three completions listed at end. */
  test(
    ~name="two insertions on same line",
    ~input="(let x = 1) + (let y = 2",
    ~expected={|(let x = 1·) + (let y = 2·    // in ? in ? )|},
  ),
  /* Nested with sibling: complete inner, incomplete outer */
  test(
    ~name="complete inner with incomplete outer",
    ~input="(1 + 2) + (3",
    ~expected={|(1 + 2) + (3·    // )|},
  ),
  /* Multiple different incomplete constructs on same line:
   * - fun x needs `-> ?`
   * - let y = 1 needs `in ?`
   * - second paren needs `)` */
  test(
    ~name="fun and let incomplete on same line",
    ~input="(fun x) (let y = 1",
    ~expected={|(fun x·) (let y = 1·    // -> ? in ? )|},
  ),
  /* Deeply nested: all parens incomplete, all lets incomplete.
   * Completions listed innermost-to-outermost. */
  test(
    ~name="three levels nested",
    ~input="(let x = (let y = (let z = 1",
    ~expected={|(let x = (let y = (let z = 1·    // in ? ) in ? ) in ? )|},
  ),
];

/* Phase 3: Multi-line with partitioning */
let multiline_tests = [
  /* Column-0 content triggers partition */
  test(
    ~name="let then column-0 content",
    ~input={|let a = 1
a|},
    ~expected={|let a = 1·    // in ?
a|},
  ),
  /* Two lets on separate lines */
  test(
    ~name="two lets on separate lines",
    ~input={|let x = 1
let y = 2|},
    ~expected={|let x = 1·    // in ?
let y = 2·    // in ?|},
  ),
  /* Blank line partition - dot goes on the blank line */
  test(
    ~name="let then blank line then var",
    ~input={|let x = 1

y|},
    ~expected={|let x = 1
·    // in ?
y|},
  ),
  /* Multiple blank line partitions */
  test(
    ~name="three lets with blank lines",
    ~input={|let a = 1

let b = 2

let c = 3|},
    ~expected=
      {|let a = 1
·    // in ?
let b = 2
·    // in ?
let c = 3·    // in ?|},
  ),
  /* Mixed: complete let followed by incomplete */
  test(
    ~name="complete then incomplete with blank line",
    ~input={|let a = 1 in a

let b = 2|},
    ~expected={|let a = 1 in a

let b = 2·    // in ?|},
  ),
];

/* Phase 4: Indented content and relative indent */
let indent_tests = [
  /* Indented content - no partition, absorbed into let body */
  test(
    ~name="let with indented body",
    ~input={|let x = 1
  y|},
    /* The y becomes part of the let, grout inserted, then in ? at end */
    ~expected={|let x = 1
  y·    // in ?|},
  ),
  /* Same-indent triggers partition. */
  test(
    ~name="indented let then same-indent content",
    ~input={|fun x ->
    let
    y|},
    ~expected={|fun x ->
    let·    // = ? in ?
    y|},
  ),
  /* Inside complete fun body.
   * Child insertions are now captured and visualized. */
  test(
    ~name="incomplete fun in complete let body",
    ~input={|let f = fun x ->
  fun y
  body
in f(1)|},
    ~expected={|let f = fun x ->
  fun y·    // -> ?
  body
in f(1)|},
  ),
];

/* === TEST RUNNERS === */

let run_tests = (tests: list(viz_test)) =>
  tests
  |> List.map(({name, input, expected}) =>
       test_case(
         name,
         `Quick,
         () => {
           let output = visualize(input);
           check(string_testable, name, expected, output);
         },
       )
     );

let tests: list((string, list(Alcotest.test_case(unit)))) = [
  ("CompletionVisualization: simple", run_tests(simple_tests)),
  ("CompletionVisualization: nested", run_tests(nested_tests)),
  ("CompletionVisualization: complex", run_tests(complex_tests)),
  ("CompletionVisualization: multiline", run_tests(multiline_tests)),
  ("CompletionVisualization: indent", run_tests(indent_tests)),
];
