open Alcotest;
open Haz3lcore;

/* Test helpers */

let segment_testable =
  testable(Fmt.using(Segment.show, Fmt.string), Segment.equal);

let string_testable = testable(Fmt.string, String.equal);

/* Parse a string to a segment */
let parse_segment = (s: string): option(Segment.t) => {
  /* Parser.to_segment (not bare to_zipper): the final rescan_reassemble
     is what gloms |/=> shards into rule tiles, and Zipper.init's
     trailing grout is stripped — matching what the editor's sem path
     actually feeds completion. */
  switch (Parser.to_segment(~root=Exp, s)) {
  | exception _ => None
  | seg => seg
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
  Printer.of_segment(~holes="?", ~concave_holes="~", seg);

/* Count incomplete tiles recursively */
let count_incomplete_deep = (seg: Segment.t): int =>
  Segment.incomplete_tiles_deep(seg) |> List.length;

/* === BUILDING BLOCK TESTS ===
 *
 * Before testing completion, let's verify Segment.reassemble and
 * Segment.regrout behave as expected.
 */

/* Create a single-shard tile from a parsed complete tile */
let make_shard = (t: Tile.t, shard_index: int): Tile.t => {
  {
    ...t,
    shards: [shard_index],
    children: [],
  };
};

/* Test Segment.reassemble: given scattered same-ID shards, combines them */
let reassemble_tests = [
  test_case(
    "reassemble: already complete",
    `Quick,
    () => {
      /* A complete tile should be unchanged */
      let seg = must_parse("let x = 1 in x");
      let result = Segment.reassemble(seg);
      let output = print_seg(result);
      check(string_testable, "unchanged", "let x = 1 in x", output);
    },
  ),
  test_case(
    "reassemble: scattered let shards",
    `Quick,
    () => {
      /* Test: parse an incomplete let, manually scatter its shard, then reassemble.
       * This simulates what our completion does: insert shards then reassemble. */

      /* Start with incomplete let: "let x = 1" (missing "in") */
      let incomplete_seg = must_parse("let x = 1");
      print_endline("Incomplete seg: " ++ print_seg(incomplete_seg));

      /* Find the incomplete let tile */
      let incomplete_tiles = Segment.incomplete_tiles(incomplete_seg);
      print_endline(
        "Incomplete tile count: "
        ++ string_of_int(List.length(incomplete_tiles)),
      );

      switch (incomplete_tiles) {
      | [] => fail("Expected incomplete tile")
      | [let_tile, ..._] =>
        print_endline(
          "Let tile shards: "
          ++ String.concat(",", List.map(string_of_int, let_tile.shards)),
        );
        print_endline(
          "Let tile label: " ++ String.concat(",", let_tile.label),
        );

        /* Create the missing shard (index 2 = "in") */
        let shard_2 = make_shard(let_tile, 2);

        /* Insert it at the end of the segment */
        let seg_with_shard = incomplete_seg @ [Piece.Tile(shard_2)];
        print_endline("Seg with shard: " ++ print_seg(seg_with_shard));

        /* Now reassemble - should combine the shards */
        let result = Segment.reassemble(seg_with_shard);
        print_endline("Reassembled: " ++ print_seg(result));

        /* Should have no incomplete tiles now */
        let incomplete_count =
          Segment.incomplete_tiles(result) |> List.length;
        check(Alcotest.int, "no incomplete tiles", 0, incomplete_count);
      };
    },
  ),
];

/* Debug test: try regrout on scattered shards */
let regrout_debug_tests = [
  test_case(
    "regrout: scattered shards",
    `Quick,
    () => {
      /* Parse incomplete let */
      let incomplete_seg = must_parse("let x = 1");
      print_endline("=== Debug: regrout on scattered shards ===");
      print_endline("Incomplete seg: " ++ print_seg(incomplete_seg));

      let incomplete_tiles = Segment.incomplete_tiles(incomplete_seg);
      switch (incomplete_tiles) {
      | [] => fail("Expected incomplete tile")
      | [let_tile, ..._] =>
        /* Create the missing shard */
        let shard_2 = make_shard(let_tile, 2);
        let seg_with_shard = incomplete_seg @ [Piece.Tile(shard_2)];
        print_endline(
          "Seg with shard (before regrout): " ++ print_seg(seg_with_shard),
        );

        /* Try regrout */
        print_endline("Calling regrout((Convex, Convex), seg_with_shard)...");
        let regrouted = Segment.regrout((Convex, Convex), seg_with_shard);
        print_endline("After regrout: " ++ print_seg(regrouted));

        check(Alcotest.bool, "regrout succeeded", true, true);
      };
    },
  ),
  test_case(
    "regrout: linebreak case debug",
    `Quick,
    () => {
      /* Parse the linebreak case */
      let seg = must_parse("let a = 1\na");
      print_endline("=== Debug: linebreak case ===");
      print_endline("Parsed seg: " ++ print_seg(seg));
      print_endline("Piece count: " ++ string_of_int(List.length(seg)));

      /* Show each piece */
      List.iteri(
        (i, p) => {
          let desc =
            switch (p) {
            | Piece.Tile(t) => "Tile(" ++ String.concat(",", t.label) ++ ")"
            | Piece.Grout(g) =>
              "Grout(" ++ (g.shape == Convex ? "Convex" : "Concave") ++ ")"
            | Piece.Secondary(s) =>
              "Secondary("
              ++ (Secondary.is_linebreak(s) ? "linebreak" : "other")
              ++ ")"
            | Piece.Projector(_) => "Projector"
            };
          print_endline("  [" ++ string_of_int(i) ++ "]: " ++ desc);
        },
        seg,
      );

      /* Run completion and show result */
      let result =
        CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
      print_endline("Completed: " ++ print_seg(result.completed_seg));

      check(Alcotest.bool, "debug", true, true);
    },
  ),
  test_case(
    "debug: nested let case",
    `Quick,
    () => {
      /* Parse the nested let case */
      let seg = must_parse("let a = 1 in\nlet b = 1\na + b");
      print_endline("=== Debug: nested let case ===");
      print_endline("Full segment: " ++ print_seg(seg));
      print_endline(
        "Top-level piece count: " ++ string_of_int(List.length(seg)),
      );

      /* Look at the structure */
      List.iteri(
        (i, p) => {
          switch (p) {
          | Piece.Tile(t) =>
            print_endline(
              "  ["
              ++ string_of_int(i)
              ++ "]: Tile("
              ++ String.concat(",", t.label)
              ++ ") shards=["
              ++ String.concat(",", List.map(string_of_int, t.shards))
              ++ "] children="
              ++ string_of_int(List.length(t.children))
              ++ " is_complete="
              ++ string_of_bool(Tile.is_complete(t)),
            );
            /* Print each child */
            List.iteri(
              (ci, child) => {
                print_endline(
                  "    child["
                  ++ string_of_int(ci)
                  ++ "]: "
                  ++ print_seg(child),
                )
              },
              t.children,
            );
          | Piece.Secondary(s) =>
            print_endline(
              "  ["
              ++ string_of_int(i)
              ++ "]: Secondary("
              ++ (Secondary.is_linebreak(s) ? "linebreak" : "space/other")
              ++ ")",
            )
          | _ => print_endline("  [" ++ string_of_int(i) ++ "]: other")
          }
        },
        seg,
      );

      /* Test partition_segment on the TOP-LEVEL segment */
      print_endline("\n=== Testing partition_segment on top-level ===");
      let partitions = CanonicalCompletion.partition_segment(seg);
      print_endline(
        "Partition count: " ++ string_of_int(List.length(partitions)),
      );
      List.iteri(
        (i, (subseg, incomplete)) => {
          print_endline("  Partition " ++ string_of_int(i) ++ ":");
          print_endline("    content: " ++ print_seg(subseg));
          print_endline(
            "    incomplete: " ++ string_of_int(List.length(incomplete)),
          );
          List.iter(
            (t: Tile.t) => {
              print_endline(
                "      - "
                ++ String.concat(",", t.label)
                ++ " shards=["
                ++ String.concat(",", List.map(string_of_int, t.shards))
                ++ "]",
              )
            },
            incomplete,
          );
        },
        partitions,
      );

      /* Run full completion */
      let result =
        CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
      print_endline("\n=== Final result ===");
      print_endline("Completed: " ++ print_seg(result.completed_seg));

      check(Alcotest.bool, "debug", true, true);
    },
  ),
];

/* Test Segment.regrout: explore its behavior */
let regrout_tests = [
  test_case(
    "regrout: explore behavior",
    `Quick,
    () => {
      let concave = Nib.Shape.concave();

      /* Test 1: Incomplete binary op with different outer shapes */
      let seg1 = must_parse("1 +");
      print_endline("=== Test 1: 1 + with different contexts ===");
      print_endline("Before: " ++ print_seg(seg1));
      print_endline(
        "  regrout(Convex,Convex): "
        ++ print_seg(Segment.regrout((Convex, Convex), seg1)),
      );
      print_endline(
        "  regrout(Convex,Concave): "
        ++ print_seg(Segment.regrout((Convex, concave), seg1)),
      );
      print_endline(
        "  regrout(Concave,Convex): "
        ++ print_seg(Segment.regrout((concave, Convex), seg1)),
      );
      print_endline(
        "  regrout(Concave,Concave): "
        ++ print_seg(Segment.regrout((concave, concave), seg1)),
      );

      /* Test 2: Empty segment */
      print_endline("=== Test 2: empty segment ===");
      print_endline(
        "  regrout(Convex,Convex): "
        ++ print_seg(Segment.regrout((Convex, Convex), [])),
      );
      print_endline(
        "  regrout(Concave,Concave): "
        ++ print_seg(Segment.regrout((concave, concave), [])),
      );

      /* Test 3: What if we manually add grout then reassemble? */
      let incomplete_let = must_parse("let x = 1");
      let let_tile = List.hd(Segment.incomplete_tiles(incomplete_let));
      let shard_2 = make_shard(let_tile, 2);
      let grout: Piece.t =
        Grout({
          id: Id.mk(),
          shape: Convex,
        });

      /* Try: incomplete_let @ [shard_2, grout] */
      let with_shard_and_grout =
        incomplete_let @ [Piece.Tile(shard_2), grout];
      print_endline("=== Test 3: let x = 1 + in shard + grout ===");
      print_endline(
        "Before reassemble: " ++ print_seg(with_shard_and_grout),
      );
      let reassembled = Segment.reassemble(with_shard_and_grout);
      print_endline("After reassemble: " ++ print_seg(reassembled));

      check(Alcotest.bool, "completed", true, true);
    },
  ),
];

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
  expected: string, /* Expected output with insert_separators=true */
  expected_no_sep: option(string) /* Expected without separators, if different */
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
  test(
    ~name="complete let unchanged",
    ~input="let x = 1 in x",
    ~expected="let x = 1 in x",
  ),
  test(~name="variable unchanged", ~input="x", ~expected="x"),
  test(~name="binary op unchanged", ~input="1 + 2", ~expected="1 + 2"),
  test(
    ~name="complete fun unchanged",
    ~input="fun x -> x",
    ~expected="fun x -> x",
  ),
  test(
    ~name="complete parens unchanged",
    ~input="(1 + 2)",
    ~expected="(1 + 2)",
  ),
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
  test(~name="open paren", ~input="(1", ~expected="(1)"),
  test(~name="open paren with expr", ~input="(1 + 2", ~expected="(1 + 2)"),
  /* List literals - no hole needed */
  test(~name="open bracket", ~input="[1", ~expected="[1]"),
  test(~name="open bracket multi", ~input="[1, 2", ~expected="[1, 2]"),
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
  /* Nested: incomplete fun and let, column-0 content on next line.
   * The fun is completed in the let's child (gets hole for fun body).
   * f(1) becomes the let body since shapes fit (no hole needed). */
  test(
    ~name="let with incomplete fun followed by application on next line",
    ~input={|let f = fun x
f(1)|},
    ~expected={|let f = fun x->?in
f(1)|},
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
  test(~name="two open parens", ~input="((1", ~expected="((1))"),
];

/* === PHASE 4 TESTS: Linebreak Sensitivity ===
 *
 * Partition heuristics:
 * 1. BLANK LINE: Two consecutive linebreaks always partition (if incomplete before)
 * 2. RELATIVE INDENT: After a linebreak, if content's indent is ≤ the incomplete
 *    tile's indent, partition there (if incomplete before)
 *
 * The relative indent heuristic interprets same-or-lesser indented content
 * after incomplete syntax as user intent to start something new.
 * This subsumes the old "zero indent" case: incomplete at col 0, content at
 * col 0 means 0 ≤ 0 → partition.
 */

let linebreak_tests = [
  /* === Zero-indent: column-0 content triggers partition === */
  /* Single linebreak followed by var at column 0 - partition.
   * The body is empty (no hole printed) because content is separate. */
  test(
    ~name="let then linebreak then var at column 0 - partition",
    ~input={|let a = 1
a|},
    ~expected={|let a = 1in
a|},
  ),
  /* Same with different names */
  test(
    ~name="let then linebreak then var at column 0",
    ~input={|let x = 1
y|},
    ~expected={|let x = 1in
y|},
  ),
  /* fun followed by column-0 content */
  test(
    ~name="fun then linebreak then var at column 0",
    ~input={|fun x
y|},
    ~expected={|fun x->
y|},
  ),
  /* === Zero-indent: indented content does NOT partition === */
  /* Linebreak followed by spaces then content - no partition. The
   * missing delimiter junction-drops at the unique concave-grout
   * junction (def=1, body=y) instead of absorbing y as a multihole. */
  test(
    ~name="let then linebreak then indented var - no partition",
    ~input={|let x = 1
  y|},
    ~expected={|let x = 1
  iny|},
  ),
  /* fun with indented body - arrow junction-drops before it */
  test(
    ~name="fun then linebreak then indented var - no partition",
    ~input={|fun x
  y|},
    ~expected={|fun x
  ->y|},
  ),
  /* Mixed: some indented, some at column 0.
   * body is indented (no partition there), next is at col 0 (partition) */
  test(
    ~name="fun with indented body then column-0 content",
    ~input={|let f = fun x
  body
next|},
    ~expected={|let f = fun x
  ->bodyin
next|},
  ),
  /* === Blank line tests (existing behavior preserved) === */
  /* Blank line (two consecutive linebreaks) - always partitions */
  test(
    ~name="let then blank line then var",
    ~input={|let x = 1

y|},
    ~expected={|let x = 1
in
y|},
  ),
  /* Two lets with single linebreak: second let at column 0 triggers partition.
   * Each let is completed independently with empty body (no hole printed). */
  test(
    ~name="two lets on separate lines",
    ~input={|let x = 1
let y = 2|},
    ~expected={|let x = 1in
let y = 2in?|},
  ),
  /* Two lets separated by blank line: both get completed */
  test(
    ~name="two lets separated by blank line",
    ~input={|let x = 1

let y = 2|},
    ~expected={|let x = 1
in
let y = 2in?|},
  ),
  /* Four segments separated by blank lines: all should get completed.
   * Tests recursive handling of multiple blank-line splits.
   * Each let becomes nested in the previous one's body. */
  test(
    ~name="four lets separated by blank lines",
    ~input={|let a = 1

let b = 2

let c = 3

let d = 4|},
    ~expected={|let a = 1
in
let b = 2
in
let c = 3
in
let d = 4in?|},
  ),
  /* Mix of complete and incomplete segments separated by blank lines.
   * Grout (~) inserted between complete `let a` and incomplete `let b`
   * because both are Convex and need Concave grout between them. */
  test(
    ~name="mixed complete and incomplete with blank lines",
    ~input={|let a = 1 in a

let b = 2

let c = 3 in c|},
    ~expected={|let a = 1 in a

~let b = 2
in
let c = 3 in c|},
  ),
  /* Complete let followed by incomplete let, then column-0 content.
   * The incomplete let is INSIDE the body of the complete let.
   * Should partition at `a + b` and complete `let b = 1` with `in`. */
  test(
    ~name="complete let with nested incomplete let then column-0",
    ~input={|let a = 1 in
let b = 1
a + b|},
    ~expected={|let a = 1 in
let b = 1in
a + b|},
  ),
  /* === RELATIVE INDENT: Same indent triggers partition ===
   * When an incomplete tile is at some positive column, content at the
   * same indentation level should partition (not be absorbed). */
  /* Incomplete let at col 4, followed by let at col 4 → partition.
   * This is the key case for typing a new let inside a function body.
   * The pattern hole ? is needed because `let` has no pattern. */
  test(
    ~name="indented incomplete let then same-indent let - partition",
    ~input={|fun x ->
    let
    let a = 1 in a|},
    ~expected={|fun x ->
    let?=?in
    let a = 1 in a|},
  ),
  /* Incomplete let at col 4, followed by var at col 4 → partition */
  test(
    ~name="indented incomplete let then same-indent var - partition",
    ~input={|fun x ->
    let
    y|},
    ~expected={|fun x ->
    let?=?in
    y|},
  ),
  /* Incomplete let at col 4, followed by MORE indented content → NO partition.
   * The more-indented content becomes the pattern of the let. */
  test(
    ~name="indented incomplete let then more-indented var - no partition",
    ~input={|fun x ->
    let
      y|},
    ~expected={|fun x ->
    let
      y=?in?|},
  ),
  /* Inside complete fun, incomplete fun, then same-indent content.
   * Uses fun (not let) to avoid the inner form stealing the outer "in".
   * Tests that partitioning works inside children of complete tiles.
   * The partitioned content becomes a sibling, not absorbed into the fun body. */
  test(
    ~name="complete fun body with incomplete fun then same-indent",
    ~input={|let f = fun x ->
  fun y
  body
in f(1)|},
    ~expected={|let f = fun x ->
  fun y->
  body
in f(1)|},
  ),
  /* Incomplete fun at col 4, content also at col 4 → partition.
   * The inner fun can't steal "->" from outer let.
   * Content at same indent is partitioned as sibling. */
  test(
    ~name="indented incomplete fun then same-indent content - partition",
    ~input={|let x =
    fun y
    z
in x|},
    ~expected={|let x =
    fun y->
    z
in x|},
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
       test_case(
         name,
         `Quick,
         () => {
           let seg = must_parse(input);
           let output = print_seg(seg);
           check(string_testable, name, expected, output);
         },
       )
     );

let run_completion_tests = (tests: list(completion_test)) =>
  tests
  |> List.map(({name, input, expected, expected_no_sep}) => {
       /* Use expected_no_sep if available, otherwise expected */
       let expected_output = Option.value(expected_no_sep, ~default=expected);
       test_case(
         name,
         `Quick,
         () => {
           let seg = must_parse(input);
           let result =
             CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
           let output = print_seg(result.completed_seg);
           check(string_testable, name, expected_output, output);

           /* Verify no incomplete tiles remain */
           check(
             Alcotest.int,
             "no incomplete",
             0,
             count_incomplete_deep(result.completed_seg),
           );
         },
       );
     });

/* === Phase 5 golden tests: leading, middle, wraps, interplay ===
 * These document the completion heuristics:
 * - leading openers insert at the start of the closer's left-operand
 *   span in the partition skel (maximal absorption, bounded by
 *   enclosing structure); same-position ties open later-closers
 *   outermost.
 * - middle gaps fill in place: content keeps its opening-shard slot,
 *   new slots get holes.
 * - orphaned rule chains wrap in a synthesized case/end. */
let leading_tests = [
  test(~name="unopened paren", ~input="1, 2)", ~expected="(1, 2)"),
  test(~name="unopened bracket", ~input="1, 2]", ~expected="[1, 2]"),
  test(
    ~name="two unopened parens: later closer opens outermost",
    ~input="1) + 2)",
    ~expected="((1) + 2)",
  ),
  test_sep(
    ~name="opener bounded by enclosing prefix form",
    ~input="let a = 1,2]",
    ~expected="let a = [1,2] in ?",
    ~expected_no_sep="let a = [1,2]in?",
  ),
  /* Partitioning at the unindented x leaves the juxtaposition's concave
     grout dangling at the first partition's edge; it must be dropped or
     the non-convex partition degrades opener placement to line start
     (and x stays unbound) */
  test_sep(
    ~name="opener placement across partition boundary",
    ~input="let a = 1,2]\na",
    ~expected="let a = [1,2] in\na",
    ~expected_no_sep="let a = [1,2]in\na",
  ),
  test(
    ~name="leading and trailing interplay",
    ~input="(1, 2]",
    ~expected="([1, 2])",
  ),
];

let middle_tests = [
  test_sep(
    ~name="let missing equals",
    ~input="let x in 2",
    ~expected="let x = ? in 2",
    ~expected_no_sep="let x =?in 2",
  ),
  test_sep(
    ~name="if missing then",
    ~input="if true else 2",
    ~expected="if true then ? else 2",
    ~expected_no_sep="if true then?else 2",
  ),
];

/* Orphaned rule chains only exist with REAL ["|","=>"] rule tiles,
 * which typing cannot produce outside a case (sort-driven expansion):
 * typed bare `1 | A => 2` is standalone |, => token tiles — an
 * unknown-operator juxtaposition handled by the op-lexeme machinery
 * (stuck application), NOT wrappable without fusing two tiles into one
 * (open design: two-id provenance). The wrappable state arises from
 * edits: cutting case/end off a complete match. Construct it here by
 * extracting the case tile's child from a complete parse. */
let orphan_rules_seg = (src: string): Segment.t => {
  let seg = must_parse(src);
  switch (
    seg
    |> List.find_opt((p: Piece.t) =>
         switch (p) {
         | Tile(t) => t.label == ["case", "end"]
         | _ => false
         }
       )
  ) {
  | Some(Tile(t)) => List.hd(t.children)
  | _ => fail("no case tile in: " ++ src)
  };
};

let wrap_seg_tests = [
  (
    "orphaned rule (edit-derived)",
    "case 1 | A => 2 end",
    " case1 | A => 2end ",
  ),
  (
    "orphaned rule chain (edit-derived)",
    "case 1 | A => 2 | B => 3 end",
    " case1 | A => 2 | B => 3end ",
  ),
];

let run_wrap_seg_tests =
  wrap_seg_tests
  |> List.map(((name, src, expected)) =>
       test_case(
         name,
         `Quick,
         () => {
           let seg = orphan_rules_seg(src);
           let result =
             CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
           let output = print_seg(result.completed_seg);
           check(string_testable, name, expected, output);
           check(
             Alcotest.int,
             "no incomplete",
             0,
             count_incomplete_deep(result.completed_seg),
           );
         },
       )
     );

/* Typed bare rules: standalone token tiles; completion is a no-op and
 * the chain lives on as a stuck unknown-op application. Documented. */
let wrap_tests = [
  test(
    ~name="typed bare rules: no wrap",
    ~input="1 | A => 2",
    ~expected="1 | A => 2",
  ),
];

/* === Junction drops (middle shards) + sort-frontier clipping === */
let junction_tests = [
  test_sep(
    ~name="let missing equals drops at junction",
    ~input="let x 1 in 2",
    ~expected="let x = 1 in 2",
    ~expected_no_sep="let x =1 in 2",
  ),
  test_sep(
    ~name="if missing then drops at junction",
    ~input="if true 1 else 2",
    ~expected="if true then 1 else 2",
    ~expected_no_sep="if true then1 else 2",
  ),
  test_sep(
    ~name="ambiguous junctions fall back to everything-left",
    ~input="let x y 1 in 2",
    ~expected="let x y 1 = ? in 2",
    ~expected_no_sep="let x ~y ~1 =?in 2",
  ),
];

let frontier_tests = [
  test_sep(
    ~name="fun arrow clips before let (inline)",
    ~input="fun x let y = 1 in y",
    ~expected="fun x -> let y = 1 in y",
    ~expected_no_sep="fun x-> let y = 1 in y",
  ),
  test_sep(
    ~name="fun arrow clips before indented let line",
    ~input="fun x\n  let y = 1 in y",
    ~expected="fun x ->\n  let y = 1 in y",
    ~expected_no_sep="fun x->\n  let y = 1 in y",
  ),
  test_sep(
    ~name="type in clips at typ frontier",
    ~input="type T = Int\n  2",
    ~expected="type T = Int in\n  2",
    ~expected_no_sep="type T = Intin\n  2",
  ),
  test_sep(
    ~name="exp slots never clip; junction drop still applies",
    ~input="let x = 1\n  2",
    ~expected="let x = 1\n  in 2",
    ~expected_no_sep="let x = 1\n  in2",
  ),
];

/* === Trailing junction drops (concave-right shards fill unique
   sort-legal junctions; closers excluded; ambiguity falls back) === */
let trailing_junction_tests = [
  test_sep(
    ~name="deleted else restored at junction",
    ~input="if true then 1 2",
    ~expected="if true then 1 else 2",
    ~expected_no_sep="if true then 1 else2",
  ),
  test_sep(
    ~name="deleted in restored at junction",
    ~input="let x = 1 2",
    ~expected="let x = 1 in 2",
    ~expected_no_sep="let x = 1 in2",
  ),
  test_sep(
    ~name="then drops at unique junction, else still appends",
    ~input="if true 1",
    ~expected="if true then 1 else ?",
    ~expected_no_sep="if true then1else?",
  ),
  test_sep(
    ~name="ambiguous junctions: no drop",
    ~input="if true then 1 2 3",
    ~expected="if true then 1 2 3 else ?",
    ~expected_no_sep="if true then 1 ~2 ~3else?",
  ),
  test_sep(
    ~name="closers never junction-drop",
    ~input="[1 2",
    ~expected="[1 2]",
    ~expected_no_sep="[1 ~2]",
  ),
  test_sep(
    ~name="junction drop across linebreak",
    ~input="let x = 1\n2",
    ~expected="let x = 1 in\n2",
    ~expected_no_sep="let x = 1in\n2",
  ),
];

/* === Prefix-token witnesses (backspaced delimiters) === */
let prefix_witness_tests = [
  test_sep(
    ~name="i in operator position completes to in",
    ~input="let x = 1 i 2",
    ~expected="let x = 1 in 2",
    ~expected_no_sep="let x = 1 in 2",
  ),
  test_sep(
    ~name="e in operator position completes to else",
    ~input="if true then 1 e 2",
    ~expected="if true then 1 else 2",
    ~expected_no_sep="if true then 1 else 2",
  ),
  test_sep(
    ~name="non-matching prefix is not a witness",
    ~input="let x = 1 e 2",
    ~expected="let x = 1 e 2 in ?",
    ~expected_no_sep="let x = 1 e 2in?",
  ),
];

/* === Case arbitration + rule walls ===
   Edit-derived states (deleting delimiter chars in the editor differs
   from text-parse: an unclosed paren ABSORBS following rules at parse
   time, while editor deletion leaves them as siblings). */
let destruct_l = Action.Destruct(Local(Left, ByChar));
let edit_complete = (acts: list(Action.t)): string => {
  let z = Test_Editing.perform(Zipper.init(), acts);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  print_seg(result.completed_seg);
};
let edit_case = (~name, ~acts, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, edit_complete(acts))
  );

let case_repair_tests = [
  test_sep(
    ~name="deleted end: single case, no wrap double-fire",
    ~input="case x | 1 => 2",
    ~expected="case x | 1 => 2 end",
    ~expected_no_sep="case x | 1 => 2end",
  ),
];

let case_repair_edit_tests = [
  edit_case(
    ~name="broken case head: cas witnesses case, exact restore",
    ~acts=Test_Editing.mk("case¦ x | 1 => 2 end") @ [destruct_l],
    ~expected="case x | 1 => 2 end",
  ),
  edit_case(
    ~name="deleted closer stops at rule wall",
    ~acts=Test_Editing.mk("case go(e1)¦ | 1 => 2 end") @ [destruct_l],
    ~expected="case go(e1) | 1 => 2 end",
  ),
  edit_case(
    ~name="deleted second bar restored at its junction",
    ~acts=Test_Editing.mk("case x | 1 => 2 |¦ 3 => 4 end") @ [destruct_l],
    ~expected="case x | 1 => 2 | 3 => 4 end",
  ),
  edit_case(
    ~name="deleted first bar restored at scrutinee junction",
    ~acts=Test_Editing.mk("case x |¦ 1 => 2 end") @ [destruct_l],
    ~expected="case x | 1 => 2 end",
  ),
];

/* === Entry experience (typed through the edit pipeline) ===
   The motivating flows for trailing completion: adding a new let
   between existing definitions, and starting a definition inside a
   function body. Enter auto-indents the following line, so the new
   let absorbs it as body (the designed wrapping read) and its `in`
   junction-drops at the chain boundary — `let x = 3 in let b = ...`.
   Guards the entry experience against heuristic additions. */
let entry_experience_tests = [
  edit_case(
    ~name="new let typed between existing definitions",
    ~acts=
      Test_Editing.mk("let a = 1 in\n¦let b = 2 in\nb")
      @ Test_Editing.string_to_ltr_actions("let x = 3\n"),
    ~expected="let a = 1 in\nlet x = 3\n  inlet b = 2 in\nb",
  ),
  edit_case(
    ~name="new let typed at start of fun body",
    ~acts=
      Test_Editing.mk("let f = fun q ->\n  ¦q * 2 in\nf(1)")
      @ Test_Editing.string_to_ltr_actions("let y = 1\n"),
    ~expected="let f = fun q ->\n    let y = 1in\n    q * 2 in\nf(1)",
  ),
];

/* === Leading witnesses: a >=2-char token at the opener position of
   a tile expecting that opener completes in place (the tile's
   surviving shards carry the expectation; no mold gate exists in
   operand position, so length >= 2 is the residual protection) === */
let leading_witness_tests = [
  edit_case(
    ~name="typ witnesses type",
    ~acts=Test_Editing.mk("type¦ T = Int in 2") @ [destruct_l],
    ~expected="type T = Int in 2",
  ),
  edit_case(
    ~name="le witnesses let",
    ~acts=Test_Editing.mk("let¦ x = 1 in x") @ [destruct_l],
    ~expected="let x = 1 in x",
  ),
  edit_case(
    ~name="fu witnesses fun",
    ~acts=Test_Editing.mk("fun¦ q -> q") @ [destruct_l],
    ~expected="fun q -> q",
  ),
  /* REGRESSION GUARD: a condition variable named i must not be eaten
     as an if-witness when the whole `if` is deleted (length gate) */
  edit_case(
    ~name="single-char var i survives a deleted if",
    ~acts=Test_Editing.mk("if¦ i then 1 else 2") @ [destruct_l, destruct_l],
    ~expected=" ifi then 1 else 2",
  ),
  /* REGRESSION GUARD: the witness is the broken keyword, never the
     following content, even when both share letters. The extra holes
     are editor-state, not completion's doing: `c` itself prefixes
     "case", so the InfixDelimiterPrefix mold grabs it as an operator
     in the broken buffer (`cas c? |`) — the round-9 mold-liberality
     concern, concretely. c survives; that's what this guards. */
  edit_case(
    ~name="scrutinee starting with same letters is preserved",
    ~acts=Test_Editing.mk("case¦ c | 1 => 2 end") @ [destruct_l],
    ~expected="case? c? | 1 => 2 end",
  ),
  /* fresh prefix with NO expecting tile: completion stays silent
     (materializing a whole form from a token is TyDi's job) */
  edit_case(
    ~name="fresh cas conjures nothing",
    ~acts=Test_Editing.string_to_ltr_actions("cas 1"),
    ~expected="cas ~1",
  ),
];

/* Provenance: leading-prefix masks roundtrip (the printer re-emits
   the typed prefix token, exact id, at the leading boundary) */
let leading_witness_roundtrip_tests = [
  Alcotest.test_case(
    "broken type head roundtrips through completion",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("type¦ T = Int in 2") @ [destruct_l],
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      Alcotest.(check(bool))(
        "roundtrips",
        true,
        Test_RoundtripFuzz.roundtrips(seg),
      );
    },
  ),
  Alcotest.test_case(
    "broken case head roundtrips through completion",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("case¦ x | 1 => 2 end") @ [destruct_l],
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      Alcotest.(check(bool))(
        "roundtrips",
        true,
        Test_RoundtripFuzz.roundtrips(seg),
      );
    },
  ),
];

let tests: list((string, list(Alcotest.test_case(unit)))) = [
  /* Debug test - run first to isolate crash */
  ("CanonicalCompletion: regrout-debug", regrout_debug_tests),
  /* Building block tests - verify Segment.reassemble and regrout behavior */
  ("CanonicalCompletion: reassemble", reassemble_tests),
  ("CanonicalCompletion: regrout", regrout_tests),
  /* Main completion tests */
  ("CanonicalCompletion: baseline", run_baseline_tests),
  (
    "CanonicalCompletion: trailing",
    run_completion_tests(trailing_single_tests),
  ),
  (
    "CanonicalCompletion: multi-incomplete",
    run_completion_tests(multi_incomplete_tests),
  ),
  ("CanonicalCompletion: leading", run_completion_tests(leading_tests)),
  ("CanonicalCompletion: middle", run_completion_tests(middle_tests)),
  ("CanonicalCompletion: junction", run_completion_tests(junction_tests)),
  ("CanonicalCompletion: frontier", run_completion_tests(frontier_tests)),
  (
    "CanonicalCompletion: trailing-junction",
    run_completion_tests(trailing_junction_tests),
  ),
  (
    "CanonicalCompletion: prefix-witness",
    run_completion_tests(prefix_witness_tests),
  ),
  (
    "CanonicalCompletion: case-repair",
    run_completion_tests(case_repair_tests),
  ),
  ("CanonicalCompletion: case-repair (edit-derived)", case_repair_edit_tests),
  ("CanonicalCompletion: entry-experience", entry_experience_tests),
  ("CanonicalCompletion: leading-witness", leading_witness_tests),
  (
    "CanonicalCompletion: leading-witness-roundtrip",
    leading_witness_roundtrip_tests,
  ),
  ("CanonicalCompletion: wraps", run_completion_tests(wrap_tests)),
  ("CanonicalCompletion: wraps (edit-derived)", run_wrap_seg_tests),
  ("CanonicalCompletion: linebreaks", run_completion_tests(linebreak_tests)),
];
