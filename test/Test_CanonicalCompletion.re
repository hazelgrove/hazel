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
    ~expected={|let x = 1in

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
    ~expected={|let x = 1in

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
    ~expected={|let a = 1in

let b = 2in

let c = 3in

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

~let b = 2in

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
   function body. Guards the entry experience against heuristic
   additions.

   RE-JUDGED 2026-07-27 (indentation consumes the completion's
   partitioner): a flush-written following definition is a SIBLING —
   the new let's `in` junction-drops at the chain boundary and the
   rest of the program does NOT shift right. The old expectation
   (`let b` re-indented to 2) was the wrapping read of the retired
   absorb-everything walk; inserting a definition mid-program should
   not re-indent everything below it. */
let entry_experience_tests = [
  edit_case(
    ~name="new let typed between existing definitions",
    ~acts=
      Test_Editing.mk("let a = 1 in\n¦let b = 2 in\nb")
      @ Test_Editing.string_to_ltr_actions("let x = 3\n"),
    ~expected="let a = 1 in\nlet x = 3in\nlet b = 2 in\nb",
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
/* Sort- and position-aware symbolic witnesses: `-`/`=` have no
   legitimate non-label mold at the slot's sort AND position, so they
   can only be broken `->`/`=>` prefixes there. (`-` does out at Pat
   since #2419, but only as a prefix, so it stays illegitimate after
   a complete pattern.) */
let symbolic_witness_tests = [
  edit_case(
    ~name="deleted > of ->: dash witnesses arrow in Pat slot",
    ~acts=Test_Editing.mk("let f = fun x ->¦ x * 2 in f(3)") @ [destruct_l],
    ~expected="let f = fun x -> x * 2 in f(3)",
  ),
  edit_case(
    ~name="deleted > of =>: equals witnesses rule arrow",
    ~acts=Test_Editing.mk("case x | 1 =>¦ 2 end") @ [destruct_l],
    ~expected="case x | 1 => 2 end",
  ),
  edit_case(
    ~name="genuine minus beyond the frontier is not eaten",
    /* both dashes now sit in the witness region (the Pat frontier no
       longer clips at the first one), so the leftmost rule is what
       keeps the body's real minus */
    ~acts=Test_Editing.mk("let f = fun x ->¦ x - 2 in f(1)") @ [destruct_l],
    ~expected="let f = fun x -> x - 2 in f(1)",
  ),
  /* OUT OF SCOPE: the Typ arrow `->` is a single-token operator
     form, not a shard of a multi-token tile — no remnant exists to
     EXPECT it, and witnesses are expectation-gated by design.
     Broken operators are TyDi/backpack territory. */
];

/* Opener line-walls: deleted form heads stay on their line.
   Expecteds are raw prints (spliced openers glue: ` letb`). */
let opener_wall_tests = [
  edit_case(
    ~name="fully-deleted second let stays on its line",
    ~acts=
      Test_Editing.mk("let a = 1 in\nlet¦ b = a + 2 in\na + b")
      @ [destruct_l, destruct_l, destruct_l],
    ~expected="let a = 1 in\n letb = a + 2 in\na + b",
  ),
  edit_case(
    ~name="fully-deleted if below a let stays on its line",
    ~acts=
      Test_Editing.mk("let a = 1 in\nif¦ a < 2 then a else a + 1")
      @ [destruct_l, destruct_l],
    ~expected="let a = 1 in\n ifa < 2 then a else a + 1",
  ),
  edit_case(
    ~name="fully-deleted case below a let stays on its line",
    ~acts=
      Test_Editing.mk("let t = 1 in\ncase¦ t\n| 1 => 2\n| _ => 3\nend")
      @ [destruct_l, destruct_l, destruct_l, destruct_l],
    ~expected="let t = 1 in\n caset\n| 1 => 2\n| _ => 3\nend",
  ),
  edit_case(
    ~name="multiline bracket absorption is not walled",
    /* line 2 starts with an operand, not a prefix form: the opener
       keeps its maximal-left span across the linebreak */
    ~acts=Test_Editing.mk("[¦1 +\n2]") @ [destruct_l],
    ~expected="[1+\n  2]",
  ),
  edit_case(
    ~name="inline paren around a let keeps its maximal reading",
    /* no linebreak between the let and the broken paren: no wall */
    ~acts=Test_Editing.mk("(¦let a = 1 in a)") @ [destruct_l],
    ~expected="(let a = 1 in a)",
  ),
];

let leading_witness_tests = [
  edit_case(
    ~name="typ witnesses type",
    ~acts=Test_Editing.mk("type¦ T = Int in 2") @ [destruct_l],
    ~expected="type T = Int in 2",
  ),
  /* REGRESSION GUARD (andrew report): the witness must fire with
     complete definitions ABOVE the broken one — the opener span is
     maximal-left, so a first-piece-only check missed the witness and
     spliced the opener at program start, absorbing everything */
  edit_case(
    ~name="second let's witness fires past the first definition",
    ~acts=
      Test_Editing.mk("let a = 1 in\nlet¦ b = a + 2 in\na + b")
      @ [destruct_l],
    ~expected="let a = 1 in\nlet b = a + 2 in\na + b",
  ),
  edit_case(
    ~name="type below a let witnesses in place",
    ~acts=
      Test_Editing.mk("let a = 1 in\ntype¦ T = Int in\na") @ [destruct_l],
    ~expected="let a = 1 in\ntype T = Int in\na",
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
     following content, even when both share letters. Exact since the
     prefix-mold table was restricted to non-leading delimiters (`c`
     no longer molds as an operator in the broken buffer). */
  edit_case(
    ~name="scrutinee starting with same letters is preserved",
    ~acts=Test_Editing.mk("case¦ c | 1 => 2 end") @ [destruct_l],
    ~expected="case c | 1 => 2 end",
  ),
  /* (A) corroborated single-char witness: deleting the f of if
     leaves i AGAINST JUNCTION DEBRIS — absorbed, exact restore */
  edit_case(
    ~name="corroborated i witnesses if",
    ~acts=Test_Editing.mk("if¦ x < 3 then 1 else 2") @ [destruct_l],
    ~expected="if x < 3 then 1 else 2",
  ),
  /* fresh prefix with NO expecting tile: completion stays silent
     (materializing a whole form from a token is TyDi's job) */
  edit_case(
    ~name="fresh cas conjures nothing",
    ~acts=Test_Editing.string_to_ltr_actions("cas 1"),
    ~expected="cas ~1",
  ),
];

/* Provenance: leading-prefix masks REPRINT the typed prefix token +
   its junction debris — but with a KNOWN one-space layout gap: the
   buffer has [tok][grout][sp] while the reprint produces
   [tok][sp][grout][sp] (the shard-boundary secondary run lands
   before the re-emitted debris). Strict roundtrip is therefore OPEN
   for leading-witness states (plans/completion-heuristics.md); these
   pins surface any movement in either direction. */
let leading_witness_roundtrip_tests = [
  Alcotest.test_case(
    "broken type head: reprint text (KNOWN one-space gap)",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("type¦ T = Int in 2") @ [destruct_l],
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      let result =
        CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
      let masks = CanonicalCompletion.masks_of_records(result.shard_records);
      let term = MakeTerm.go_impl(~masks, result.completed_seg).term;
      let seg2 = Test_ExpToSegment.exp_to_segment_roundtrip(term);
      check(string_testable, "buffer", "typ~ T = Int in 2", print_seg(seg));
      check(
        string_testable,
        "reprint (one extra space: OPEN)",
        "typ ~ T = Int in 2",
        print_seg(seg2),
      );
    },
  ),
];

/* === Continuation lines + closer witnesses (multiline case/if) === */
let continuation_tests = [
  edit_case(
    ~name="deleted d of multiline end: en witnesses past the rules",
    ~acts=
      Test_Editing.mk("let t = 1 in\ncase t\n| 1 => 2\n| _ => 3\nend¦")
      @ [destruct_l],
    ~expected="let t = 1 in\ncase t\n| 1 => 2\n| _ => 3\nend",
  ),
  /* no witness left: end appends after the rules (the emptied last
     line's linebreak partitions off; splice-glue cosmetics) */
  edit_case(
    ~name="fully deleted multiline end appends after the rules",
    ~acts=
      Test_Editing.mk("let t = 1 in\ncase t\n| 1 => 2\n| _ => 3\nend¦")
      @ [destruct_l, destruct_l, destruct_l],
    ~expected="let t = 1 in\ncase t\n| 1 => 2\n| _ => 3end\n",
  ),
  edit_case(
    ~name="multiline els witnesses else",
    ~acts=
      Test_Editing.mk("let a = 1 in\nif a < 2 then a\nelse¦ a + 1")
      @ [destruct_l],
    ~expected="let a = 1 in\nif a < 2 then a\nelse a + 1",
  ),
  edit_case(
    ~name="inline en witnesses end",
    ~acts=Test_Editing.mk("case t | 1 => 2 end¦") @ [destruct_l],
    ~expected="case t | 1 => 2 end",
  ),
];

/* Joint satisfiability: end+in double deletion restores exactly
   (endin adjacency is print glue). */
let dbl_del_inline =
  Test_Editing.mk("let f = case x | 1 => 2 | 3 => 4 end in¦ f")
  @ [destruct_l, destruct_l]
  @ Test_Editing.mv_l(1)
  @ [destruct_l, destruct_l, destruct_l];
let probe_raw = (acts: list(Action.t)): string => {
  let z = Test_Editing.perform(Zipper.init(), acts);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let inc = Segment.incomplete_tiles_deep(seg);
  let tiles =
    List.filter_map(
      (pc: Piece.t) =>
        switch (pc) {
        | Tile(t) =>
          Some(
            Printf.sprintf(
              "%s[%s]",
              String.concat("", Tile.effective_label(t)),
              String.concat(",", List.map(string_of_int, t.shards)),
            ),
          )
        | Grout(g) => Some(g.shape == Convex ? "?" : "~")
        | _ => None
        },
      seg,
    );
  Printf.sprintf(
    "%s | top: %s | inc: %d",
    print_seg(seg),
    String.concat(" ", tiles),
    List.length(inc),
  );
};
let probe_case = (~name, ~acts, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, probe_raw(acts))
  );
/* Reassociation guards: delete + retype the l of a NON-FIRST let
   must re-pair with its orphaned =/in (rescan matches singletons
   only; the fallback must try the sibling scope). */
let probe_tests = [
  probe_case(
    ~name="second-let delete+retype l reassociates",
    ~acts=
      Test_Editing.mk("let f = 1 in\nl¦et g = 2 in\nf + g")
      @ [destruct_l, Action.Insert("l")],
    ~expected=
      "let f = 1 in\nlet g = 2 in\nf + g | top: let=in[0,1,2] let=in[0,1,2] f[0] +[0] g[0] | inc: 0",
  ),
  probe_case(
    ~name="first-let delete+retype l reassociates (control)",
    ~acts=
      Test_Editing.mk("l¦et f = 1 in\nf + 1")
      @ [destruct_l, Action.Insert("l")],
    ~expected=
      "let f = 1 in\nf + 1 | top: let=in[0,1,2] f[0] +[0] 1[0] | inc: 0",
  ),
];
/* Hole-minimizing append, semi-only (2026-09 round): a closer stops
   before a span-final trailing SEQUENCE SEPARATOR when content
   follows — the semi legitimately binds across the boundary. */
let probe2_tests = [
  edit_case(
    ~name="deleted test-end stops before the semicolon",
    ~acts=
      Test_Editing.mk("test 1 == 1 end¦;\ntest 2 == 2 end;\n3")
      @ [destruct_l, destruct_l, destruct_l],
    ~expected="test 1 == 1end ;\ntest 2 == 2 end;\n3",
  ),
];
/* Print the insertion list: delimiters per record (+-joined) in list
   order, |-separated across records — coalescing and ordering are
   what these pin */
let probe_ins = (acts: list(Action.t)): string => {
  let z = Test_Editing.perform(Zipper.init(), acts);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let r = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  r.insertions
  |> List.map((i: CanonicalCompletion.insertion) =>
       i.delimiters
       |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
       |> String.concat("+")
     )
  |> String.concat(" | ");
};
let ins_case = (~name, ~acts, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, probe_ins(acts))
  );
let ordering_tests = [
  ins_case(
    ~name="end witness + in coalesce in nesting order",
    /* backspace the in, then the d of end: the en witness completes
       first and the in must anchor AT the en (alias), coalescing
       into one record reading end-then-in (andrew: it displayed
       'in end') */
    ~acts=
      Test_Editing.mk("let arm_adt = case c | Red => 1 end in¦ 2")
      @ [destruct_l, destruct_l, destruct_l, destruct_l],
    ~expected="end | in",
  ),
  ins_case(
    ~name="line-final: end witness + in coalesce (andrew repro)",
    ~acts=
      Test_Editing.mk("let arm_adt = case c | Red => 1 end in¦")
      @ [destruct_l, destruct_l, destruct_l, destruct_l],
    ~expected="end+in",
  ),
];
/* TyDi delimiter-suffix gates (the e/el/els matrix): ci is None on
   these states (completion consumed the prefix token) — suggestions
   must survive via the ci-free witness route. Probes replicate
   Editor.calculate's exact call. */
let probe_tydi = (acts: list(Action.t)): string => {
  let z = Test_Editing.perform(Zipper.init(), acts);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let ci = Indicated.ci_for_completion(z, statics.info_map);
  let tok =
    switch (TyDi.token_to_left(z)) {
    | None => "tok:NONE"
    | Some(t) => "tok:" ++ t
    };
  let ci_s =
    switch (ci) {
    | None => "ci:NONE"
    | Some(i) => "ci:" ++ (Language.Info.cls_of(i) |> Language.Cls.show)
    };
  let buf =
    switch (TyDi.set_buffer(~ci, z)) {
    | None => "buf:NONE"
    | Some(z') =>
      switch (TyDi.get_unparsed_buffer(z')) {
      | None => "buf:???"
      | Some(t) => "buf:" ++ t
      }
    };
  String.concat(" | ", [tok, ci_s, buf]);
};
let tydi_case = (~name, ~acts, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, probe_tydi(acts))
  );
let tydi_probe_tests = [
  tydi_case(
    ~name="els suggests remainder e",
    ~acts=Test_Editing.mk("if 1 < 2 then 3 else¦ 4") @ [destruct_l],
    ~expected="tok:els | ci:NONE | buf:e",
  ),
  tydi_case(
    ~name="el suggests remainder se",
    ~acts=
      Test_Editing.mk("if 1 < 2 then 3 else¦ 4") @ [destruct_l, destruct_l],
    ~expected="tok:el | ci:NONE | buf:se",
  ),
  tydi_case(
    ~name="e suggests remainder lse (expectation bypasses length gate)",
    ~acts=
      Test_Editing.mk("if 1 < 2 then 3 else¦ 4")
      @ [destruct_l, destruct_l, destruct_l],
    ~expected="tok:e | ci:NONE | buf:lse",
  ),
  tydi_case(
    ~name="dash suggests arrow remainder",
    ~acts=Test_Editing.mk("fun x ->¦ x * 2") @ [destruct_l],
    ~expected="tok:- | ci:NONE | buf:>",
  ),
  tydi_case(
    ~name="non-head obligation suggests (en inside open paren)",
    /* the old head-only path could never suggest this: the paren's )
       is the nearest obligation, the case's end is deeper — the
       witness route matches by anchor, not stack position */
    ~acts=Test_Editing.mk("(case x | 1 => 2 en¦"),
    ~expected="tok:en | ci:NONE | buf:d",
  ),
  tydi_case(
    ~name="1-char ctx prefix stays gated (noise guard)",
    /* no expectation in play: the length gate still blocks 1-char
       context-variable suggestions */
    ~acts=Test_Editing.mk("let ee = 7 in e¦"),
    ~expected="tok:e | ci:Variable reference | buf:NONE",
  ),
  tydi_case(
    ~name="equals suggests rule-arrow remainder",
    ~acts=Test_Editing.mk("case x | 1 =>¦ 2 end") @ [destruct_l],
    ~expected="tok:= | ci:NONE | buf:>",
  ),
];
/* Mid-entry case rules: the end sits AFTER the growing rule from
   the first bar onward and never retreats. Raw-print pins. */
/* Triple-click a rule: selection stops at the body's last content
   piece — no trailing linebreak or indentation. */
let sel_case = (~name, ~acts, ~expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = Test_Editing.perform(Zipper.init(), acts);
      check(string_testable, name, expected, print_seg(z.selection.content));
    },
  );
let rule_selection_tests = [
  sel_case(
    ~name="triple-click mid rule selects rule sans trailing whitespace",
    ~acts=
      Test_Editing.mk("case x\n| 1 =>¦ 2\n| 3 => 4\nend")
      @ [Select(Smart(2)), Select(Smart(3))],
    ~expected="| 1 => 2",
  ),
  sel_case(
    ~name="triple-click last rule stops at body end",
    ~acts=
      Test_Editing.mk("case x\n| 1 => 2\n| 3 =>¦ 4\nend")
      @ [Select(Smart(2)), Select(Smart(3))],
    ~expected="| 3 => 4",
  ),
];

/* Tab dispatch: the caret pinned to a quiver chip discharges that
   obligation via obligation_at_caret -> ApplyCompletion(One) — no
   inline buffer needed (buffers only appear on edits; Tab must work
   after pure movement too). */
let move_l = Action.Move(Local(Left, ByChar));
let move_r = Action.Move(Local(Right, ByChar));
/* mirrors the editor's TAB policy exactly: paste the chip's next
   chunk through the normal pipeline. Output is the CARET-MARKED
   printer (¦), so these pin text, spacing, AND caret together. */
let tab_once = (z: Zipper.t): option(Zipper.t) =>
  switch (CompletionQuery.chip_at_caret(z)) {
  | Some(ins) =>
    switch (CompletionQuery.tab_text(z, ins)) {
    | Some(text) => Some(Test_Editing.perform(z, [Paste(text)]))
    | None => None
    }
  | None => None
  };

let tab_dispatch = (~tabs=1, acts: list(Action.t)): string => {
  let z = Test_Editing.perform(Zipper.init(), acts);
  let rec go = (z, k) =>
    k <= 0
      ? Some(z)
      : (
        switch (tab_once(z)) {
        | Some(z) => go(z, k - 1)
        | None => None
        }
      );
  switch (go(z, tabs)) {
  | None => "NONE"
  | Some(z) => Test_Editing.printer(z)
  };
};

let tab_case = (~name, ~acts, ~tabs=1, ~expected, ()) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, tab_dispatch(~tabs, acts))
  );

let tab_dispatch_tests = [
  tab_case(
    /* the left neighbor is a case TILE whose effective last token is
       `end`: the junction predicate must see it (tab pasted "in " and
       produced endin) */
    ~name="chip after a multi-token tile spaces the junction",
    ~acts=Test_Editing.mk("let x = case y | _ => 1 end¦"),
    ~expected="let x = case y | _ => 1 end in ¦?",
    (),
  ),
  tab_case(
    ~name="tab after 4: space, in, caret past",
    ~acts=Test_Editing.mk("let a = 4¦"),
    ~expected="let a = 4 in ¦?",
    (),
  ),
  tab_case(
    ~name="tab after 4 and a space: no double space, caret past",
    ~acts=Test_Editing.mk("let a = 4 ¦"),
    ~expected="let a = 4 in ¦?",
    (),
  ),
  tab_case(
    ~name="multi-delimiter chip: one delimiter per tab",
    ~acts=Test_Editing.mk("let _: (Int, Bool) ¦"),
    ~expected="let _: (Int, Bool) =¦?",
    (),
  ),
  tab_case(
    ~name="multi-delimiter chip: second tab takes the next",
    ~acts=Test_Editing.mk("let _: (Int, Bool) ¦"),
    ~tabs=2,
    ~expected="let _: (Int, Bool) =? in ¦?",
    (),
  ),
  tab_case(
    ~name="witness: tab completes the arrow like typing",
    ~acts=
      Test_Editing.mk("case x | 1 =¦") @ [move_l, move_l, move_r, move_r],
    ~expected="case x | 1 =>¦?",
    (),
  ),
  tab_case(
    ~name="tab away from any chip dispatches nothing",
    ~acts=Test_Editing.mk("1 + 2¦"),
    ~expected="NONE",
    (),
  ),
  tab_case(
    ~name="coalesced end+paren: innermost only, symbolic spacing",
    ~acts=Test_Editing.mk("(case x | 1 => 2¦"),
    ~expected="(case x | 1 => 2 end ¦",
    (),
  ),
];

/* The two accept gestures must agree: tabbing a completion to its
   fixpoint and materializing it wholesale converge to the same
   program modulo whitespace. Guards the type-it-for-me / make-it-so
   split from semantic drift. */
let strip_ws = (s: string): string =>
  String.to_seq(s) |> Seq.filter(c => c != ' ' && c != '\n') |> String.of_seq;

let tabs_vs_materialize = (~name, ~acts, ()) =>
  test_case(
    name,
    `Quick,
    () => {
      let z0 = Test_Editing.perform(Zipper.init(), acts);
      let via_mat =
        CanonicalCompletion.materialize_all(
          ~sort=Sort.Exp,
          Zipper.unselect_and_zip(~erase_buffer=true, z0),
        )
        |> print_seg;
      let rec tab_out = (z, fuel) =>
        fuel <= 0
          ? z
          : (
            switch (tab_once(z)) {
            | Some(z) => tab_out(z, fuel - 1)
            | None => z
            }
          );
      let via_tabs =
        tab_out(z0, 8)
        |> Zipper.unselect_and_zip(~erase_buffer=true)
        |> print_seg;
      check(string_testable, name, strip_ws(via_mat), strip_ws(via_tabs));
    },
  );

let tab_materialize_equiv_tests = [
  tabs_vs_materialize(
    ~name="equiv: let missing in",
    ~acts=Test_Editing.mk("let a = 4¦"),
    (),
  ),
  tabs_vs_materialize(
    ~name="equiv: annotated let missing = and in",
    ~acts=Test_Editing.mk("let _: (Int, Bool) ¦"),
    (),
  ),
  tabs_vs_materialize(
    ~name="equiv: paren case missing end and closer",
    ~acts=Test_Editing.mk("(case x | 1 => 2¦"),
    (),
  ),
  tabs_vs_materialize(
    ~name="equiv: rule arrow witness",
    ~acts=Test_Editing.mk("case x | 1 =¦"),
    (),
  ),
  tabs_vs_materialize(
    ~name="equiv: nested second let",
    ~acts=Test_Editing.mk("let a = 2 in\nlet _: (Int, Bo¦"),
    (),
  ),
];

/* Materialization: ALL = the joint result; ONE discharges a single
   tile and moves nothing else. */
let materialize_tests = [
  test_case(
    "materialize all commits the joint completion",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(Zipper.init(), Test_Editing.mk("let x = 1¦"));
      let z = Test_Editing.perform(z, [ApplyCompletion(All)]);
      check(
        string_testable,
        "all",
        "let x = 1 in?",
        print_seg(Zipper.unselect_and_zip(~erase_buffer=true, z)),
      );
    },
  ),
  test_case(
    "materialize one discharges only that tile",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("(case x | 1 => 2¦"),
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      let case_id =
        Segment.incomplete_tiles_deep(seg)
        |> List.find((t: Tile.t) => List.mem("case", t.label))
        |> ((t: Tile.t) => t.id);
      let z = Test_Editing.perform(z, [ApplyCompletion(One(case_id))]);
      check(
        string_testable,
        "one",
        "(case x | 1 => 2 end",
        print_seg(Zipper.unselect_and_zip(~erase_buffer=true, z)),
      );
    },
  ),
];

let bogus_materialize_test =
  test_case(
    "materialize one refuses a non-obligation id",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(Zipper.init(), Test_Editing.mk("let x = 1¦"));
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      check(
        Alcotest.bool,
        "bogus id",
        true,
        CanonicalCompletion.materialize_one(~sort=Sort.Exp, seg, Id.mk())
        == None,
      );
    },
  );

let entry_stability_tests = [
  edit_case(
    ~name="bar alone: end stays after the rule",
    ~acts=Test_Editing.mk("let new_fun =\nfun foo ->\ncase foo\n|¦\n2"),
    ~expected=
      "let new_fun =\n  fun foo ->\n    case foo\n    |\n    2=>?endin?",
  ),
  edit_case(
    ~name="bar pattern arrow: end stays after the rule",
    ~acts=Test_Editing.mk("let new_fun =\nfun foo ->\ncase foo\n| 1 =>¦\n2"),
    ~expected=
      "let new_fun =\n  fun foo ->\n    case foo\n    | 1 =>\n      2endin?",
  ),
  edit_case(
    ~name="complete rule: end after the rule body",
    ~acts=
      Test_Editing.mk("let new_fun =\nfun foo ->\ncase foo\n| 1 => 2¦\n3"),
    ~expected=
      "let new_fun =\n  fun foo ->\n    case foo\n    | 1 => 2end\n    in3",
  ),
];

/* Placement guards from the hygiene investigation: junction
   restoration for deleted middle delimiters (incl. multiline) and
   append landing at the content line's end, never after trailing
   linebreaks. */
let placement_guard_tests = [
  edit_case(
    ~name="deleted = restores at the definition junction",
    ~acts=Test_Editing.mk("let x =¦ f 1 in x") @ [destruct_l],
    ~expected="let x =f ~1 in x",
  ),
  edit_case(
    ~name="deleted = multiline restores at line end",
    ~acts=Test_Editing.mk("let x =¦\nf 1 in x") @ [destruct_l],
    ~expected="let x =\nf ~1 in x",
  ),
  edit_case(
    ~name="deleted rule arrow with next-line body restores exactly",
    ~acts=
      Test_Editing.mk("case v\n| Cons(x, xs) =>¦\ngo(xs)\nend")
      @ [destruct_l, destruct_l],
    ~expected="case v\n| Cons(x, xs) => go(xs)\nend",
  ),
  edit_case(
    ~name="deleted in lands before the trailing linebreak",
    ~acts=
      Test_Editing.mk("type Result = Ok(Exp) in¦\n1")
      @ [destruct_l, destruct_l],
    ~expected="type Result = Ok(Exp)in \n1",
  ),
  edit_case(
    ~name="entry in lands at the content line, not the blank line",
    ~acts=Test_Editing.mk("let x = 1¦\n\n2"),
    ~expected="let x = 1in\n\n2",
  ),
];

/* Inline deleted form heads: the sort clamp lands a restored head
   at the nearest span fitting its interior slot (clippable sorts),
   while Exp-slot wrappers keep their maximal span. */
let head_restoration_tests = [
  edit_case(
    ~name="inline second let head restores at its pattern",
    ~acts=
      Test_Editing.mk("let a = 1 in let¦ b = 2 in b")
      @ [destruct_l, destruct_l, destruct_l],
    ~expected="let a = 1 in  letb = 2 in b",
  ),
  edit_case(
    ~name="inline second type head restores at its tpat",
    ~acts=
      Test_Editing.mk("type A = Int in type¦ B = Bool in x")
      @ [destruct_l, destruct_l, destruct_l, destruct_l],
    ~expected="type A = Int in  typeB = Bool in x",
  ),
  edit_case(
    ~name="deleted open paren keeps maximal wrap",
    ~acts=Test_Editing.mk("(¦let a = 1 in a)") @ [destruct_l],
    ~expected="(let a = 1 in a)",
  ),
];

let joint_tests = [
  edit_case(
    ~name="end+in double deletion: placements incompatible (KNOWN-BAD)",
    ~acts=dbl_del_inline,
    ~expected="let f = case x | 1 => 2 | 3 => 4end in  f",
  ),
  /* control: in alone (end intact) — its deletion-debris junction
     should restore it in place */
  edit_case(
    ~name="in alone: junction restores in place",
    ~acts=
      Test_Editing.mk("let f = case x | 1 => 2 | 3 => 4 end in¦ f")
      @ [destruct_l, destruct_l],
    ~expected="let f = case x | 1 => 2 | 3 => 4 end in f",
  ),
];

/* === Clippable-sort guard ===
 * clip_position clips only Pat/TPat/Typ slots; the justification is
 * statistical: an Exp frontier is vacuous (nearly every label has an
 * Exp mold) while Pat/TPat/Typ frontiers are real signal, and Rul
 * defers to the case-wrap machinery. This pins the form-table
 * coverage those decisions rest on — if the table drifts, re-decide
 * clippable_sort rather than repinning blindly. */
let clippable_guard_tests = {
  let labels =
    Form.forms
    |> List.map(((_, f: Form.t)) => f.label)
    |> List.sort_uniq(compare);
  let n = List.length(labels);
  let covered = (s: Sort.t): int =>
    labels
    |> List.filter(l =>
         Form.Molds.get_base(l)
         |> List.exists((m: Mold.t) => m.out == s || m.out == Sort.Any)
       )
    |> List.length;
  let table =
    [Sort.Exp, Sort.Pat, Sort.Typ, Sort.TPat, Sort.Rul]
    |> List.map(s =>
         Printf.sprintf("%s %d/%d", Sort.to_string(s), covered(s), n)
       )
    |> String.concat(" | ");
  [
    Alcotest.test_case("form-table sort coverage", `Quick, () =>
      Alcotest.(check(string))(
        "coverage",
        /* Pat 8 -> 9: negative literal patterns (#2419) gave `-` a
           Pat mold. Re-decided, not repinned blindly: 9/88 is still
           real signal, so clippable_sort stands.
           Typ 14 -> 15, TPat 2 -> 4: #2448's symbolic delimiter-prefix
           backup molds (Form.symbolic_delim_prefixes = `-`, `=`) reach
           the InfixDelimiterPrefix atomic, which molds at all four
           sorts. Re-decided: both frontiers stay far from vacuous, so
           clippable_sort stands. */
        "Exp 67/88 | Pat 9/88 | Typ 15/88 | TPat 4/88 | Rul 1/88",
        table,
      )
    ),
    Alcotest.test_case(
      "clippable = the meaningfully partial sorts", `Quick, () =>
      Alcotest.(check(list(bool)))(
        "clippable",
        [false, true, true, true, false],
        List.map(
          CanonicalCompletion.clippable_sort,
          [Sort.Exp, Sort.Pat, Sort.Typ, Sort.TPat, Sort.Rul],
        ),
      )
    ),
  ];
};

/* === Closer-severance round (2026-09-01, PR #2374 review) ===
 * The hole-min back-over must not sever non-separator material.
 * Cyrus's premature `end in` (a completed if-form counted as a
 * span-final "trailing operator" and was backed over, landing the
 * end after the rule arrow) and the `)`-before-`:` annotation flip
 * are the same overfiring. Content at column 0 below is load-bearing
 * in every input: it arms the content-follows gate. */
let severance_tests = [
  test(
    ~name="end+in stay after the rule-body if (case-def)",
    ~input="let f =\n    case 0\n    | 0 =>\n        if \n1",
    ~expected=
      "let f =\n    case 0\n    | 0 =>\n        if?then?else? endin\n1",
  ),
  test(
    ~name="end+in stay after the rule-body if (Cyrus partition_at)",
    ~input=
      "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n    if \n1",
    ~expected=
      "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n    if?then?else? endin\n1",
  ),
  test(
    ~name="end stays after the rule-body if (standalone case)",
    ~input="case 0\n| 0 =>\n    if \n1",
    ~expected="case 0\n| 0 =>\n    if?then?else? end~\n1",
  ),
  test(
    ~name="ap-pattern closer stays after the annotation colon",
    ~input=
      "let qsort(xs :\ntest qsort([5, 4, 8, 9, 3, 2, 7]) == [2, 3, 4, 5, 7, 8, 9] end",
    ~expected=
      "let qsort(xs :?)=?in\ntest qsort([5, 4, 8, 9, 3, 2, 7]) == [2, 3, 4, 5, 7, 8, 9] end",
  ),
  test(
    ~name="paren closer stays after a trailing colon",
    ~input="(x :\nf(3)",
    ~expected="(x :?)~\nf(3)",
  ),
  test(
    ~name="paren closer keeps a trailing + when in interposes",
    ~input="let x = (1 +\nf(3)",
    ~expected="let x = (1 +?)in\nf(3)",
  ),
];

/* Completion recurses while the incomplete count strictly decreases —
   no fixed pass ceiling. 30 obligations (the old fuel of 24 left 5). */
let depth_tests = [
  test(
    ~name="30 nested openers complete fully",
    ~input=String.make(30, '(') ++ "1",
    ~expected=String.make(30, '(') ++ "1" ++ String.make(30, ')'),
  ),
];
let depth_count_tests = [
  test_case(
    "30 nested let prefixes leave nothing incomplete",
    `Quick,
    () => {
      let seg =
        must_parse(String.concat("", List.init(30, _ => "let x = ")) ++ "1");
      let r = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
      check(
        Alcotest.int,
        "incomplete",
        0,
        count_incomplete_deep(r.completed_seg),
      );
    },
  ),
];

let tests: list((string, list(Alcotest.test_case(unit)))) = [
  (
    "CanonicalCompletion: depth",
    run_completion_tests(depth_tests) @ depth_count_tests,
  ),
  (
    "CanonicalCompletion: closer-severance",
    run_completion_tests(severance_tests),
  ),
  ("CanonicalCompletion: head-restoration", head_restoration_tests),
  ("CanonicalCompletion: reassociation-guards", probe_tests),
  ("CanonicalCompletion: closer-vs-separator", probe2_tests),
  ("CanonicalCompletion: tydi-gates", tydi_probe_tests),
  ("CanonicalCompletion: insertion-ordering", ordering_tests),
  ("CanonicalCompletion: rule-selection", rule_selection_tests),
  (
    "CanonicalCompletion: materialize",
    materialize_tests @ [bogus_materialize_test],
  ),
  ("CanonicalCompletion: entry-stability", entry_stability_tests),
  ("CanonicalCompletion: placement-guards", placement_guard_tests),
  ("CanonicalCompletion: tab-dispatch", tab_dispatch_tests),
  ("CanonicalCompletion: tab-materialize-equiv", tab_materialize_equiv_tests),
  ("CanonicalCompletion: joint-satisfiability", joint_tests),
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
  ("CanonicalCompletion: symbolic-witness", symbolic_witness_tests),
  ("CanonicalCompletion: opener-walls", opener_wall_tests),
  ("CanonicalCompletion: continuation", continuation_tests),
  (
    "CanonicalCompletion: leading-witness-roundtrip",
    leading_witness_roundtrip_tests,
  ),
  ("CanonicalCompletion: wraps", run_completion_tests(wrap_tests)),
  ("CanonicalCompletion: wraps (edit-derived)", run_wrap_seg_tests),
  ("CanonicalCompletion: linebreaks", run_completion_tests(linebreak_tests)),
  ("CanonicalCompletion: clippable-guard", clippable_guard_tests),
];
