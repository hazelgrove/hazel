/**
 * This file contains tests to validate the AutoProbe module's probe placement logic.
 *
 * Tests are written in concrete syntax with trailing comments indicating expected probe results:
 *
 * Example:
 * let a = 1 + 2 in # 1 + 2 #
 * let (x, y) = 1 in # 1 #
 * a # no probe #
 *
 * The test harness extracts comments and compares them to actual probe placements.
 */
open Alcotest;
open Haz3lcore;
open Language;

/* Comment parsing utilities */

/* Extract trailing comments from each line of code */
let extract_line_comments = (code: string): (string, list(option(string))) => {
  let lines = String.split_on_char('\n', code);

  let process_line = (line: string): (string, option(string)) => {
    /* Find the position of "#" comment delimiters */
    switch (String.index_opt(line, '#')) {
    | Some(first_hash) =>
      /* Look for the closing # after the first one */
      let after_first = first_hash + 1;
      switch (String.index_from_opt(line, after_first, '#')) {
      | Some(second_hash) =>
        let clean_line = String.sub(line, 0, first_hash) |> String.trim;
        let comment_part =
          String.sub(line, after_first, second_hash - after_first)
          |> String.trim;

        let expected =
          if (comment_part == "no probe") {
            None;
          } else {
            Some(comment_part);
          };
        (clean_line, expected);
      | None => (line, None)
      };
    | None => (line, None)
    };
  };

  let (clean_lines, comments) =
    lines |> List.map(process_line) |> List.split;
  let clean_code = String.concat("\n", clean_lines);
  (clean_code, comments);
};

/* Normalize whitespace by collapsing linebreaks and multiple spaces into single spaces */
let normalize_whitespace = (s: string): string => {
  let s = Str.global_replace(Str.regexp("[\n\r\t]+"), " ", s);
  let s = Str.global_replace(Str.regexp(" +"), " ", s);
  String.trim(s);
};

/* Convert term ID to string representation using segment */
let term_id_to_string =
    (id: Id.t, _terms: TermMap.t, data: TermData.t): option(string) => {
  switch (TermData.segment(id, data)) {
  | Some(segment) =>
    Some(
      Base.segment_to_string(
        ~holes="",
        ~concave_holes="",
        ~refractor_seg_to_seg=(a, b) => (a, b),
        ~refractors=Id.Map.empty,
        ~projector_to_segment=_ => [],
        segment,
      )
      |> normalize_whitespace,
    )
  | None => None
  };
};

/* Main testing function */
let test_probe_placement = (~name: string, ~code: string): test_case(_) => {
  test_case(
    name,
    `Quick,
    () => {
      /* Parse the test code to extract expected results */
      let (clean_code, expected_comments) = extract_line_comments(code);

      /* Filter out lines with no expected probe and extract expected strings */
      let expected_probes = expected_comments |> List.filter_map(Fun.id);

      /* Parse the clean code into a zipper */
      let zipper =
        switch (Parser.to_zipper(clean_code)) {
        | Some(z) => z
        | None => fail("Failed to parse code: " ++ clean_code)
        };

      /* Get the term for statics computation */
      let root_segment = Zipper.unselect_and_zip(zipper);
      let root_id =
        Segment.root_id(Segment.skel(root_segment), root_segment);

      /* Compute statics */
      let MakeTerm.{term, _} = MakeTerm.go(root_segment);
      let info_map =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);

      /* Build the syntax cache with statics */
      let syntax = CachedSyntax.mk(zipper, ~info_map, ~dyn_map=Id.Map.empty);

      /* Call AutoProbe to get probe term IDs using the sophisticated version */
      let probe_ids =
        switch (
          AutoProbe.ids_to_autoprobe(
            root_id,
            syntax.term_data,
            syntax.terms,
            syntax.measured,
            info_map,
          )
        ) {
        | Some(ids) => List.filter_map(Fun.id, ids)
        | None => fail("AutoProbe returned None")
        };

      /* Convert probe IDs to string representations */
      let actual_probes =
        List.filter_map(
          term_id_to_string(_, syntax.terms, syntax.term_data),
          probe_ids,
        );

      /* Debug for failing tests */
      if (List.length(expected_probes) != List.length(actual_probes)) {
        print_endline(
          "Expected: "
          ++ String.concat(
               ", ",
               List.map(s => "'" ++ s ++ "'", expected_probes),
             ),
        );
        print_endline(
          "Actual: "
          ++ String.concat(
               ", ",
               List.map(s => "'" ++ s ++ "'", actual_probes),
             ),
        );
      };

      /* Compare expected vs actual */
      let expected_count = List.length(expected_probes);
      let actual_count = List.length(actual_probes);

      check(int, "Number of probes", expected_count, actual_count);

      /* Compare each probe string */
      List.iter2(
        (expected, actual) => {
          check(string, "Probe content", expected, actual)
        },
        expected_probes,
        actual_probes,
      );
    },
  );
};

/* Basic test cases */
let basic_tests = [
  test_probe_placement(~name="Probe atomic literal", ~code={|1 # 1 #|}),
  test_probe_placement(
    ~name="Probe largest rightmost term",
    ~code={|2 + 1 # 2 + 1 #|},
  ),
  test_probe_placement(
    ~name="Single-line function application",
    ~code={|get_fn(true) # get_fn(true) #|},
  ),
];

/* NESTED / MULTILINE */
let nested_multiline_tests = [
  test_probe_placement(
    ~name="Multi-line parens - don't redundatly probe parens",
    ~code=
      {|
let x = (    # x #
  1 + 1      # 1 + 1 #
) in
1 + 1        # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multi-line function application - probe ap not last arg",
    ~code=
      {|
let x = f(1 + 1,  # 1 + 1 #
  2) in           # f(1 + 1, 2) #
1 + 1             # 1 + 1 #|},
  ),
];

/* HOLE AVOIDANCE EXAMPLES - prefer non-holes when available */
let hole_avoidance_tests = [
  test_probe_placement(
    ~name="Avoid hole if there's an alternative",
    ~code=
      {|
let incomplete = ? in  # incomplete #
1 + 1                  # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Probe hole if there's no alternative",
    ~code={|? # ? #|},
  ),
];

/* CONTAINER SPECIAL CASES - multi-line containers prefer elements */
let container_tests = [
  test_probe_placement(
    //TODO: consider probing parens instead of tuple
    ~name="Single-line tuple - normal behavior",
    ~code={|let pair = (a, b) in # a, b #
pair # pair #|},
  ),
  test_probe_placement(
    ~name="Single-line list - normal behavior",
    ~code={|let list = [1, 2, 3 + 1] in # [1, 2, 3 + 1] #
list # list #|},
  ),
  /* Note: Multi-line containers probe the trailing elements on each line,
     but not the cotainer itself */
  test_probe_placement(
    ~name="Multi-line tuple - probe elements but not container",
    ~code=
      {|let triple = ( # triple #
  a, # a #
  b, # b #
  c # c #
) in
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multi-line tuple - probe trailing elements on each line 1",
    ~code=
      {|let triple = ( # triple #
  a, # a #
  b, c + d # c + d #
) in
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multi-line tuple - probe trailing elements on each line 2",
    ~code=
      {|let triple = ( # triple #
  a, b + c, # b + c #
  d # d #
) in
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multi-line list - probe elements but not container",
    ~code=
      {|let items = [ # items #
  a, # a #
  b, # b #
  c # c #
] in
1 + 1 # 1 + 1 #|},
  ),
];

/* LET EXPRESSION SPECIAL CASES - testing let body handling */
let let_expression_tests = [
  test_probe_placement(
    ~name="Only one term at rightmost position",
    ~code={|let (x, y) = 1 in # 1 #
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multiple terms at rightmost - largest wins",
    ~code={|let (x, y) = 2 + 1 in # 2 + 1 #
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Let with hole body ending on same line - don't probe let or hole",
    ~code={|let x = 2 + 1 in ? # 2 + 1 #|},
  ),
  test_probe_placement(
    ~name="Let with hole body plus hole avoidance",
    ~code={|let x = ? in ? # x #|},
  ),
  test_probe_placement(
    /* Probing whole let vs body is semantically equivalent (same value).
     * Current implementation keeps it simpler by using default behavior. */
    ~name="Normal single line let (no hole body)",
    ~code={|let x = 2 + 1 in x # let x = 2 + 1 in x #|},
  ),
];

/* IF EXPRESSION SPECIAL CASES - multi-line if prefers trailing else */
let if_expression_tests = [
  test_probe_placement(
    ~name="Single-line if - default behavior",
    ~code=
      {|let result = if c then a else b in # if c then a else b #
1 + 1 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Multi-line if - probe branches",
    ~code=
      {|let result =  # result #
  if condition then   # condition #
  branch1             # branch1 #
  else branch2 in     # branch2 #
1 + 1                 # 1 + 1 #|},
  ),
  test_probe_placement(
    ~name="Nested if - probe branches",
    ~code=
      {|let complex =   # complex #
  if outer_cond then    # outer_cond #
    if inner_cond then  # inner_cond #
    val1                # val1 #
    else val2           # val2 #
  else val3 in          # val3 #
1 + 1                   # 1 + 1 #|},
  ),
];

/* FUNCTION TYPE FILTERING - avoid probing function values */
let function_type_tests = [
  test_probe_placement(
    ~name=
      "Function literal - probe function body, but not var of function type",
    ~code={|
let adder =
  fun x -> x + 1 in      # x + 1 #
adder|},
  ),
];

/* CASE EXPRESSION SPECIAL CASES - multi-tile forms (case + rules) */
let case_expression_tests = [
  test_probe_placement(
    ~name="Single-line case - probe scrutinee and case expression",
    ~code={|case 0      # 0 #
| _ => 1 end # case 0 | _ => 1 end #|},
  ),
  test_probe_placement(
    ~name="Multi-line case - end on own line",
    ~code=
      {|case 0      # 0 #
| _ => 1    # 1 #
end         # case 0 | _ => 1 end #|},
  ),
  test_probe_placement(
    ~name="Case with multiple branches",
    ~code=
      {|case x        # x #
| 0 => a      # a #
| _ => b      # b #
end           # case x | 0 => a | _ => b end #|},
  ),
];

let tests = [
  ("AutoProbe.Basic", basic_tests),
  ("AutoProbe.DefaultSelection", nested_multiline_tests),
  ("AutoProbe.HoleAvoidance", hole_avoidance_tests),
  ("AutoProbe.Containers", container_tests),
  ("AutoProbe.LetExpressions", let_expression_tests),
  ("AutoProbe.IfExpressions", if_expression_tests),
  ("AutoProbe.FunctionTypes", function_type_tests),
  ("AutoProbe.CaseExpressions", case_expression_tests),
];
