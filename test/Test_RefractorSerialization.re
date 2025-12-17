/**
 * Tests for refractor serialization - specifically testing that whitespace
 * is preserved when probes are serialized to invocation syntax.
 *
 * The issue: When serializing segments with refractors (probes stored indirectly
 * via term IDs), the skel-based approach in `refractor_seg_to_seg` discards
 * whitespace (all Secondary content including comments).
 *
 * Example:
 *   Input: "1 + 2 * 3" with probes on + and * expressions
 *   Expected: "^^probe(1 + ^^probe(2 * 3))"
 *   Actual (broken): "^^probe(1+^^probe(2*3))"
 *
 * Status: The `refractor_seg_to_seg` call in Base.re is currently commented out
 * because of this whitespace issue. These tests document the expected behavior
 * and demonstrate the bug in the `refractor_seg_to_seg` function directly.
 */
open Alcotest;
open Haz3lcore;
open Util;

/* Parse code and get the term with its term data */
let parse_with_term_data = (code: string): option((Segment.t, MakeTerm.t)) => {
  switch (Parser.to_segment(code)) {
  | Some(seg) =>
    let mt = MakeTerm.go(Id.Map.empty, seg);
    Some((seg, mt));
  | None => None
  };
};

/* Find the term ID of a bin op expression by its operator string.
 * This walks the term structure to find binary operations. */
let rec find_binop_ids = (op_str: string, exp: Language.Exp.t): list(Id.t) => {
  open Language;
  let ids = IdTagged.(exp.annotation.ids);
  let term_ids =
    switch (exp.term) {
    | BinOp(_, e1, e2) =>
      /* Check if this is the operator we're looking for */
      let is_target =
        switch (exp.term) {
        | BinOp(Int(Plus), _, _) when op_str == "+" => true
        | BinOp(Int(Times), _, _) when op_str == "*" => true
        | BinOp(Int(Minus), _, _) when op_str == "-" => true
        | BinOp(Int(Divide), _, _) when op_str == "/" => true
        | _ => false
        };
      let this_ids = is_target ? ids : [];
      this_ids @ find_binop_ids(op_str, e1) @ find_binop_ids(op_str, e2);
    | Parens(e) => find_binop_ids(op_str, e)
    | Let(_, def, body) =>
      find_binop_ids(op_str, def) @ find_binop_ids(op_str, body)
    | If(cond, then_, else_) =>
      find_binop_ids(op_str, cond)
      @ find_binop_ids(op_str, then_)
      @ find_binop_ids(op_str, else_)
    | Tuple(es) => List.concat_map(find_binop_ids(op_str), es)
    | ListLit(es) => List.concat_map(find_binop_ids(op_str), es)
    | Ap(_, e1, e2) =>
      find_binop_ids(op_str, e1) @ find_binop_ids(op_str, e2)
    | Fun(_, e, _, _) => find_binop_ids(op_str, e)
    | _ => []
    };
  term_ids;
};

/* Create a refractor map from a list of term IDs */
let make_refractors = (ids: list(Id.t)): Id.Map.t(Base.projector) =>
  List.fold_left(
    (map: Id.Map.t(Base.projector), id: Id.t) =>
      Id.Map.add(id, MkRefractor.mk(Probe, Id.transform_variant(id)), map),
    Id.Map.empty,
    ids,
  );

/* Serialize a segment with refractors to a string */
let serialize_with_refractors =
    (refractors: Id.Map.t(Base.projector), seg: Segment.t): string =>
  Printer.of_segment(~holes="?", ~refractors, seg);

/* Test helper to verify serialization preserves whitespace */
let test_refractor_whitespace =
    (
      ~name: string,
      ~code: string,
      ~probe_ops: list(string),
      ~expected: string,
    )
    : test_case(_) =>
  test_case(name, `Quick, () => {
    switch (parse_with_term_data(code)) {
    | None => fail("Failed to parse code: " ++ code)
    | Some((seg, mt)) =>
      /* Find all binop IDs for the specified operators */
      let ids =
        List.concat_map(
          (op: string) => find_binop_ids(op, mt.term),
          probe_ops,
        );

      /* Create refractors for those IDs */
      let refractors = make_refractors(ids);

      /* Serialize and compare */
      let actual = serialize_with_refractors(refractors, seg);

      check(string, "Serialization with probes", expected, actual);
    }
  });

/* These tests verify the expected end-to-end behavior of Printer.of_segment
 * when refractors are passed in. Currently these will FAIL because the
 * refractor_seg_to_seg call is commented out in Base.re, so refractors
 * are not being converted to ^^probe(...) syntax at all. */
let whitespace_preservation_tests = [
  test_refractor_whitespace(
    ~name="Simple addition with probe - whitespace preserved",
    ~code="1 + 2",
    ~probe_ops=["+"],
    ~expected="^^probe(1 + 2)",
  ),
  test_refractor_whitespace(
    ~name="Nested operators with probes - whitespace preserved",
    ~code="1 + 2 * 3",
    ~probe_ops=["+", "*"],
    ~expected="^^probe(1 + ^^probe(2 * 3))",
  ),
  test_refractor_whitespace(
    ~name="Single multiplication - whitespace preserved",
    ~code="2 * 3",
    ~probe_ops=["*"],
    ~expected="^^probe(2 * 3)",
  ),
  test_refractor_whitespace(
    ~name="Multiple spaces between operators",
    ~code="1  +  2",
    ~probe_ops=["+"],
    ~expected="^^probe(1  +  2)",
  ),
  test_refractor_whitespace(
    ~name="Complex expression with all operators probed",
    ~code="1 + 2 * 3 - 4",
    ~probe_ops=["+", "*", "-"],
    ~expected="^^probe(^^probe(1 + ^^probe(2 * 3)) - 4)",
  ),
];

/* Direct test of refractor_seg_to_seg function.
 * This tests the actual bug: when refractor_seg_to_seg transforms a segment,
 * it loses whitespace because it rebuilds the segment using Skel which
 * only tracks piece indices, not Secondary (whitespace/comments) content. */
let test_refractor_seg_to_seg =
    (~name: string, ~code: string, ~expected_with_whitespace: string)
    : test_case(_) =>
  test_case(name, `Quick, () => {
    switch (parse_with_term_data(code)) {
    | None => fail("Failed to parse code: " ++ code)
    | Some((seg, mt)) =>
      /* Find the root expression's ID */
      let root_id = List.hd(mt.term.annotation.ids);
      let refractors = make_refractors([root_id]);

      /* Call refractor_seg_to_seg directly */
      let (remaining_refractors, new_seg) =
        Triggers.refractor_seg_to_seg(refractors, seg);

      /* The refractor should have been consumed */
      check(
        bool,
        "Refractor was consumed",
        true,
        Id.Map.is_empty(remaining_refractors),
      );

      /* Serialize the new segment to see the result */
      let result = Printer.of_segment(~holes="?", new_seg);

      /* Print debug info for test output */
      print_endline("Input: " ++ code);
      print_endline("Expected: " ++ expected_with_whitespace);
      print_endline("Actual: " ++ result);

      /* This test documents the expected behavior (with whitespace preserved).
       * Currently this FAILS because refractor_seg_to_seg loses whitespace. */
      check(
        string,
        "Output preserves whitespace",
        expected_with_whitespace,
        result,
      );
    }
  });

let direct_function_tests = [
  test_refractor_seg_to_seg(
    ~name="refractor_seg_to_seg on 1 + 2 preserves spaces",
    ~code="1 + 2",
    ~expected_with_whitespace="^^probe(1 + 2)",
  ),
  test_refractor_seg_to_seg(
    ~name="refractor_seg_to_seg on 1 + 2 * 3 preserves spaces",
    ~code="1 + 2 * 3",
    ~expected_with_whitespace="^^probe(1 + 2 * 3)",
  ),
  test_refractor_seg_to_seg(
    ~name="refractor_seg_to_seg preserves multiple spaces",
    ~code="1  +  2",
    ~expected_with_whitespace="^^probe(1  +  2)",
  ),
];

let tests = [
  (
    "RefractorSerialization.WhitespacePreservation",
    whitespace_preservation_tests,
  ),
  ("RefractorSerialization.DirectFunction", direct_function_tests),
];
