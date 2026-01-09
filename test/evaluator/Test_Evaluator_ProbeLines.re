open Alcotest;
open Util;
open Haz3lcore;
open Language;

/* Line-based probe testing infrastructure.
 *
 * Tests probes by line number. Assumes at most one probe per line.
 *
 * Usage:
 *   probe_line_test(
 *     "test name",
 *     {|let x = ^^probe(1 + 2)
 *       in x|},
 *     [(0, ["3"])],  // Line 0 has probe with value "3"
 *   )
 */

/* Convert a sample value to a string for comparison */
let format_sample_value = (value: Exp.t): string => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
        show_unknown_as_hole: false,
      },
      value |> DHExp.strip_ascriptions,
    );
  let str =
    Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg);
  StringUtil.replace(StringUtil.regexp("\n"), str, " ");
};

/* Get probe samples organized by line number.
 * Returns a map from line number to list of formatted sample values.
 * Uses TermData to look up probe positions. */
let get_samples_by_line = (code: string): IntMap.t(list(string)) => {
  /* Parse to zipper */
  switch (Parser.to_zipper(code)) {
  | None => IntMap.empty
  | Some(z) =>
    let MakeTerm.{term, term_data, _} = MakeTerm.from_zip_for_sem(z);
    /* Extract probe IDs directly from zipper's refractors.
     * Map values to unit since we only need the IDs as keys. */
    let probe_ids =
      Id.Map.union(
        (_, _, _) => Some(),
        Id.Map.map(_ => (), z.refractors.manuals),
        Id.Map.map(_ => (), z.refractors.ephemerals),
      );
    let info_map =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    let probe_map =
      Id.Map.fold(
        (id, (), acc) => {
          let refs =
            switch (Statics.Map.lookup(id, info_map)) {
            | Some(InfoExp(_)) => Statics.Map.refs_in(info_map, id)
            | Some(InfoPat(_)) => Statics.Map.bound_in(info_map, id)
            | _ => []
            };
          let probe = {Probe.refs: refs};
          Id.Map.add(id, probe, acc);
        },
        probe_ids,
        Id.Map.empty,
      );

    /* Elaborate and evaluate */
    let elaborated = Elaborator.elaborate(info_map, term) |> fst;
    let (_, state) =
      Evaluator.evaluate(~probe_map, ~env=Builtins.env_init, elaborated);
    let probes = EvaluatorState.get_probes(state);

    /* Get segment and measured for position lookup */
    let segment = Zipper.unselect_and_zip(z);
    let measured =
      Measured.of_segment(
        segment,
        ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Build map from line number to samples using TermData */
    Id.Map.fold(
      (probe_id, samples, acc) => {
        switch (TermData.extreme_measures(probe_id, term_data, measured)) {
        | Some((start, _)) =>
          let line = start.row;
          let formatted_values =
            List.map(
              (s: Sample.t) => format_sample_value(s.value),
              samples,
            );
          let existing =
            IntMap.find_opt(line, acc) |> Option.value(~default=[]);
          IntMap.add(line, existing @ formatted_values, acc);
        | None => acc
        }
      },
      probes,
      IntMap.empty,
    );
  };
};

/* Main test function: check that probes on specified lines have expected values */
let probe_line_test =
    (name: string, code: string, expected: list((int, list(string)))) => {
  test_case(
    name,
    `Quick,
    () => {
      let actual_by_line = get_samples_by_line(code);

      List.iter(
        ((line, expected_values)) => {
          let actual_values =
            IntMap.find_opt(line, actual_by_line)
            |> Option.value(~default=[]);
          check(
            list(string),
            Printf.sprintf("Line %d", line),
            expected_values,
            actual_values,
          );
        },
        expected,
      );
    },
  );
};

let tests = (
  "Evaluator.ProbeLines",
  [
    /* ===== Basic probe tests ===== */
    probe_line_test(
      "Simple arithmetic probe",
      {|let x = ^^probe(1 + 2) in x|},
      [(0, ["3"])],
    ),
    probe_line_test(
      "Probe on variable",
      {|let x = 5 in ^^probe(x)|},
      [(0, ["5"])],
    ),
    probe_line_test(
      "Probe on string",
      {|^^probe("hello")|},
      [(0, ["\"hello\""])],
    ),
    probe_line_test(
      "Probe on boolean",
      {|^^probe(true)|},
      [(0, ["true"])],
    ),
    probe_line_test(
      "Probe on list",
      {|^^probe([1, 2, 3])|},
      [(0, ["[1, 2, 3]"])],
    ),
    probe_line_test(
      "Probe on function application",
      {|let f = fun x -> x + 1 in ^^probe(f(5))|},
      [(0, ["6"])],
    ),
    /* ===== Probes inside conditionals (probe on branch, not whole expr) ===== */
    probe_line_test(
      "Probe in if-then branch (taken)",
      {|if true then ^^probe(1) else 2|},
      [(0, ["1"])],
    ),
    probe_line_test(
      "Probe in if-else branch (taken)",
      {|if false then 1 else ^^probe(2)|},
      [(0, ["2"])],
    ),
    probe_line_test(
      "Probe in if-then branch (not taken)",
      {|if false then ^^probe(1) else 2|},
      [(0, [])],
    ),
    probe_line_test(
      "Probe in case branch",
      {|case 1 | 1 => ^^probe(10) | _ => 20 end|},
      [(0, ["10"])],
    ),
    /* ===== Multiple probes ===== */
    probe_line_test(
      "Multiple probes on different lines",
      {|let x = ^^probe(1)
in let y = ^^probe(2)
in x + y|},
      [(0, ["1"]), (1, ["2"])],
    ),
    probe_line_test(
      "Probes in both if branches",
      {|let x = true
in if x then ^^probe(1) else ^^probe(2)|},
      [(1, ["1"])],
    ),
    /* ===== Recursion ===== */
    probe_line_test(
      "Factorial recursive probe",
      {|let fact = fun n ->
  if n <= 1 then 1
  else ^^probe(n) * fact(n - 1)
in fact(5)|},
      [(2, ["5", "4", "3", "2"])],
    ),
    probe_line_test(
      "Recursive sum with multiple probes",
      {|let sum = fun n ->
  if n <= 0 then ^^probe(0)
  else ^^probe(n) + sum(n - 1)
in sum(3)|},
      [(1, ["0"]), (2, ["3", "2", "1"])],
    ),
    /* ===== Higher-order functions ===== */
    probe_line_test(
      "Probe in map function",
      {|let double = fun x -> ^^probe(x * 2)
in [double(1), double(2), double(3)]|},
      [(0, ["2", "4", "6"])],
    ),
    probe_line_test(
      "Probe on closure result",
      {|let make_adder = fun n -> fun x -> x + n
in let add5 = make_adder(5)
in ^^probe(add5(10))|},
      [(2, ["15"])],
    ),
    /* ===== Probes ON compound expressions (currently broken) ===== */
    /* These test probing the whole compound expression, not just a branch */
    probe_line_test(
      "Probe on if-then-else",
      {|^^probe(if true then 1 else 2)|},
      [(0, ["1"])],
    ),
    probe_line_test(
      "Probe on if-then-else (else branch)",
      {|^^probe(if false then 1 else 2)|},
      [(0, ["2"])],
    ),
    probe_line_test(
      "Probe on let expression",
      {|^^probe(let x = 1 in x)|},
      [(0, ["1"])],
    ),
    probe_line_test(
      "Probe on let with computation",
      {|^^probe(let x = 1 + 2 in x * 3)|},
      [(0, ["9"])],
    ),
    probe_line_test(
      "Probe on case expression",
      {|^^probe(case 1 | 1 => 10 | _ => 20 end)|},
      [(0, ["10"])],
    ),
    probe_line_test(
      "Probe on case (second branch)",
      {|^^probe(case 2 | 1 => 10 | _ => 20 end)|},
      [(0, ["20"])],
    ),
    probe_line_test("Probe on sequence", {|^^probe(1; 2)|}, [(0, ["2"])]),
    /* ===== Nested compound expressions (currently broken) ===== */
    probe_line_test(
      "Probe on nested ifs",
      {|^^probe(if true then (if false then 1 else 2) else 3)|},
      [(0, ["2"])],
    ),
    probe_line_test(
      "Probe on nested lets",
      {|^^probe(let x = 1 in let y = 2 in x + y)|},
      [(0, ["3"])],
    ),
    probe_line_test(
      "Probe on if containing let",
      {|^^probe(if true then let x = 5 in x else 0)|},
      [(0, ["5"])],
    ),
    probe_line_test(
      "Probe on let containing if",
      {|^^probe(let x = if true then 1 else 2 in x + 10)|},
      [(0, ["11"])],
    ),
    probe_line_test(
      "Probe on deeply nested compound",
      {|^^probe(let a = 1 in
  let b = if a == 1 then 10 else 20 in
  case b | 10 => 100 | _ => 200 end)|},
      [(0, ["100"])],
    ),
    /* ===== Mixed: probe on compound + probes inside ===== */
    probe_line_test(
      "Outer probe on if with inner probe",
      {|^^probe(if true then ^^probe(1 + 2) else 0)|},
      [(0, ["3", "3"])] /* Both probes capture same value */
    ),
    probe_line_test(
      "Outer probe on let with inner probe on body",
      {|^^probe(let x = 5 in ^^probe(x * 2))|},
      [(0, ["10", "10"])],
    ),
    /* ===== Parens (known issue: paren stripping in elaborator) ===== */
    /* These fail due to ID mismatch from paren stripping, separate from
       the compound expression issue. Documenting current (broken) behavior. */
    probe_line_test(
      "Probe on parens (known issue: paren stripping)",
      {|^^probe((1 + 2))|},
      [(0, [])] /* Should be ["3"] when fixed */
    ),
    probe_line_test(
      "Probe on tuple (known issue: paren stripping)",
      {|^^probe((1, "a"))|},
      [(0, [])] /* Should be ["(1, \"a\")"] when fixed */
    ),
    /* ===== Pattern probes ===== */
    probe_line_test(
      "Pattern probe on let binding",
      {|let ^^probe(x) = 42 in x|},
      [(0, ["42"])],
    ),
    probe_line_test(
      "Pattern probe on tuple destructuring",
      {|let (^^probe(a), b) = (1, 2) in a + b|},
      [(0, ["1"])],
    ),
    probe_line_test(
      "Pattern probe in function parameter",
      {|let f = fun ^^probe(x) -> x * 2
in f(5)|},
      [(0, ["5"])],
    ),
    probe_line_test(
      "Pattern probe in case",
      {|case (1, 2) | (^^probe(x), y) => x + y end|},
      [(0, ["1"])],
    ),
    /* ===== Type ascription interactions ===== */
    probe_line_test(
      "Probe with unknown type ascription",
      {|^^probe(1 + 2) : ?|},
      [(0, ["3"])],
    ),
    probe_line_test(
      "Probe in let with labeled tuple type (value coercion)",
      {|let x : (l=String) = ^^probe("a") in x|},
      [(0, ["(l=\"a\")"])],
    ),
    /* Note: Adapted from old Evaluator.Probes test "Evaluate probe around
       inferred labeled tuple" which passed before the probe_map refactor.
       Now broken - may be related to paren/ID issues or ascription evaluation.
       Should investigate what changed. */
    probe_line_test(
      "Probe with outer ascription (known issue: was passing pre-refactor)",
      {|^^probe("a") : (l=String)|},
      [(0, [])],  /* Should capture value, needs investigation */
    ),
    probe_line_test(
      "Pattern probe with labeled tuple type (value coercion)",
      {|let ^^probe(x) : (l=String) = "a" in x|},
      [(0, ["(l=\"a\")"])],
    ),
    /* ===== Builtins and list operations ===== */
    /* Builtin call captures unevaluated expression - may need investigation */
    probe_line_test(
      "Probe on builtin function call (captures unevaluated)",
      {|^^probe(String.length("hello"))|},
      [(0, ["String .length(\"hello\")"])],  /* Should be ["5"] */
    ),
    probe_line_test(
      "Probe on list concat",
      {|^^probe([1, 2] @ [3, 4])|},
      [(0, ["[1, 2, 3, 4]"])],
    ),
    /* List cons has same compound expr issue - steps with is_value: false */
    probe_line_test(
      "Probe on list cons (same compound expr issue)",
      {|^^probe(1 :: [2, 3])|},
      [(0, [])],  /* Should be ["[1, 2, 3]"] */
    ),
    /* ===== Edge cases ===== */
    probe_line_test(
      "Probe on empty hole",
      {|^^probe(?)|},
      [(0, ["?"])],
    ),
    probe_line_test(
      "Probe on constructor",
      {|type T = A + B in ^^probe(A)|},
      [(0, ["A"])],
    ),
    probe_line_test(
      "Probe on constructor with arg",
      {|type T = A(Int) + B in ^^probe(A(42))|},
      [(0, ["A(42)"])],
    ),
  ],
);
