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

/* Test for broken behavior: probe returns no samples */
let probe_broken_test = (name: string, code: string, line: int) => {
  test_case(
    name,
    `Quick,
    () => {
      let actual_by_line = get_samples_by_line(code);
      let actual_values =
        IntMap.find_opt(line, actual_by_line) |> Option.value(~default=[]);
      check(
        list(string),
        Printf.sprintf("Line %d should have no samples (broken)", line),
        [],
        actual_values,
      );
    },
  );
};

let tests = (
  "Evaluator.ProbeLines",
  [
    /* Basic probe tests */
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
    /* Conditionals */
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
    /* Case expressions */
    probe_line_test(
      "Probe in case branch",
      {|case 1 | 1 => ^^probe(10) | _ => 20 end|},
      [(0, ["10"])],
    ),
    /* Multiple probes on different lines */
    probe_line_test(
      "Multiple probes on different lines",
      {|let x = ^^probe(1)
in let y = ^^probe(2)
in x + y|},
      [(0, ["1"]), (1, ["2"])],
    ),
    /* Recursion - factorial */
    probe_line_test(
      "Factorial recursive probe",
      {|let fact = fun n ->
  if n <= 1 then 1
  else ^^probe(n) * fact(n - 1)
in fact(5)|},
      [(2, ["5", "4", "3", "2"])],
    ),
    /* Higher-order functions */
    probe_line_test(
      "Probe in map function",
      {|let double = fun x -> ^^probe(x * 2)
in [double(1), double(2), double(3)]|},
      [(0, ["2", "4", "6"])],
    ),
    /* Known broken: probe on parenthesized expression */
    probe_broken_test(
      "Probe on parens (known broken)",
      {|^^probe((1 + 2))|},
      0,
    ),
    /* Known broken: probe on tuple */
    probe_broken_test(
      "Probe on tuple (known broken)",
      {|^^probe((1, "a"))|},
      0,
    ),
    /* Known broken: probe on if-then-else */
    probe_broken_test(
      "Probe on if-then-else (known broken)",
      {|^^probe(if true then 1 else 2)|},
      0,
    ),
    /* Known broken: probe on case */
    probe_broken_test(
      "Probe on case expression (known broken)",
      {|^^probe(case 1 | 1 => 10 | _ => 20 end)|},
      0,
    ),
    probe_broken_test("Probe on let", {|^^probe(let x = 1 in x)|}, 0),
    /* Pattern probes */
    probe_line_test(
      "Pattern probe on let binding",
      {|let ^^probe(x) = 42 in x|},
      [(0, ["42"])],
    ),
  ],
);
