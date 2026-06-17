/* Tests for ChartCore.parse_chart.
 *
 * The positive cases drive the full pipeline (parse -> elaborate ->
 * parse_chart). Using positional tuples (e.g. ("A", 3.0)) additionally
 * verifies that the built-in `Chart` ADT is registered in the typing context:
 * if BarChart/LineChart/... weren't registered with their labeled-tuple
 * payload types, elaboration wouldn't auto-label the rows and parse_chart
 * would return None.
 *
 * Edge cases that the ADT's types make unreachable through real programs
 * (grouped/multi-series bars, ragged rows, non-finite values) are exercised on
 * hand-built expressions, since parse_chart pattern-matches raw syntax. */

open Alcotest;
open Haz3lcore;

module E = Language.IdTagged.FreshGrammar.Exp;
let fwd = Language.Operators.Forward;

let parse_exp = (s: string) =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

let dhexp_of_uexp = u => {
  let (_, elab) =
    Language.Statics.mk(
      Language.CoreSettings.on,
      Language.Builtins.ctx_init(Some(Int)),
      u,
    );
  elab;
};

/* Find the first chart-constructor application in an elaborated program. */
let find_chart = (exp: Language.Exp.t): option(Language.Exp.t) => {
  module M = {
    exception Found(Language.Exp.t);
  };
  let names = ["BarChart", "LineChart", "ScatterChart", "PieChart"];
  let rec lstrip = (e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Parens(i)
    | Asc(i, _)
    | Closure(_, i)
    | Filter(_, i) => lstrip(i)
    | _ => e
    };
  let ctr_name = (e: Language.Exp.t) =>
    switch (lstrip(e).term) {
    | Constructor(n, _) => Some(n)
    | _ => None
    };
  switch (
    Language.Exp.map_term(
      ~f_exp=
        (cont, e) =>
          switch (e.term) {
          | Ap(_, fn, _) =>
            switch (ctr_name(fn)) {
            | Some(n) when List.mem(n, names) => raise(M.Found(e))
            | _ => cont(e)
            }
          | _ => cont(e)
          },
      exp,
    )
  ) {
  | exception (M.Found(x)) => Some(x)
  | _ => None
  };
};

let chart_of_program = (program_str: string): ChartCore.chart_spec => {
  let elaborated = dhexp_of_uexp(parse_exp(program_str));
  switch (find_chart(elaborated)) {
  | Some(e) =>
    switch (ChartCore.parse_chart(e)) {
    | Some(c) => c
    | None => Alcotest.fail("parse_chart returned None: " ++ program_str)
    }
  | None => Alcotest.fail("No chart constructor found: " ++ program_str)
  };
};

let flt = float(1e-9);

/* hand-built helpers */
let row = (cells: list((string, E.t))): E.t =>
  E.tuple(List.map(((l, v)) => E.tup_label(E.label(l), v), cells));
let bar_ap = (rows: list(E.t)): E.t =>
  E.ap(fwd, E.constructor("BarChart", None), E.list_lit(rows));

let tests = (
  "ChartCore.parse_chart",
  [
    test_case("BarChart (positional) auto-labels via the ADT", `Quick, () => {
      switch (chart_of_program({|BarChart([("A", 3.0), ("B", 5.0)])|})) {
      | Bar({categories, series: [{name: _, values}]}) =>
        check(list(string), "categories", ["A", "B"], categories);
        check(list(flt), "values", [3.0, 5.0], values);
      | _ => Alcotest.fail("expected single-series Bar")
      }
    }),
    test_case("BarChart (explicit labels)", `Quick, () => {
      switch (
        chart_of_program(
          {|BarChart([(label="A", value=3.0), (label="B", value=5.0)])|},
        )
      ) {
      | Bar({categories, series: [{values, _}]}) =>
        check(list(string), "categories", ["A", "B"], categories);
        check(list(flt), "values", [3.0, 5.0], values);
      | _ => Alcotest.fail("expected single-series Bar")
      }
    }),
    test_case("LineChart", `Quick, () => {
      switch (
        chart_of_program({|LineChart([(x=0.0, y=1.0), (x=1.0, y=4.0)])|})
      ) {
      | Line([{x: x0, y: y0}, {x: x1, y: y1}]) =>
        check(list(flt), "xs", [0.0, 1.0], [x0, x1]);
        check(list(flt), "ys", [1.0, 4.0], [y0, y1]);
      | _ => Alcotest.fail("expected Line with 2 points")
      }
    }),
    test_case("ScatterChart", `Quick, () => {
      switch (chart_of_program({|ScatterChart([(x=1.0, y=2.0)])|})) {
      | Scatter([{x, y}]) =>
        check(flt, "x", 1.0, x);
        check(flt, "y", 2.0, y);
      | _ => Alcotest.fail("expected Scatter with 1 point")
      }
    }),
    test_case("PieChart", `Quick, () => {
      switch (
        chart_of_program(
          {|PieChart([(label="X", value=10.0), (label="Y", value=30.0)])|},
        )
      ) {
      | Pie([("X", a), ("Y", b)]) =>
        check(list(flt), "slice values", [10.0, 30.0], [a, b])
      | _ => Alcotest.fail("expected Pie with 2 slices")
      }
    }),
    test_case("empty BarChart parses to an empty spec", `Quick, () => {
      switch (chart_of_program({|BarChart([])|})) {
      | Bar({categories: [], series: []}) => ()
      | _ => Alcotest.fail("expected empty Bar")
      }
    }),
    test_case("grouped bar: multiple numeric columns (hand-built)", `Quick, () => {
      let exp =
        bar_ap([
          row([("label", E.string("A")), ("q1", E.float(1.0)), ("q2", E.float(2.0))]),
          row([("label", E.string("B")), ("q1", E.float(3.0)), ("q2", E.float(4.0))]),
        ]);
      switch (ChartCore.parse_chart(exp)) {
      | Some(Bar({categories, series})) =>
        check(list(string), "categories", ["A", "B"], categories);
        check(int, "series count", 2, List.length(series));
        let names = List.map((s: ChartCore.series) => s.name, series);
        check(list(string), "series names", ["q1", "q2"], names);
      | _ => Alcotest.fail("expected grouped Bar with 2 series")
      };
    }),
    test_case("non-finite value rejected (hand-built)", `Quick, () => {
      let exp =
        bar_ap([row([("label", E.string("A")), ("value", E.float(Float.nan))])]);
      check(bool, "None", true, Option.is_none(ChartCore.parse_chart(exp)));
    }),
    test_case("ragged rows rejected (hand-built)", `Quick, () => {
      let exp =
        bar_ap([
          row([("label", E.string("A")), ("value", E.float(1.0))]),
          row([("label", E.string("B"))]),
        ]);
      check(bool, "None", true, Option.is_none(ChartCore.parse_chart(exp)));
    }),
    test_case("non-chart constructor rejected", `Quick, () => {
      let exp = E.ap(fwd, E.constructor("Some", None), E.int(3));
      check(bool, "None", true, Option.is_none(ChartCore.parse_chart(exp)));
    }),
    test_case("plain list is not a chart", `Quick, () => {
      let exp = E.list_lit([E.int(1), E.int(2)]);
      check(bool, "None", true, Option.is_none(ChartCore.parse_chart(exp)));
    }),
  ],
);
