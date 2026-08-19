open Alcotest;
open Language;
open Test_Evaluator_Prelude;

module Shape = Haz3lcore.MvuShape;

/* Coordinates are computed, so compare them with a tolerance. */
let approx = float(1e-9);

/* Tests for the charting library in hazel-programs/charts/charts.hz.
 *
 * There is no chart type or chart projector in Hazel: a chart is an ordinary
 * HTML value built from Node/Create. So these tests evaluate the library and
 * assert on the value tree it produces — which is also the first thing the d3
 * chart projector could not be tested for, since its output only ever existed
 * inside d3's DOM.
 *
 * The library under test is the shipped slide itself, cut at the gallery
 * header, so nothing here is a copy that can drift from what users read. */

let gallery_marker = "# ===== A GALLERY OF ALL FIVE KINDS ===== #";

let library: string = {
  let text =
    switch (List.assoc_opt("Charts / Charts", Charts.Slides.all_slides)) {
    | Some({backup_text, _}: Haz3lcore.PersistentZipper.t) => backup_text
    | None => Alcotest.fail("the Charts slide is not registered")
    };
  let rec take = (acc, lines) =>
    switch (lines) {
    | [] =>
      Alcotest.fail(
        "charts.hz no longer contains the line " ++ gallery_marker,
      )
    | [line, ...rest] =>
      String.trim(line) == gallery_marker
        ? String.concat("\n", List.rev(acc)) : take([line, ...acc], rest)
    };
  take([], String.split_on_char('\n', text));
};

let eval = (expr: string): Exp.t =>
  parse_and_evaluate(library ++ "\n" ++ expr);

/* Every fixture is evaluated in ONE pass. Elaborating the library costs a
   few seconds, so a fixture per test would put this suite in the minutes;
   batching keeps it comparable to its neighbours. */
let batch = (fields: list((string, string))): Lazy.t(string => Exp.t) =>
  lazy({
    let program =
      "("
      ++ String.concat(
           ", ",
           List.map(((name, expr)) => name ++ " = " ++ expr, fields),
         )
      ++ ")";
    let value = eval(program);
    let entries =
      switch (Shape.of_tuple(value)) {
      | Some(elements) => List.filter_map(Shape.of_field, elements)
      | None => Alcotest.fail("fixture batch did not evaluate to a tuple")
      };
    name =>
      switch (List.assoc_opt(name, entries)) {
      | Some(v) => v
      | None => Alcotest.fail("no fixture named " ++ name)
      };
  });

/* === Reading the produced value tree === */

type node = {
  tag: string,
  attrs: list((string, string)),
  kids: list(Exp.t),
};

let create_pair = (attr: Exp.t): option((string, string)) =>
  switch (Shape.of_constructor(attr)) {
  | Some(("Create", body)) => Shape.of_pair(body)
  | _ => None
  };

let node_of = (exp: Exp.t): option(node) =>
  switch (Shape.of_constructor(exp)) {
  | Some(("Node", body)) =>
    switch (Shape.of_tuple(body)) {
    | Some([tag, attrs, kids]) =>
      switch (
        Shape.of_string(tag),
        Shape.of_list(attrs),
        Shape.of_list(kids),
      ) {
      | (Some(tag), Some(attrs), Some(kids)) =>
        Some({
          tag,
          attrs: List.filter_map(create_pair, attrs),
          kids,
        })
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

/* Every element in the tree, outermost first. */
let rec elements = (exp: Exp.t): list(node) =>
  switch (node_of(exp)) {
  | None => []
  | Some(n) => [n, ...List.concat_map(elements, n.kids)]
  };

let root = (exp: Exp.t): node =>
  switch (node_of(exp)) {
  | Some(n) => n
  | None => Alcotest.fail("expected an SVG element, got: " ++ Exp.show(exp))
  };

let tagged = (tag: string, exp: Exp.t): list(node) =>
  List.filter(n => n.tag == tag, elements(exp));

let attr = (n: node, k: string): option(string) =>
  List.assoc_opt(k, n.attrs);

let attr_f = (n: node, k: string): float =>
  switch (attr(n, k)) {
  | Some(v) => float_of_string(v)
  | None => Alcotest.fail("element <" ++ n.tag ++ "> has no " ++ k)
  };

/* Text content of an element, including a nested <title>. */
let rec text_of = (exp: Exp.t): string =>
  switch (Shape.of_constructor(exp)) {
  | Some(("Text", body)) => Option.value(Shape.of_string(body), ~default="")
  | Some(("Node", _)) =>
    switch (node_of(exp)) {
    | Some(n) => String.concat("", List.map(text_of, n.kids))
    | None => ""
    }
  | _ => ""
  };

let all_text = (exp: Exp.t): list(string) =>
  List.map(
    n => String.concat("", List.map(text_of, n.kids)),
    elements(exp),
  );

/* The tooltip a mark carries, which is how a test names a specific bar. */
let title_of = (n: node): string =>
  switch (List.filter(k => text_of(k) != "", n.kids)) {
  | [] => ""
  | [k, ..._] => text_of(k)
  };

let floats = (exp: Exp.t): list(float) =>
  switch (Shape.of_list(exp)) {
  | Some(items) => List.filter_map(Shape.of_float, items)
  | None => Alcotest.fail("expected a list of floats: " ++ Exp.show(exp))
  };

let is_sorted_strictly = (xs: list(float)): bool =>
  switch (xs) {
  | []
  | [_] => true
  | [x, ...rest] =>
    fst(
      List.fold_left(
        ((ok, prev), y) => (ok && prev < y, y),
        (true, x),
        rest,
      ),
    )
  };

/* === Fixtures === */

/* One program, one evaluation, every case below. */
let fixtures =
  batch([
    ("bar", {|Chart.bar([("A", 3.0), ("B", 5.0), ("C", 1.0)])|}),
    (
      "grouped",
      {|Chart.groupedBar([
          (name="s1", data=[("A", 1.0), ("B", 2.0)]),
          (name="s2", data=[("B", 3.0), ("C", 4.0)])
        ])|},
    ),
    (
      "sharedCategory",
      {|Chart.groupedBar([
          (name="s1", data=[("A", 1.0)]),
          (name="s2", data=[("A", 2.0)])
        ])|},
    ),
    ("line", {|Chart.line([(0.0, 1.0), (1.0, 4.0), (2.0, 2.0)])|}),
    ("scatter", {|Chart.scatter([(0.0, 1.0), (1.0, 4.0), (2.0, 2.0)])|}),
    ("pie", {|Chart.pie([("X", 10.0), ("Y", 30.0)])|}),
    ("pieLone", {|Chart.pie([("Only", 5.0)])|}),
    (
      "negative",
      {|Chart.pie([("X", 10.0), ("Y", 0.0 -. 5.0), ("Z", 10.0)])|},
    ),
    ("emptyBar", {|Chart.bar([])|}),
    ("emptyLine", {|Chart.line([])|}),
    ("emptyPie", {|Chart.pie([])|}),
    ("emptyGrouped", {|Chart.groupedBar([])|}),
    ("flat", {|Chart.line([(0.0, 5.0), (1.0, 5.0), (2.0, 5.0)])|}),
    ("allEqual", {|Chart.bar([("A", 5.0), ("B", 5.0)])|}),
    ("nice", {|Scale.nice(0.0, 42.0, 4)|}),
    ("niceFlat", {|Scale.nice(5.0, 5.0, 4)|}),
    ("ticks", {|Scale.ticks(0.0, 50.0, 10.0)|}),
    ("band", {|Scale.band(4, 0, 0.0, 100.0, 0.0)|}),
  ]);

let fixture = (name: string): Exp.t => Lazy.force(fixtures, name);

/* Marks carry a tooltip; legend swatches and gridlines do not. */
let marks = (tag: string, exp: Exp.t): list(node) =>
  List.filter(n => title_of(n) != "", tagged(tag, exp));

/* === Tests === */

let tests = (
  "Charts",
  [
    test_case(
      "bar: one rect per category, inside the plot",
      `Quick,
      () => {
        let v = fixture("bar");
        check(string, "root is an svg", "svg", root(v).tag);
        let bars = tagged("rect", v);
        check(int, "three bars", 3, List.length(bars));
        check(
          bool,
          "bars run left to right",
          true,
          is_sorted_strictly(List.map(n => attr_f(n, "x"), bars)),
        );
        check(
          bool,
          "every bar has positive height",
          true,
          List.for_all(n => attr_f(n, "height") > 0.0, bars),
        );
      },
    ),
    test_case(
      "bar: axis, category and value labels are drawn",
      `Quick,
      () => {
        let texts = all_text(fixture("bar"));
        List.iter(
          s => check(bool, "renders " ++ s, true, List.mem(s, texts)),
          ["A", "B", "C", "0", "5"],
        );
      },
    ),
    test_case("bar: tooltips name the category and value", `Quick, () =>
      check(
        list(string),
        "one title per bar",
        ["A: 3", "B: 5", "C: 1"],
        List.map(title_of, tagged("rect", fixture("bar"))),
      )
    ),
    /* The alignment fix this library inherits from the chart projector:
       series need not share categories, and a series missing one must leave
       a gap rather than shift its remaining bars into the next slot. */
    test_case(
      "grouped: series align by label, not position",
      `Quick,
      () => {
        let bars = marks("rect", fixture("grouped"));
        check(int, "four marks, not six", 4, List.length(bars));
        check(
          list(string),
          "each mark keeps its own label",
          ["A: 1", "B: 2", "B: 3", "C: 4"],
          List.map(title_of, bars),
        );
        /* Series-major order over A, B, B, C: strictly increasing x holds only
           if s2 sits in B and C. Positional placement would put it in A and B,
           sending "B: 3" to the left of "B: 2". */
        check(
          bool,
          "s2 sits in B and C, not A and B",
          true,
          is_sorted_strictly(List.map(n => attr_f(n, "x"), bars)),
        );
      },
    ),
    test_case(
      "grouped: a shared category holds both series",
      `Quick,
      () => {
        let v = fixture("sharedCategory");
        check(int, "two marks", 2, List.length(marks("rect", v)));
        let texts = all_text(v);
        check(bool, "legend names s1", true, List.mem("s1", texts));
        check(bool, "legend names s2", true, List.mem("s2", texts));
      },
    ),
    test_case(
      "line connects its points; scatter does not",
      `Quick,
      () => {
        let l = fixture("line");
        let s = fixture("scatter");
        check(int, "line draws a path", 1, List.length(tagged("path", l)));
        check(
          int,
          "line marks each point",
          3,
          List.length(tagged("circle", l)),
        );
        check(
          int,
          "scatter draws no path",
          0,
          List.length(tagged("path", s)),
        );
        check(
          int,
          "scatter marks each point",
          3,
          List.length(tagged("circle", s)),
        );
      },
    ),
    test_case(
      "pie: one slice per value, with a legend",
      `Quick,
      () => {
        let v = fixture("pie");
        check(int, "two slices", 2, List.length(tagged("path", v)));
        check(
          bool,
          "legend pairs label and value",
          true,
          List.mem("X  10", all_text(v)),
        );
      },
    ),
    /* A slice covering the whole circle has no distinct arc endpoints, so an
       arc path would collapse to nothing visible. */
    test_case(
      "pie: a lone slice is drawn as a circle",
      `Quick,
      () => {
        let v = fixture("pieLone");
        check(int, "no degenerate arc", 0, List.length(tagged("path", v)));
        check(
          int,
          "a full circle instead",
          1,
          List.length(tagged("circle", v)),
        );
      },
    ),
    /* Clamping a negative to zero must drop its slice entirely rather than
       draw a zero-width wedge. */
    test_case(
      "pie: negative values are dropped, not drawn",
      `Quick,
      () => {
        let slices = tagged("path", fixture("negative"));
        check(int, "only the two positive slices", 2, List.length(slices));
        check(
          list(string),
          "and the negative is not among them",
          ["X: 10", "Z: 10"],
          List.map(title_of, slices),
        );
      },
    ),
    test_case("empty data says so instead of drawing axes", `Quick, () =>
      List.iter(
        name => {
          let v = fixture(name);
          check(
            bool,
            name ++ ": says no data",
            true,
            List.mem("no data", all_text(v)),
          );
          check(
            int,
            name ++ ": draws no marks",
            0,
            List.length(tagged("rect", v)),
          );
          check(
            int,
            name ++ ": draws no slices",
            0,
            List.length(tagged("path", v)),
          );
        },
        ["emptyBar", "emptyLine", "emptyPie", "emptyGrouped"],
      )
    ),
    /* A flat series has a zero-width domain; nothing may divide by it. */
    test_case(
      "a constant series produces no NaN coordinates",
      `Quick,
      () => {
        let values =
          List.concat_map(
            n => List.map(snd, n.attrs),
            elements(fixture("flat")),
          );
        check(
          bool,
          "no attribute is nan or inf",
          false,
          List.exists(
            v => {
              let v = String.lowercase_ascii(v);
              v == "nan" || v == "inf" || v == "-inf";
            },
            values,
          ),
        );
      },
    ),
    test_case(
      "equal values still yield drawable bars",
      `Quick,
      () => {
        let bars = tagged("rect", fixture("allEqual"));
        check(int, "two bars", 2, List.length(bars));
        check(
          bool,
          "both have positive height",
          true,
          List.for_all(n => attr_f(n, "height") > 0.0, bars),
        );
      },
    ),
    test_case("Scale.nice rounds a domain outward to whole steps", `Quick, () =>
      switch (Shape.of_tuple(fixture("nice"))) {
      | Some([lo, hi, step]) =>
        check(option(approx), "lo", Some(0.0), Shape.of_float(lo));
        check(option(approx), "hi", Some(50.0), Shape.of_float(hi));
        check(option(approx), "step", Some(10.0), Shape.of_float(step));
      | _ => Alcotest.fail("Scale.nice did not return a triple")
      }
    ),
    test_case("Scale.nice widens a degenerate domain", `Quick, () =>
      switch (Shape.of_tuple(fixture("niceFlat"))) {
      | Some([lo, hi, _]) =>
        check(
          bool,
          "lo and hi differ",
          true,
          Shape.of_float(lo) != Shape.of_float(hi),
        )
      | _ => Alcotest.fail("Scale.nice did not return a triple")
      }
    ),
    test_case("Scale.ticks covers the domain inclusively", `Quick, () =>
      check(
        list(approx),
        "0 to 50 by 10",
        [0.0, 10.0, 20.0, 30.0, 40.0, 50.0],
        floats(fixture("ticks")),
      )
    ),
    test_case("Scale.band splits a range into bands", `Quick, () =>
      switch (Shape.of_tuple(fixture("band"))) {
      | Some([start, width]) =>
        check(
          option(approx),
          "the first band starts at the range start",
          Some(0.0),
          Shape.of_float(start),
        );
        check(
          option(approx),
          "unpadded bands fill the range",
          Some(25.0),
          Shape.of_float(width),
        );
      | _ => Alcotest.fail("Scale.band did not return a pair")
      }
    ),
  ],
);
