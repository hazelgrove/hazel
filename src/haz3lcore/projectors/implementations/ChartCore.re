open Util;
open Language;

/* ChartCore: pure recognizer that turns an elaborated Hazel expression of the
 * built-in `Chart` ADT (BuiltinsADT.Chart) into a `chart_spec`.
 *
 * This module is intentionally free of any rendering / Js_of_ocaml code so it
 * can be unit-tested natively (see test/Test_ChartCore.re). The D3-backed SVG
 * rendering lives in ChartProj/D3, which consume the `chart_spec` produced
 * here.
 *
 * The ADT constructors carry the same labeled-tuple data that the Table
 * projector recognizes, so extraction reuses TableCore.parse_table to pull out
 * (headers, rows), then interprets the columns per chart kind. */

[@deriving sexp]
type point = {
  x: float,
  y: float,
};

/* A named numeric column. Bar charts carry one series per numeric column,
 * which lets a single recognizer produce both single-series and grouped
 * (multi-numeric-column) bar charts. */
[@deriving sexp]
type series = {
  name: string,
  values: list(float),
};

[@deriving sexp]
type chart_spec =
  | Bar({
      categories: list(string),
      series: list(series),
    })
  | Pie(list((string, float)))
  | Line(list(point))
  | Scatter(list(point));

let strip = HazelJson.JsonADT.strip_wrappers;

/* Extract a finite float from a value expression, leniently accepting integer
 * literals (which the ADT's Float fields rule out statically, but which can
 * still appear when statics are disabled) and negated literals. */
let rec as_float = (e: Exp.t): option(float) =>
  switch (strip(e).term) {
  | Atom(Float(f)) => Float.is_finite(f) ? Some(f) : None
  | Atom(Int(i))
  | Atom(Nat(i)) => Some(Bigint.to_float(i))
  | Atom(SInt(i)) => Some(float_of_int(i))
  | UnOp(Int(Minus) | Float(Minus) | SInt(Minus) | Nat(Minus), inner) =>
    as_float(inner) |> Option.map(f => -. f)
  | _ => None
  };

let as_string = (e: Exp.t): option(string) =>
  switch (strip(e).term) {
  | Atom(String(s)) => Some(s)
  | _ => None
  };

/* Transpose a (headers, rows) table into a list of (header, column-cells). */
let columns =
    (headers: list(option(string)), rows: list(list(Exp.t)))
    : list((option(string), list(Exp.t))) => {
  let n = List.length(headers);
  List.init(n, j =>
    (List.nth(headers, j), List.map(row => List.nth(row, j), rows))
  );
};

let is_numeric_column = (cells: list(Exp.t)): bool =>
  cells != [] && List.for_all(c => as_float(c) != None, cells);

let is_string_column = (cells: list(Exp.t)): bool =>
  cells != [] && List.for_all(c => as_string(c) != None, cells);

/* Categorical data: one string "label" column plus one or more numeric value
 * columns. Each numeric column becomes a series (one for plain bars, several
 * for grouped bars). */
let parse_categorical = (arg: Exp.t): option((list(string), list(series))) =>
  switch (strip(arg).term) {
  | ListLit([]) => Some(([], []))
  | _ =>
    switch (TableCore.parse_table(arg)) {
    | None => None
    | Some((headers, rows)) =>
      let cols = columns(headers, rows);
      let indexed = List.mapi((i, c) => (i, c), cols);
      let label_idx =
        switch (
          List.find_opt(((_, (h, _))) => h == Some("label"), indexed)
        ) {
        | Some((i, _)) => Some(i)
        | None =>
          List.fold_left(
            (acc, (i, (_, cells))) =>
              switch (acc) {
              | Some(_) => acc
              | None => is_string_column(cells) ? Some(i) : None
              },
            None,
            indexed,
          )
        };
      switch (label_idx) {
      | None => None
      | Some(li) =>
        let (_, label_cells) = List.nth(cols, li);
        let categories = List.filter_map(as_string, label_cells);
        let series =
          List.filter_map(
            ((i, (h, cells))) =>
              if (i == li || !is_numeric_column(cells)) {
                None;
              } else {
                Some({
                  name: Option.value(~default="value", h),
                  values: List.filter_map(as_float, cells),
                });
              },
            indexed,
          );
        switch (series) {
        | [] => None
        | _ => Some((categories, series))
        };
      };
    }
  };

/* (x, y) point data: prefer columns labeled x / y, else fall back to the
 * first two numeric columns positionally. */
let parse_points = (arg: Exp.t): option(list(point)) =>
  switch (strip(arg).term) {
  | ListLit([]) => Some([])
  | _ =>
    switch (TableCore.parse_table(arg)) {
    | None => None
    | Some((headers, rows)) =>
      let cols = columns(headers, rows);
      let numeric =
        List.filter(((_, cells)) => is_numeric_column(cells), cols);
      let pick = (name, fallback_idx) =>
        switch (List.find_opt(((h, _)) => h == Some(name), cols)) {
        | Some((_, cells)) => Some(cells)
        | None =>
          List.nth_opt(numeric, fallback_idx) |> Option.map(((_, c)) => c)
        };
      switch (pick("x", 0), pick("y", 1)) {
      | (Some(xs), Some(ys)) =>
        let xf = List.filter_map(as_float, xs);
        let yf = List.filter_map(as_float, ys);
        let n = List.length(rows);
        List.length(xf) == n && List.length(yf) == n
          ? Some(
              List.map2(
                (x, y) =>
                  {
                    x,
                    y,
                  },
                xf,
                yf,
              ),
            )
          : None;
      | _ => None
      };
    }
  };

let rec combine_truncating = (xs: list('a), ys: list('b)): list(('a, 'b)) =>
  switch (xs, ys) {
  | ([x, ...xs], [y, ...ys]) => [(x, y), ...combine_truncating(xs, ys)]
  | _ => []
  };

let parse_chart = (exp: Exp.t): option(chart_spec) => {
  let exp = strip(exp);
  switch (exp.term) {
  | Ap(_, fn, arg) =>
    let fn = strip(fn);
    let arg = strip(arg);
    switch (fn.term) {
    | Constructor("BarChart", _) =>
      parse_categorical(arg)
      |> Option.map(((categories, series)) =>
           Bar({
             categories,
             series,
           })
         )
    | Constructor("PieChart", _) =>
      parse_categorical(arg)
      |> Option.map(((categories, series)) => {
           let values =
             switch (series) {
             | [s, ..._] => s.values
             | [] => []
             };
           Pie(combine_truncating(categories, values));
         })
    | Constructor("LineChart", _) =>
      parse_points(arg) |> Option.map(pts => Line(pts))
    | Constructor("ScatterChart", _) =>
      parse_points(arg) |> Option.map(pts => Scatter(pts))
    | _ => None
    };
  | _ => None
  };
};
