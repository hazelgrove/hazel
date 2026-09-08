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
 * (multi-numeric-column) bar charts.
 *
 * `values` is positional against the chart's `categories`: entry i is the
 * value at category i, and None means this series has no bar there. Only
 * grouped charts produce None — the series there are separate tables that
 * need not agree on their labels. */
[@deriving sexp]
type series = {
  name: string,
  values: list(option(float)),
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

/* Extract a finite float from a value expression. Only Float literals are
 * accepted — no implicit Int/Nat/SInt coercion. Chart data must be Float in
 * the surface language (use float_of_int etc. to convert explicitly), matching
 * the ADT's Float payload types. */
let rec as_float = (e: Exp.t): option(float) =>
  switch (strip(e).term) {
  | Atom(Float(f)) => Float.is_finite(f) ? Some(f) : None
  | UnOp(Float(Minus), inner) => as_float(inner) |> Option.map(f => -. f)
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
                  values: List.map(as_float, cells),
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

/* Zip, dropping any tail the shorter list doesn't cover. */
let rec combine_truncating = (xs: list('a), ys: list('b)): list(('a, 'b)) =>
  switch (xs, ys) {
  | ([x, ...xs], [y, ...ys]) => [(x, y), ...combine_truncating(xs, ys)]
  | _ => []
  };

/* Union of every series' labels, in first-seen order. */
let union_labels = (labelss: list(list(string))): list(string) =>
  List.fold_left(
    (acc, labels) =>
      List.fold_left(
        (acc, l) => List.mem(l, acc) ? acc : [l, ...acc],
        acc,
        labels,
      ),
    [],
    labelss,
  )
  |> List.rev;

/* Multi-series bar data: a list of named series, each carrying its own
 * (label, value) list. Produces one chart series per named entry.
 *
 * The category axis is the ordered union of every series' labels, and each
 * series' values are looked up by label — series need not carry the same
 * categories, or the same number of them, and one that skips a category
 * simply has no bar there instead of a bar shifted into someone else's
 * slot. A label repeated within one series takes its first value. */
let parse_grouped = (arg: Exp.t): option((list(string), list(series))) =>
  switch (strip(arg).term) {
  | ListLit([]) => Some(([], []))
  | _ =>
    switch (TableCore.parse_table(arg)) {
    | None => None
    | Some((headers, rows)) =>
      let cols = columns(headers, rows);
      let col = name =>
        List.find_opt(((h, _)) => h == Some(name), cols)
        |> Option.map(snd);
      switch (col("name"), col("data")) {
      | (Some(name_cells), Some(data_cells)) =>
        let names = List.filter_map(as_string, name_cells);
        let inner = List.map(parse_categorical, data_cells);
        switch (OptUtil.sequence(inner)) {
        | Some(parsed)
            when
              List.length(names) == List.length(name_cells)
              && List.length(names) == List.length(parsed) =>
          /* Each inner table is one series: pair its labels with its first
             numeric column. (The ADT types the payload as a single
             (label, value) list, so any further columns are stray syntax.) */
          let entries =
            List.map(
              ((cats, slist)) =>
                switch (slist) {
                | [s, ..._] => combine_truncating(cats, s.values)
                | [] => []
                },
              parsed,
            );
          let categories = union_labels(List.map(List.map(fst), entries));
          let series =
            List.map2(
              (name, entries) =>
                {
                  name,
                  values:
                    List.map(
                      c => Option.join(List.assoc_opt(c, entries)),
                      categories,
                    ),
                },
              names,
              entries,
            );
          Some((categories, series));
        | _ => None
        };
      | _ => None
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

/* Is this expression an application of a `Chart` ADT constructor?
 *
 * Identified by the type statics puts on the constructor rather than by a
 * list of constructor names: every Chart variant is covered without this
 * having to track them, and a same-named constructor from a user's own ADT
 * is correctly rejected. Elaborated expressions carry `Some(Some(ty))` on
 * constructors (see the Grammar.exp_term comment); user syntax carries
 * None, so this answers false there — the gate only runs on elaborated
 * results.
 *
 * Cheap enough to run on every application, which is the point: it decides
 * whether the (much costlier) parse_chart is worth attempting. */
let is_chart_ctr_ap = (exp: Exp.t): bool => {
  /* Constructor types are `payload -> Chart`; the nullary case can't be a
     chart, since every Chart variant takes data. */
  let rec result_typ = (ty: Typ.t): Typ.t =>
    switch (Typ.term_of(ty)) {
    | Parens(ty)
    | Arrow(_, ty) => result_typ(ty)
    | _ => ty
    };
  let is_chart_ctr = (e: Exp.t): bool =>
    switch (strip(e).term) {
    | Constructor(_, Some(Some(ty))) =>
      Typ.fast_equal(result_typ(ty), BuiltinsADT.Chart.t)
    | _ => false
    };
  switch (strip(exp).term) {
  | Ap(_, fn, _) => is_chart_ctr(fn)
  | _ => false
  };
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
    | Constructor("GroupedBarChart", _) =>
      parse_grouped(arg)
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
           Pie(
             combine_truncating(categories, values)
             |> List.filter_map(((label, value)) =>
                  Option.map(v => (label, v), value)
                ),
           );
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
