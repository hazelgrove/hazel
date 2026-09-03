open Language;

[@deriving sexp]
type point = {
  x: float,
  y: float,
};

/* `values` is positional against a chart's `categories`; None means the
   series has no bar at that category (only grouped charts produce None). */
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

/* Is this an application of a `Chart` ADT constructor, judged by the type
 * statics puts on the constructor? A cheap gate for auto-projection: true
 * here means parse_chart is worth attempting. */
let is_chart_ctr_ap: Exp.t => bool;

/* Recognize an elaborated expression of the built-in `Chart` ADT. Returns
 * None when the expression is not a (well-formed) chart constructor. */
let parse_chart: Exp.t => option(chart_spec);
