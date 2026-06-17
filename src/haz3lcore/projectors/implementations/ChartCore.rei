open Language;

type point = {
  x: float,
  y: float,
};

type series = {
  name: string,
  values: list(float),
};

type chart_spec =
  | Bar({
      categories: list(string),
      series: list(series),
    })
  | Pie(list((string, float)))
  | Line(list(point))
  | Scatter(list(point));

/* Recognize an elaborated expression of the built-in `Chart` ADT. Returns
 * None when the expression is not a (well-formed) chart constructor. */
let parse_chart: Exp.t => option(chart_spec);
