module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg;

/* Create an icon from a set of points with coords between 0 and 100.
 * The actual size of the icon can be styled using a CSS class.
 */
let icon = (classes, points) =>
  Js_of_ocaml_tyxml.Tyxml_js.Html.svg(
    ~a=[Svg.a_class(classes), Svg.a_viewBox((0.0, 0.0, 100.0, 100.0))],
    [Svg.polygon(~a=[Svg.a_points(points)], [])],
  );

/* Creating elements is side-effectful, so the extra
 * parameter is to allow delaying their construction.
 */
let left_arrow = (classes: list(string), _: unit) =>
  icon(classes, [(0.0, 50.0), (100.0, 0.0), (100.0, 100.0)]);
let right_arrow = (classes: list(string), _: unit) =>
  icon(classes, [(0.0, 0.0), (100.0, 50.0), (0.0, 100.0)]);
