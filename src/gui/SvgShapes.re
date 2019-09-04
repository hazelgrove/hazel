open Incr_dom;

/* Create an icon from a set of points with coords between 0 and 100.
 * The actual size of the icon can be styled using a CSS class.
 */
let icon = (classes, _points) =>
  Vdom.Node.div([Vdom.Attr.classes(classes)], []);
/* TODO dunno how to get svg stuff working with incr_dom
   Tyxml.(
     Svg.svg(
       ~a=[Svg.a_class(classes), Svg.a_viewBox((0.0, 0.0, 100.0, 100.0))],
       [Svg.polygon(~a=[Svg.a_points(points)], [])],
     )
   );
   */

/* Creating elements is side-effectful, so the extra
 * parameter is to allow delaying their construction.
 */
let left_arrow = (classes: list(string), _: unit) =>
  icon(classes, [(0.0, 50.0), (100.0, 0.0), (100.0, 100.0)]);
let right_arrow = (classes: list(string), _: unit) =>
  icon(classes, [(0.0, 0.0), (100.0, 50.0), (0.0, 100.0)]);
