open Incr_dom;

/* Create an icon from a set of points with coords between 0 and 100.
 * The actual size of the icon can be styled using a CSS class.
 */
let icon = (classes, string) =>
  Vdom.Node.div([Vdom.Attr.classes(classes)], [Vdom.Node.text(string)]);

/* Creating elements is side-effectful, so the extra
 * parameter is to allow delaying their construction.
 */
let left_arrow = (classes: list(string), _: unit) => icon(classes, "◀");
let right_arrow = (classes: list(string), _: unit) => icon(classes, "▶");
