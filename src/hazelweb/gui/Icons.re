open Incr_dom;

let arrow = (classes, string) =>
  Vdom.Node.div([Vdom.Attr.classes(classes)], [Vdom.Node.text(string)]);

let left_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, UnicodeConstants.left_triangle);
let right_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, UnicodeConstants.right_triangle);

let up_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, UnicodeConstants.up_triangle);
let down_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, UnicodeConstants.down_triangle);
let undo = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, UnicodeConstants.undo);

let question_mark_circle: Vdom.Node.t =
  Vdom.(
    Node.a(
      [Attr.classes(["question-mark-circle"]), Attr.href("#")],
      [Node.text("?")],
    )
  );
