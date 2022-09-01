open Incr_dom;

let arrow = (classes, string) =>
  Vdom.Node.div(
    ~attr=Vdom.Attr.classes(classes),
    [Vdom.Node.text(string)],
  );

let left_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, Unicode.left_triangle);
let right_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, Unicode.right_triangle);

let down_arrow = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, Unicode.down_triangle);
let undo = (classes: list(string)): Vdom.Node.t =>
  arrow(classes, Unicode.undo);

let question_mark_circle: Vdom.Node.t =
  Vdom.(
    Node.a(
      ~attr=
        Attr.many([Attr.classes(["question-mark-circle"]), Attr.href("#")]),
      [Node.text("?")],
    )
  );

let left_side_bar_icon_opened =
  Vdom.Node.div([
    left_arrow(["left-sidebar-tab-icon-opened"]),
    question_mark_circle,
  ]);
