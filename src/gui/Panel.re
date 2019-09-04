module Vdom = Virtual_dom.Vdom;

let view_of_main_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar"])],
      [Node.text(title_text)],
    )
  );

/* For title bars that appear mid-panel. These are styled differently. */
let view_of_other_title_bar = (title_text: string) =>
  Vdom.(Node.div([Attr.classes(["title-bar"])], [Node.text(title_text)]));
