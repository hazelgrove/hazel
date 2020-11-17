module Vdom = Virtual_dom.Vdom;

let view_of_main_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar", "noselect"])],
      [Node.text(title_text)],
    )
  );

let view_of_other_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(
      [Attr.classes(["title-bar", "noselect"])],
      [Node.text(title_text)],
    )
  );
