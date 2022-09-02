module Vdom = Virtual_dom.Vdom;

let view_of_main_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(
      ~attr=Attr.classes(["title-bar", "panel-title-bar"]),
      [Node.text(title_text)],
    )
  );

let view_of_other_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(~attr=Attr.classes(["title-bar"]), [Node.text(title_text)])
  );
