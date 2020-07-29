module Vdom = Virtual_dom.Vdom;

let view_of_main_title_bar = (title_text: string) =>
  Vdom.(
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar"])],
      [Node.text(title_text)],
    )
  );

let view_of_other_title_bar = (title_text: string) =>
  Vdom.(Node.div([Attr.classes(["title-bar"])], [Node.text(title_text)]));

let view =
    (
      ~title_text: string,
      ~id: option(string),
      ~classes: List.t(string),
      ~body_contents: List.t(Vdom.Node.t),
    )
    : Vdom.Node.t => {
  switch (id) {
  | Some(id) =>
    Vdom.(
      Node.div(
        [Attr.id(id), Attr.classes(["panel"] @ classes)],
        [view_of_main_title_bar(title_text), ...body_contents],
      )
    )
  | None =>
    Vdom.(
      Node.div(
        [Attr.classes(["panel"] @ classes)],
        [view_of_main_title_bar(title_text), ...body_contents],
      )
    )
  };
};
