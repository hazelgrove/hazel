module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;

exception InvalidInstance;
/* let history_entry = (history: UndoHistory.t) => {
    let (_, option(Action.t), id) = history;
    switch
     let static_info = static_info((x, ty));
     let children =
       switch (dynamic_info(sigma, x)) {
       | Some(dynamic_info) => [static_info, dynamic_info]
       | None => [static_info]
       };
     Vdom.(Node.div([Attr.classes(["history-entry"])], children));
   }; */

let view = () => {
  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("history"),
        Node.div(
          [Attr.classes(["panel-body", "context-inspector-body"])],
          [Node.div([], [Node.text("ahhhh")])],
        ),
      ],
    )
  );
};
