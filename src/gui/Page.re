module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

let examples_select = (~inject: Update.Action.t => Vdom.Event.t) =>
  Vdom.(
    Node.select(
      [
        Attr.on_change((_, example_id) =>
          inject(Update.Action.LoadExample(example_id))
        ),
      ],
      [
        Node.option([Attr.value("just_hole")], [Node.text("just a hole")]),
        Node.option(
          [Attr.value("holey_lambda")],
          [Node.text("holey lambda")],
        ),
        Node.option(
          [Attr.value("let_line")],
          [Node.text("let with extra lines")],
        ),
        Node.option([Attr.value("map_example")], [Node.text("map")]),
        Node.option([Attr.value("qsort_example")], [Node.text("qsort")]),
      ],
    )
  );

let page_view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [Attr.id("root")],
      [
        Node.div(
          [Attr.classes(["top-bar"])],
          [
            Node.a(
              [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
              [Node.text("Hazel")],
            ),
          ],
        ),
        Node.div(
          [Attr.classes(["main-area"])],
          [
            Sidebar.left(
              ~inject,
              false,
              [MyActionPanel.view(~inject, model)] /*the_history_panel*/,
            ),
            Node.div(
              [Attr.classes(["flex-wrapper"])],
              [
                Node.div(
                  [Attr.classes(["page-area"])],
                  [
                    Node.div(
                      [Attr.classes(["page"])],
                      [
                        Node.div(
                          [],
                          [
                            Node.text("Hazel is an experiment in "),
                            Node.strong(
                              [],
                              [Node.text("live functional programming")],
                            ),
                            Node.text(" with "),
                            Node.strong([], [Node.text("typed holes")]),
                            Node.text(
                              ". Use the actions on the left to construct an expression. Navigate using the text cursor in the usual way.",
                            ),
                          ],
                        ),
                        Node.div(
                          [Attr.id("pp_view"), Attr.classes(["ModelExp"])],
                          [Cell.view(~inject, model)],
                        ),
                        Node.div(
                          [Attr.classes(["cell-status"])],
                          [
                            Node.div(
                              [Attr.classes(["type-indicator"])],
                              [
                                Node.div(
                                  [Attr.classes(["type-label"])],
                                  [Node.text("Result of type: ")],
                                ),
                                Node.div(
                                  [Attr.classes(["htype-view"])],
                                  [
                                    {
                                      let (_, ty, _) = model.edit_state;
                                      Code.view_of_htyp(~inject, ty);
                                    },
                                  ],
                                ),
                              ],
                            ),
                          ],
                        ),
                        Node.div(
                          [Attr.classes(["result-view"])],
                          [Code.view_of_result(~inject, model)],
                        ),
                      ],
                    ),
                    examples_select(~inject),
                  ],
                ),
              ],
            ),
            Sidebar.right(
              ~inject,
              true,
              [
                MyCursorInspector.view(~inject, model),
                MyContextInspector.view(~inject, model),
              ],
            ),
          ],
        ),
      ],
    )
  );
};

[@warning "-27"]
let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t =>
  page_view(~inject, model);
