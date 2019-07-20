module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;
module ZList = GeneralUtil.ZList;

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

let cardstack_buttons =
    (model: Model.t, ~inject: Update.Action.t => Vdom.Event.t) => {
  let cardstack = model.cardstack_state;
  let show_prev =
    ZList.prefix_length(cardstack) > 0 ? [] : [Vdom.Attr.disabled];
  let show_next =
    ZList.suffix_length(cardstack) > 0 ? [] : [Vdom.Attr.disabled];
  let prev_btn =
    Vdom.(
      Node.button(
        [
          Attr.id("cardstack-prev-button"),
          Attr.on_click(_ => inject(Update.Action.PrevCard)),
          ...show_prev,
        ],
        [Node.text("Previous")],
      )
    );
  let next_btn =
    Vdom.(
      Node.button(
        [
          Attr.id("cardstack-next-button"),
          Attr.on_click(_ => inject(Update.Action.NextCard)),
          ...show_next,
        ],
        [Node.text("Next")],
      )
    );
  Vdom.(Node.div([Attr.id("cardstack-buttons")], [prev_btn, next_btn]));
};

let page_view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let card = GeneralUtil.ZList.prj_z(model.cardstack_state).card;
  let cell_status =
    switch (model.result_state) {
    | ResultsDisabled => Vdom.Node.div([], [])
    | Result(_) =>
      Vdom.(
        Node.div(
          [],
          [
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
                          let (_, ty, _) = Model.edit_state_of(model);
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
        )
      )
    };
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
              [ActionPanel.view(~inject, model)] /*the_history_panel*/,
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
                          [Attr.classes(["card-caption"])],
                          [card.caption],
                          /* [
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
                             ], */
                        ),
                        Cell.view(~inject, model),
                        cell_status,
                      ],
                    ),
                    examples_select(~inject),
                    cardstack_buttons(model, ~inject),
                  ],
                ),
              ],
            ),
            Sidebar.right(
              ~inject,
              true,
              [
                CursorInspector.view(~inject, model),
                ContextInspector.view(~inject, model),
                OptionsPanel.view(~inject, model),
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
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t =>
  page_view(~inject, model);
