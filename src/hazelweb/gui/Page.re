open Hazellib;
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

let cardstacks_select =
    (~inject: Update.Action.t => Vdom.Event.t, cardstacks: list(CardStack.t)) => {
  let cardstack_options =
    List.mapi(
      (i, cardstack: CardStack.t) => {
        let example_idx = string_of_int(i);
        Vdom.(
          Node.option(
            [Attr.value(example_idx)],
            [Node.text(cardstack.title)],
          )
        );
      },
      cardstacks,
    );
  Vdom.(
    Node.select(
      [
        Attr.on_change((_, example_idx) =>
          inject(Update.Action.LoadCardStack(int_of_string(example_idx)))
        ),
      ],
      cardstack_options,
    )
  );
};

let prev_card_button = (~inject, model: Model.t) => {
  let cardstack = Model.cardstack_state_of(model);
  let show_prev =
    ZList.prefix_length(cardstack.zcards) > 0 ? [] : [Vdom.Attr.disabled];
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
};

let next_card_button = (~inject, model: Model.t) => {
  let cardstack = Model.cardstack_state_of(model);
  let show_next =
    ZList.suffix_length(cardstack.zcards) > 0 ? [] : [Vdom.Attr.disabled];
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
};

let cardstack_controls = (~inject, model: Model.t) =>
  Vdom.(
    Node.div(
      [Attr.id("cardstack-controls")],
      [
        Node.div(
          [Attr.id("button-centering-container")],
          [
            prev_card_button(~inject, model),
            next_card_button(~inject, model),
          ],
        ),
      ],
    )
  );

let page_view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let card = Model.card_of(model);
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
            cardstacks_select(~inject, model.cardstacks),
          ],
        ),
        Node.div(
          [Attr.classes(["main-area"])],
          [
            /*
             Sidebar.left(
               ~inject,
               false,
               [ActionPanel.view(~inject, model)] //the_history_panel,
             ),
             */
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
                        cardstack_controls(~inject, model),
                      ],
                    ),
                    examples_select(~inject),
                    /*
                     Node.button(
                       [
                         Attr.on_click(_ => {
                           let block =
                             model |> Model.zblock |> ZExp.erase_block;
                           JSUtil.log(
                             Js.string(
                               Sexplib.Sexp.to_string(
                                 UHExp.sexp_of_block(block),
                               ),
                             ),
                           );
                           Event.Ignore;
                         }),
                       ],
                       [Node.text("Serialize to console")],
                     ),
                     */
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
