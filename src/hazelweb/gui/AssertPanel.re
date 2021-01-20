module Vdom = Virtual_dom.Vdom;

exception InvalidInstance;
let view = (program: Program.t): Vdom.Node.t => {
  open Vdom;

  /**
   * Shows typing info for a context entry.
   */
  let static_info = assert_num =>
    Node.div(
      [Attr.classes(["static-info"])],
      [
        Node.div(
          [Attr.classes(["code"])],
          [Node.span([Attr.classes(["var"])], [Node.text(assert_num)])],
        ),
      ],
    );

  /**
   * Shows runtime value for a context entry.
   */
  let dynamic_info = assert_result =>
    Node.div(
      [Attr.classes(["dynamic-info"])],
      [Node.div([], [Node.span([], [Node.text(assert_result)])])],
    );

  let context_entry = (assert_map, assert_number) => {
    let static_info = static_info(assert_number);
    let children =
      switch (AssertMap.lookup(int_of_string(assert_number), assert_map)) {
      | Some(a) =>
        switch (AssertMap.check(a)) {
        | Comp => [static_info, dynamic_info("Comp")]
        | _ => [static_info]
        }
      | None => [static_info]
      };
    Node.div([Attr.classes(["context-entry"])], children);
  };
  let context_view = {
    let assert_map = snd(program |> Program.get_result);
    switch (AssertMap.to_list(assert_map)) {
    | [] =>
      Node.div(
        [Attr.classes(["the-context"])],
        [
          Node.div(
            [Attr.classes(["context-is-empty-msg"])],
            [Node.text("no assertion in scope")],
          ),
        ],
      )
    | ast_lst =>
      Node.div(
        [Attr.classes(["the-context"])],
        List.map(context_entry(assert_map), ast_lst),
      )
    };
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Assert"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [context_view],
      ),
    ],
  );
};
