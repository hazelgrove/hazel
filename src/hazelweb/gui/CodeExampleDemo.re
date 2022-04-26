open Virtual_dom.Vdom;

let get_example =
    (
      ~settings: Settings.t,
      ~level,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  switch (explanation_info) {
  | ExpBinOperator(Plus, _l, _r) =>
    let exp =
      switch (level) {
      | 0 =>
        UHExp.(
          Block.wrap'(
            Seq.mk(intlit("1"), [(Operators_Exp.Plus, intlit("2"))])
            |> mk_OpSeq,
          )
        )
      | _ =>
        UHExp.(
          Block.wrap'(
            Seq.mk(
              intlit("1"),
              [
                (Operators_Exp.Plus, intlit("2")),
                (Operators_Exp.Times, intlit("3")),
              ],
            )
            |> mk_OpSeq,
          )
        )
      };
    UHCode.basic_view(~settings, ~width=30, exp);

  | _ => Node.text("Not supported")
  };
};

let view =
    (
      ~settings: Settings.t,
      ~level=0,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [Node.div([], [get_example(~settings, ~level, explanation_info)])],
    );
  };

  // TODO implement this top level function to generate and display examples

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Example"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [explanation_view],
      ),
    ],
  );
};
