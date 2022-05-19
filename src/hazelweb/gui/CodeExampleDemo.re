open Virtual_dom.Vdom;

let get_example =
    (
      ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~level,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : (Node.t, Node.t, string) => {
  switch (explanation_info) {
  | ExpBinOperator(Plus, _l, _r) =>
    let (exp, result, caption) =
      switch (level) {
      | 1 => (
          UHExp.(
            Block.wrap'(
              Seq.mk(intlit("1"), [(Operators_Exp.Plus, intlit("2"))])
              |> mk_OpSeq,
            )
          ),
          DHExp.IntLit(3),
          "Add 1 and 2 to get 3",
        )
      | _ => (
          UHExp.(
            Block.wrap'(
              Seq.mk(
                intlit("2"),
                [
                  (Operators_Exp.Plus, intlit("3")),
                  (Operators_Exp.Times, intlit("4")),
                ],
              )
              |> mk_OpSeq,
            )
          ),
          DHExp.IntLit(14),
          "Multiply 3 and 4 to get 12, then add 2 to get 14",
        )
      };
    (
      UHCode.basic_view(~settings, ~width=30, exp),
      DHCode.view(
        ~inject,
        ~settings=settings.evaluation,
        ~selected_instance=None,
        ~font_metrics,
        ~width=100,
        ~pos=0,
        result,
      ),
      caption,
    );
  | ExpBinOperator(Times, _l, _r) =>
    let exp =
      UHExp.(
        Block.wrap'(
          Seq.mk(intlit("2"), [(Operators_Exp.Times, intlit("3"))])
          |> mk_OpSeq,
        )
      );
    (
      UHCode.basic_view(~settings, ~width=30, exp),
      DHCode.view(
        ~inject,
        ~settings=settings.evaluation,
        ~selected_instance=None,
        ~font_metrics,
        ~width=100,
        ~pos=0,
        DHExp.IntLit(6),
      ),
      "Multiply 2 and 3 to get 6",
    );
  | ExpBaseOperand(IntLit(_)) => (
      UHCode.basic_view(
        ~settings,
        ~width=30,
        UHExp.(Block.wrap(intlit("7"))),
      ),
      DHCode.view(
        ~inject,
        ~settings=settings.evaluation,
        ~selected_instance=None,
        ~font_metrics,
        ~width=100,
        ~pos=0,
        DHExp.IntLit(7),
      ),
      "Integer literals can be any integer within bounds",
    )
  | _ => (Node.text("Not supported"), Node.text("-"), "-")
  };
};

let view =
    (
      ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~level=0,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  let (example_body, result, caption) =
    get_example(~inject, ~settings, ~font_metrics, ~level, explanation_info);
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [
        Node.div(
          [Attr.name("question_wrapper"), Attr.class_("question_wrapper")],
          [
            example_body,
            Node.div(
              [Attr.class_("example_result")],
              [Node.div([], [Node.text("Result: ")]), result],
            ),
            Node.div(
              [Attr.class_("example_explanation")],
              [Node.text("Explanation: "), Node.text(caption)],
            ),
          ],
        ),
      ],
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
