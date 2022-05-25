open Virtual_dom.Vdom;

let get_examples =
    (
      ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~level,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : list((Node.t, Node.t, string)) => {
  let examples =
    switch (explanation_info) {
    | ExpBinOperator(Plus, _l, _r) =>
      let level_1 = (
        UHExp.(
          Block.wrap'(
            Seq.mk(intlit("1"), [(Operators_Exp.Plus, intlit("2"))])
            |> mk_OpSeq,
          )
        ),
        DHExp.IntLit(3),
        "Add 1 and 2 to get 3",
      );
      let level_2 = (
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
      );
      switch (level) {
      | 1 => Some([level_1])
      | _ => Some([level_1, level_2])
      };
    | ExpBinOperator(Times, _l, _r) =>
      Some([
        (
          UHExp.(
            Block.wrap'(
              Seq.mk(intlit("2"), [(Operators_Exp.Times, intlit("3"))])
              |> mk_OpSeq,
            )
          ),
          DHExp.IntLit(6),
          "Multiply 2 and 3 to get 6",
        ),
      ])
    | ExpBaseOperand(IntLit(_)) =>
      Some([
        (
          UHExp.(Block.wrap(intlit("7"))),
          DHExp.IntLit(7),
          "Integer literals can be any integer within bounds",
        ),
      ])
    | _ => None
    };
  switch (examples) {
  | Some(examples) =>
    List.map(
      ((exp, result, caption)) =>
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
        ),
      examples,
    )
  | None => [(Node.text("Not supported"), Node.text("-"), "-")]
  };
};

let view =
    (
      ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~level,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  let examples =
    get_examples(~inject, ~settings, ~font_metrics, ~level, explanation_info);
  let explanation_views =
    List.map(
      ((example_body, result, caption)) => {
        Node.div(
          [Attr.classes(["the-explanation"])],
          [
            Node.div(
              [
                Attr.name("question_wrapper"),
                Attr.class_("question_wrapper"),
              ],
              [
                example_body,
                Node.div(
                  [
                    Attr.classes([
                      "example_result",
                      "example_explanation_demo",
                    ]),
                  ],
                  [Node.div([], [Node.text("Result: ")]), result],
                ),
                Node.div(
                  [
                    Attr.classes([
                      "example_explanation",
                      "example_explanation_demo",
                    ]),
                  ],
                  [Node.text("Explanation: "), Node.text(caption)],
                ),
              ],
            ),
          ],
        )
      },
      examples,
    );

  // TODO implement this top level function to generate and display examples

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Example"),
      Node.div([Attr.classes(["panel-body"])], explanation_views),
    ],
  );
};
