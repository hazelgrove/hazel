open Virtual_dom.Vdom;

let syntactic_form_view =
    (
      ~settings: Settings.t,
      ~level=0,
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
            Seq.mk(var("<exp1>"), [(Operators_Exp.Plus, var("<exp2>"))])
            |> mk_OpSeq,
          )
        )
      | _ =>
        UHExp.(
          Block.wrap'(
            Seq.mk(
              var("<lit1>"),
              [
                (Operators_Exp.Plus, var("<lit2>")),
                (Operators_Exp.Times, var("<lit3>")),
              ],
            )
            |> mk_OpSeq,
          )
        )
      };
    Node.span(
      [Attr.classes(["code"])],
      [UHCode.basic_view(~settings, ~width=30, exp)],
    );
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
      [Attr.classes(["the-explanation", "syntactic_form"])],
      [
        Node.div(
          [],
          [
            syntactic_form_view(
              ~settings: Settings.t,
              ~level,
              explanation_info,
            ),
          ],
        ),
      ],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Syntactic Form"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [explanation_view],
      ),
    ],
  );
};
