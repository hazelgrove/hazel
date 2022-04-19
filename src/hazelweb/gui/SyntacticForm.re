open Virtual_dom.Vdom;

let syntactic_form_view =
    (
      ~settings: Settings.t,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  switch (explanation_info) {
  | LetLine(CommaOperator(_pats, _type), _def, _start_index, _body) =>
    let exp =
      UHExp.[
        letline(
          UHPat.(
            OpSeq.wrap(
              Parenthesized(
                Seq.mk(
                  var("<pat1>"),
                  [
                    (Operators_Pat.Comma, var("<pat2>")),
                    (Operators_Pat.Comma, var("...")),
                  ],
                )
                |> mk_OpSeq,
              ),
            )
          ),
          Block.wrap(var("<def>")),
        ),
        ExpLine(OpSeq.wrap(var("<body>"))),
      ];
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
      explanation_info: ExplanationInfo.explanation_info,
    )
    : Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation", "syntactic_form"])],
      [
        Node.div(
          [],
          [syntactic_form_view(~settings: Settings.t, explanation_info)],
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
