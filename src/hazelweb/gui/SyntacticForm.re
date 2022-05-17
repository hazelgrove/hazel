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
    [
      Attr.classes([
        "panel",
        "context-inspector-panel",
        "syntactic-form-panel",
      ]),
    ],
    [
      Panel.view_of_main_title_bar("Syntactic Form"),
      Node.div(
        [
          Attr.classes([
            "panel-body",
            "context-inspector-body",
            "syntactic-form-panel-body",
          ]),
        ],
        [
          explanation_view,
          Node.div(
            [Attr.classes(["slider-wrapper"])],
            [
              Node.input(
                [
                  Attr.type_("range"),
                  Attr.min(1.0),
                  Attr.max(3.0),
                  Attr.value(string_of_int(2)),
                  Attr.disabled,
                ],
                [
                  Node.option(
                    [Attr.value("1")],
                    [Node.label([], [Node.text("1.0")])],
                  ),
                  Node.option([Attr.value("2")], [Node.text("2.0")]),
                  Node.option([Attr.value("3")], [Node.text("3.0")]),
                ],
              ),
              Node.div(
                [],
                [
                  Node.div([], [Node.text("less specific")]),
                  Node.div([], [Node.text("default")]),
                  Node.div([], [Node.text("more specific")]),
                ],
              ),
            ],
          ),
        ],
      ),
    ],
  );
};
