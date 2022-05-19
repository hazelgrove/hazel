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

let syntactic_max_level_ =
    (explanation_info: ExplanationInfo.explanation_info): int => {
  switch (explanation_info) {
  | LetLine(CommaOperator(_pats, _type), _def, _start_index, _body) => 5
  | _ => 2
  };
};

let generate_selector = (explanation_info, syntactic_form_level): Node.t => {
  Node.div(
    [Attr.classes(["slider-wrapper"])],
    [
      Node.input(
        [
          Attr.type_("range"),
          Attr.min(1.0),
          Attr.max(float_of_int(syntactic_max_level_(explanation_info))),
          Attr.value(string_of_int(syntactic_form_level)),
          Attr.disabled,
        ],
        List.init(syntactic_max_level_(explanation_info), index =>
          Node.option(
            [Attr.value(string_of_int(index))],
            [Node.text(string_of_float(float_of_int(index)))],
          )
        ),
      ),
      // [
      //   Node.option([Attr.value("1")], [Node.text("1.0")]),
      //   Node.option([Attr.value("2")], [Node.text("2.0")]),
      //   Node.option([Attr.value("3")], [Node.text("3.0")]),
      // ],
      Node.div(
        [],
        [
          Node.div([], [Node.text("less specific")]),
          Node.div([], [Node.text("more specific")]),
        ],
      ),
    ],
  );
};

let view =
    (
      ~settings: Settings.t,
      explanation_info: ExplanationInfo.explanation_info,
      syntactic_form_level: int,
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
          generate_selector(explanation_info, syntactic_form_level),
        ],
      ),
    ],
  );
};
