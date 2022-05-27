open Virtual_dom.Vdom;

let syntactic_form_view =
    (
      ~settings: Settings.t,
      explanation_info: ExplanationInfo.explanation_info,
      syntactic_form_level: int,
    )
    : Node.t => {
  let rec is_op = (op: UHExp.operator, OpSeq(skel, seq): UHExp.opseq) => {
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      switch (pn) {
      | Parenthesized([ExpLine(opseq)]) => is_op(op, opseq)
      | _ => false
      };
    | BinOp(_, _, o, _, _) => op == o
    };
  };
  let form =
    switch (explanation_info) {
    | LetLine(CommaOperator(_pats, _type), _def, _start_index, _body) =>
      switch (syntactic_form_level) {
      | 4 =>
        Some(
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
          ],
        )
      | 2 =>
        Some(
          UHExp.[
            letline(
              UHPat.(OpSeq.wrap(var("<pat>"))),
              Block.wrap(var("<def>")),
            ),
            ExpLine(OpSeq.wrap(var("<body>"))),
          ],
        )
      | _ => None
      }
    | Lambda(CommaOperator(_pats, _type), _body) =>
      switch (syntactic_form_level) {
      | 4 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.wrap(
                lam(
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
                  Block.wrap(var("<body>")),
                ),
              )
              |> mk_OpSeq,
            ),
          ],
        )
      | 2 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.wrap(
                lam(
                  UHPat.(OpSeq.wrap(var("<pat>"))),
                  Block.wrap(var("<body>")),
                ),
              )
              |> mk_OpSeq,
            ),
          ],
        )
      | _ => None
      }
    | ExpBaseOperand(Case(_)) =>
      Some(
        UHExp.[
          ExpLine(
            Seq.wrap(
              case(
                Block.wrap(UHExp.var("<scrutinee>")),
                [
                  Rule(
                    UHPat.(OpSeq.wrap(var("<pat1>"))),
                    Block.wrap(var("<clause1>")),
                  ),
                  Rule(
                    UHPat.(OpSeq.wrap(var("<pat2>"))),
                    Block.wrap(var("<clause2>")),
                  ),
                  Rule(
                    UHPat.(OpSeq.wrap(var("..."))),
                    Block.wrap(var("...")),
                  ),
                ],
              ),
            )
            |> mk_OpSeq,
          ),
        ],
      )
    | ExpBinOperator(Operators_Exp.Space, _, right)
        when is_op(Operators_Exp.Comma, right) =>
      switch (syntactic_form_level) {
      | 1 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.mk(var("<fn>"), [(Operators_Exp.Space, var("<arg>"))])
              |> UHExp.mk_OpSeq,
            ),
          ],
        )
      | 2 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.mk(
                var("<fn>"),
                [
                  (
                    Operators_Exp.Space,
                    Parenthesized(
                      Block.wrap'(
                        Seq.mk(
                          var("<arg1>"),
                          [
                            (Operators_Exp.Comma, var("<arg2>")),
                            (Operators_Exp.Comma, var("...")),
                          ],
                        )
                        |> mk_OpSeq,
                      ),
                    ),
                  ),
                ],
              )
              |> UHExp.mk_OpSeq,
            ),
          ],
        )
      | _ => None
      }
    | ExpBinOperator(Operators_Exp.Space, left, _)
        when is_op(Operators_Exp.Space, left) =>
      switch (syntactic_form_level) {
      | 1 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.mk(var("<fn>"), [(Operators_Exp.Space, var("<arg>"))])
              |> UHExp.mk_OpSeq,
            ),
          ],
        )
      | 2 =>
        Some(
          UHExp.[
            ExpLine(
              Seq.mk(
                var("<fn>"),
                [
                  (Operators_Exp.Space, var("<arg1>")),
                  (Operators_Exp.Space, var("<arg2>")),
                  (Operators_Exp.Space, var("...")),
                ],
              )
              |> UHExp.mk_OpSeq,
            ),
          ],
        )
      | _ => None
      }
    | _ => None
    };
  switch (form) {
  | Some(exp) =>
    Node.span(
      [Attr.classes(["code"])],
      [UHCode.basic_view(~settings, ~width=30, exp)],
    )
  | None => Node.text("Not supported")
  };
};

let syntactic_max_level_ =
    (explanation_info: ExplanationInfo.explanation_info): int => {
  switch (explanation_info) {
  | LetLine(CommaOperator(_pats, _type), _def, _start_index, _body) => 5
  | Lambda(CommaOperator(_pats, _type), _def) => 5
  | ExpBaseOperand(Case(_)) => 2
  | ExpBinOperator(Operators_Exp.Space, _, _) => 3
  | _ => 2
  };
};

let generate_selector = (explanation_info, syntactic_form_level): Node.t =>
  if (syntactic_max_level_(explanation_info) >= 2) {
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
        Node.div(
          [],
          [
            Node.div([], [Node.text("less specific")]),
            Node.div([], [Node.text("more specific")]),
          ],
        ),
      ],
    );
  } else {
    Node.text("");
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
          [
            syntactic_form_view(
              ~settings: Settings.t,
              explanation_info,
              syntactic_form_level,
            ),
          ],
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
