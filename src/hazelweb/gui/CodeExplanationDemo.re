open Virtual_dom.Vdom;

let get_msg =
    (~level, explanation_info: ExplanationInfo.explanation_info): string => {
  switch (explanation_info) {
  | ExpBinOperator(Plus, _l, _r) =>
    switch (level) {
    | 1 => "Integer addition of the [left operand](0) to the [right operand](1)"
    | 2 => "Integer addition of the [left operand](0) to the right operand. The right operand is the result of integer multiplication of its [left operand](1 0) with its [right operand](1 1)."
    | _ => "Integer addition of the [left integer](0) to the right operand. The right operand is the result of integer multiplication of its [left integer](1 0) with its [right integer](1 1)."
    }
  | ExpBinOperator(Times, _, _) =>
    switch (level) {
    | 1 => "Integer multiplication of the [left operand](0) to the [right operand](1)"
    | _ => "Integer multiplicaiton of the [left integer](0) to the [right integer](1)"
    }
  | ExpBaseOperand(IntLit(_, _i)) => "Integer literal"
  | _ => "Not supported"
  };
};

let get_mapping =
    (
      ~settings: DocumentationStudySettings.t,
      explanation_info: ExplanationInfo.explanation_info,
    )
    : ColorSteps.t =>
  if (settings.is_demo) {
    let (_, mapping) =
      CodeExplanation_common.build_msg(
        get_msg(~level=settings.example_level, explanation_info),
        true,
      );
    mapping;
  } else {
    ColorSteps.empty;
  };

let view =
    (~level, explanation_info: ExplanationInfo.explanation_info): Node.t => {
  let (msg, _) =
    CodeExplanation_common.build_msg(
      get_msg(~level, explanation_info),
      true,
    );
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation", "the-explanation-demo"])],
      [Node.div([], msg)],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Explanation"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [explanation_view],
      ),
    ],
  );
};
