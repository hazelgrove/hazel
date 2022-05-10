open Virtual_dom.Vdom;

let get_msg =
    (~level, explanation_info: ExplanationInfo.explanation_info): string => {
  switch (explanation_info) {
  | ExpBinOperator(Plus, _l, _r) =>
    switch (level) {
    | 0 => "Integer addition of the left operand to the right operand"
    | _ => "Integer addition of the left operand to the right operand"
    }
  | _ => "Not supported"
  };
};

let view =
    (~level=0, explanation_info: ExplanationInfo.explanation_info): Node.t => {
  let (msg, _) =
    CodeExplanation_common.build_msg(
      get_msg(~level, explanation_info),
      false,
    );
  let explanation_view = {
    Node.div([Attr.classes(["the-explanation"])], [Node.div([], msg)]);
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
