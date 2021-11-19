module Vdom = Virtual_dom.Vdom;

let get_cursor_term_sort = (cursor_term: CursorInfo.cursor_term): TermSort.t => {
  switch (cursor_term) {
  | ExpOperand(_, _)
  | ExpOperator(_, _)
  | Line(_, _)
  | Rule(_, _) => Exp
  | PatOperand(_, _)
  | PatOperator(_, _) => Pat
  | TypOperand(_, _)
  | TypOperator(_, _) => Typ
  | Tag(_) => Tag
  | SumBody(_)
  | SumBodyOp(_) => SumBody
  };
};

let term_tag_view =
    (tag: TermSort.t, ~show_tooltip: bool=false, add_classes: list(string))
    : Vdom.Node.t => {
  let mk_view = (class_extn, tooltip, text) => {
    let classes =
      Vdom.Attr.classes([
        "term-tag",
        "term-tag-" ++ class_extn,
        ...add_classes,
      ]);
    let attrs =
      show_tooltip
        ? [Vdom.Attr.create("title", tooltip), classes] : [classes];
    Vdom.(Node.div(attrs, [Node.text(text)]));
  };
  switch (tag) {
  | Exp => mk_view("exp", "Expression", "EXP")
  | Pat => mk_view("pat", "Pattern", "PAT")
  | Typ => mk_view("typ", "Type", "TYP")
  | Tag => mk_view("tag", "Tag", "TAG")
  | SumBody => mk_view("sum-body", "Sum", "SUM")
  };
};
