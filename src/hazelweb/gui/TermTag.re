module Vdom = Virtual_dom.Vdom;

let get_cursor_term_sort = (cursor_term: CursorInfo.cursor_term): TermSort.t => {
  switch (cursor_term) {
  | ExpOperand(_, _)
  | ExpOperator(_, _, _, _)
  | Line(_, _, _)
  | Rule(_, _, _, _) => Exp
  | PatOperand(_, _)
  | PatOperator(_, _, _, _) => Pat
  | TypOperand(_, _)
  | TypOperator(_, _, _, _) => Typ
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
  };
};
