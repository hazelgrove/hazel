module Vdom = Virtual_dom.Vdom;

type t =
  | Exp
  | Pat
  | Var
  | Typ;

let get_cursor_term_sort = (cursor_term: CursorInfo.cursor_term): t => {
  switch (cursor_term) {
  | ExpOperand(_, _)
  | ExpOperator(_, _)
  | Line(_, _)
  | Rule(_, _) => Exp
  | PatOperand(_, _, Refutable)
  | PatOperator(_, _, Refutable) => Pat
  | PatOperand(_, _, Irrefutable)
  | PatOperator(_, _, Irrefutable) => Var
  | TypOperand(_, _)
  | TypOperator(_, _) => Typ
  };
};

let term_tag_view =
    (tag: t, ~show_tooltip: bool=false, add_classes: list(string))
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
  | Var => mk_view("var", "Variable(s)", "VAR(s)")
  | Typ => mk_view("typ", "Type", "TYP")
  };
};
