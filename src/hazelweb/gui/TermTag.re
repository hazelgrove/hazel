module Vdom = Virtual_dom.Vdom;

type t =
  | Exp
  | Pat
  | Var
  | Typ;

let get_cursor_term_sort = (cursor_term: CursorInfo.cursor_term): t => {
  switch (cursor_term) {
  | Exp(_, _)
  | ExpOp(_, _)
  | Line(_, _)
  | Rule(_, _) => Exp
  | Pat(_, _, Refutable)
  | PatOp(_, _, Refutable) => Pat
  | Pat(_, _, Irrefutable)
  | PatOp(_, _, Irrefutable) => Var
  | Typ(_, _)
  | TypOp(_, _) => Typ
  };
};

let term_tag_view =
    (tag: t, ~show_tooltip: bool=false, add_classes: list(string))
    : Vdom.Node.t => {
  switch (tag) {
  | Exp =>
    let classes =
      Vdom.Attr.classes(["term-tag", "term-tag-exp", ...add_classes]);
    let attrs =
      show_tooltip
        ? [Vdom.Attr.create("title", "Expression"), classes] : [classes];
    Vdom.(Node.div(attrs, [Node.text("EXP")]));
  | Pat =>
    let classes =
      Vdom.Attr.classes(["term-tag", "term-tag-pat", ...add_classes]);
    let attrs =
      show_tooltip
        ? [Vdom.Attr.create("title", "Pattern"), classes] : [classes];
    Vdom.(Node.div(attrs, [Node.text("PAT")]));
  | Var =>
    let classes =
      Vdom.Attr.classes(["term-tag", "term-tag-pat", ...add_classes]);
    let attrs =
      show_tooltip
        ? [Vdom.Attr.create("title", "Variable(s)"), classes] : [classes];
    Vdom.(Node.div(attrs, [Node.text("VAR(s)")]));
  | Typ =>
    let classes =
      Vdom.Attr.classes(["term-tag", "term-tag-typ", ...add_classes]);
    let attrs =
      show_tooltip
        ? [Vdom.Attr.create("title", "Type"), classes] : [classes];
    Vdom.(Node.div(attrs, [Node.text("TYP")]));
  };
};
