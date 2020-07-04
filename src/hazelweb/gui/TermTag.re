module Vdom = Virtual_dom.Vdom;

type tag_typ =
  | Exp
  | Pat
  | Typ;

let get_cursor_term_tag_typ =
    (cursor_term: CursorInfo_common.cursor_term): tag_typ => {
  switch (cursor_term) {
  | Exp(_, _) => Exp
  | Pat(_, _) => Pat
  | Typ(_, _) => Typ
  | ExpOp(_, _) => Exp
  | PatOp(_, _) => Pat
  | TypOp(_, _) => Typ
  | Line(_, _)
  | Rule(_, _) => Exp
  };
};

let term_tag_view = (tag: tag_typ, add_classes: list(string)): Vdom.Node.t => {
  switch (tag) {
  | Exp =>
    Vdom.(
      Node.div(
        [Attr.classes(["term-tag", "term-tag-exp", ...add_classes])],
        [Node.text("EXP")],
      )
    )
  | Pat =>
    Vdom.(
      Node.div(
        [Attr.classes(["term-tag", "term-tag-pat", ...add_classes])],
        [Node.text("PAT")],
      )
    )
  | Typ =>
    Vdom.(
      Node.div(
        [Attr.classes(["term-tag", "term-tag-typ", ...add_classes])],
        [Node.text("TYP")],
      )
    )
  };
};
