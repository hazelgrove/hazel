type tag_typ =
  | Exp
  | Rule
  | Pat
  | Typ;

let get_cursor_term_tag_typ: CursorInfo_common.cursor_term => tag_typ;

let term_tag_view:
  (tag_typ, ~show_tooltip: bool=?, list(string)) => Virtual_dom.Vdom.Node.t;
