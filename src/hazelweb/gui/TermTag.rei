type t =
  | Exp
  | Pat
  | Var
  | Typ;

let get_cursor_term_sort: CursorInfo.cursor_term => t;

let term_tag_view:
  (t, ~show_tooltip: bool=?, list(string)) => Virtual_dom.Vdom.Node.t;
