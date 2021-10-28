let get_cursor_term_sort: CursorInfo.cursor_term => TermSort.t;

let term_tag_view:
  (TermSort.t, ~show_tooltip: bool=?, list(string)) => Virtual_dom.Vdom.Node.t;
