open Virtual_dom.Vdom;

let shortcut_node = text =>
  Node.div([Attr.classes(["code-font", "shortcut"])], [Node.text(text)]);
