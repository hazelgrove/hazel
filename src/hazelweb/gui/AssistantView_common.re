open Virtual_dom.Vdom;

let article = (tag: TermSort.t): string =>
  switch (tag) {
  | Exp => "an"
  | Pat
  | Typ => "a"
  };

let shortcut_node = text =>
  Node.div([Attr.classes(["code-font", "shortcut"])], [Node.text(text)]);
