open Virtual_dom.Vdom;

let article = (tag: TermTag.t): string =>
  switch (tag) {
  | Exp => "an"
  | Pat
  | Var
  | Typ => "a"
  };

let text_shortcut_node = text =>
  Node.div([Attr.classes(["code-font", "shortcut"])], [Node.text(text)]);

let kc_shortcut_node = key_combo =>
  text_shortcut_node(KeyCombo.name(HazelKeyCombos.get_details(key_combo)));
