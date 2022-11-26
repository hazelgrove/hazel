open Virtual_dom.Vdom;
/* // let article = (tag: TermSort.t): string =>
   //   switch (tag) {
   //   | Exp => "an"
   //   | Pat
   //   | Typ => "a"
   //   }; */

let text_shortcut_node = text =>
  Node.div(
    ~attr=Attr.classes(["code-font", "shortcut"]),
    [Node.text(text)],
  );

let kc_shortcut_node = (key_combo: KeyCombo.t) =>
  /* let sys = Os.is_mac^ ? Mac : PC; */
  switch (key_combo) {
  | Escape => text_shortcut_node("Esc")
  | Backspace => text_shortcut_node("Backspace")
  | Delete => text_shortcut_node("Del")
  | ShiftTab => text_shortcut_node("Shift + Tab")
  | Tab => text_shortcut_node("Tab")
  | GT => text_shortcut_node(">")
  | Ampersand => text_shortcut_node("&")
  | VBar => text_shortcut_node("|")
  | LeftParen => text_shortcut_node("(")
  | RightParen => text_shortcut_node(")")
  | RightBrace => text_shortcut_node("}")
  | RightSquareBracket => text_shortcut_node("]")
  | Colon => text_shortcut_node(":")
  | Equals => text_shortcut_node("=")
  | Enter => text_shortcut_node("Enter")
  | Shift_Enter => text_shortcut_node("Shift + Enter")
  | Backslash => text_shortcut_node("/")
  | Plus => text_shortcut_node("+")
  | Minus => text_shortcut_node("-")
  | Asterisk => text_shortcut_node("*")
  | Slash => text_shortcut_node("/")
  | LT => text_shortcut_node("<")
  | Space => text_shortcut_node("Space")
  | Comma => text_shortcut_node(",")
  | LeftBracket => text_shortcut_node("{")
  | Semicolon => text_shortcut_node(";")
  | Alt_L => text_shortcut_node("Alt + L")
  | Alt_R => text_shortcut_node("Alt + R")
  | Alt_C => text_shortcut_node("Alt + C")
  | Pound => text_shortcut_node("#")
  | Ctrl_Space => text_shortcut_node("Ctrl + Space")
  | Ctrl_S => text_shortcut_node("Ctrl + S")
  | Ctrl_Shift_S => text_shortcut_node("Ctrl + Shift + S")
  | Ctrl_Shift_L => text_shortcut_node("Ctrl + Shift + L")
  | CtrlOrCmd_Z => text_shortcut_node("Ctrl/Cmd + Z")
  | CtrlOrCmd_Shift_Z => text_shortcut_node("Ctrl/Cmd + Shift + Z")
  | Up => text_shortcut_node("↑")
  | Down => text_shortcut_node("↓")
  | Left => text_shortcut_node("←")
  | Right => text_shortcut_node("→")
  | Home => text_shortcut_node("Home")
  | End => text_shortcut_node("End")
  | Alt_Up => text_shortcut_node("Alt + ↑")
  | Alt_Down => text_shortcut_node("Alt + ↓")
  | Alt_Left => text_shortcut_node("ALt + ←")
  | Alt_Right => text_shortcut_node("ALt + →")
  };
