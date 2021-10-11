open Sexplib.Std;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

[@deriving sexp]
type single_key =
  | Number(int)
  | Letter(string)
  | Underscore;

[@deriving sexp]
type t =
  | Single(single_key)
  | Escape
  | Backspace
  | Delete
  | ShiftTab
  | Tab
  | GT
  | Ampersand
  | VBar
  | LeftParen
  | Colon
  | Equals
  | Enter
  | Shift_Enter
  | Backslash
  | Plus
  | Minus
  | Asterisk
  | Slash
  | LT
  | Space
  | Comma
  | LeftBracket
  | Semicolon
  | Alt_L
  | Alt_R
  | Alt_C
  | Pound
  | Ctrl_Space
  | Ctrl_Delete
  | Ctrl_S
  | CtrlOrCmd_Z
  | CtrlOrCmd_Shift_Z
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Alt_Up
  | Alt_Down
  | Alt_Left
  | Alt_Right
  | Shift_Up
  | Shift_Down
  | Shift_Left
  | Shift_Right
  | Ctrl_Up
  | Ctrl_Down
  | Ctrl_Left
  | Ctrl_Right;

let letter_regexp = Js_of_ocaml.Regexp.regexp("^[a-zA-Z']$");

let is_single_key: Js.t(Dom_html.keyboardEvent) => option(single_key) =
  evt => {
    let ctrlKey = Js.to_bool(evt##.ctrlKey);
    let altKey = Js.to_bool(evt##.altKey);
    let metaKey = Js.to_bool(evt##.metaKey);
    if (ctrlKey || altKey || metaKey) {
      None;
    } else {
      let key = Key.get_key(evt);
      switch (int_of_string_opt(key)) {
      | Some(n) => Some(Number(n))
      | None =>
        switch (Js_of_ocaml.Regexp.string_match(letter_regexp, key, 0)) {
        | Some(_) => Some(Letter(key))
        | None =>
          /* could be later refactored to a separate regex */
          switch (key) {
          | "_" => Some(Underscore)
          | "." => Some(Letter(key))
          | _ => None
          }
        }
      };
    };
  };

let string_of_single_key: single_key => string =
  single_key =>
    switch (single_key) {
    | Number(n) => string_of_int(n)
    | Letter(x) => x
    | Underscore => "_"
    };

let get_details =
  fun
  | Single(k) => k |> string_of_single_key |> KeyCombo.single
  | Pound => KeyCombo.pound
  | Escape => KeyCombo.escape
  | Backspace => KeyCombo.backspace
  | Delete => KeyCombo.delete
  | ShiftTab => KeyCombo.shift_tab
  | Tab => KeyCombo.tab
  | GT => KeyCombo.gt
  | Ampersand => KeyCombo.ampersand
  | VBar => KeyCombo.vbar
  | LeftParen => KeyCombo.left_parens
  | Colon => KeyCombo.colon
  | Equals => KeyCombo.equals
  | Enter => KeyCombo.enter
  | Shift_Enter => KeyCombo.shift_enter
  | Backslash => KeyCombo.backslash
  | Plus => KeyCombo.plus
  | Minus => KeyCombo.minus
  | Asterisk => KeyCombo.asterisk
  | Slash => KeyCombo.slash
  | LT => KeyCombo.lt
  | Space => KeyCombo.space
  | Comma => KeyCombo.comma
  | LeftBracket => KeyCombo.left_bracket
  | Semicolon => KeyCombo.semicolon
  | Alt_L => KeyCombo.alt_L
  | Alt_R => KeyCombo.alt_R
  | Alt_C => KeyCombo.alt_C
  | Ctrl_Space => KeyCombo.ctrl_space
  | Ctrl_Delete => KeyCombo.ctrl_backspace
  | Ctrl_S => KeyCombo.ctrl_s
  | CtrlOrCmd_Z => KeyCombo.ctrlOrCmd_z
  | CtrlOrCmd_Shift_Z => KeyCombo.ctrlOrCmd_shift_z
  | Up => KeyCombo.up
  | Down => KeyCombo.down
  | Left => KeyCombo.left
  | Right => KeyCombo.right
  | Home => KeyCombo.home_key
  | End => KeyCombo.end_key
  | Alt_Up => KeyCombo.alt_up
  | Alt_Down => KeyCombo.alt_down
  | Alt_Left => KeyCombo.alt_left
  | Alt_Right => KeyCombo.alt_right
  | Shift_Up => KeyCombo.shift_up
  | Shift_Down => KeyCombo.shift_down
  | Shift_Left => KeyCombo.shift_left
  | Shift_Right => KeyCombo.shift_right
  | Ctrl_Up => KeyCombo.ctrl_up
  | Ctrl_Down => KeyCombo.ctrl_down
  | Ctrl_Left => KeyCombo.ctrl_left
  | Ctrl_Right => KeyCombo.ctrl_right;

let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
  let evt_matches = details => KeyCombo.matches(details, evt);
  let single_key = is_single_key(evt);
  if (evt_matches(KeyCombo.pound)) {
    Some(Pound);
  } else if (evt_matches(KeyCombo.escape)) {
    Some(Escape);
  } else if (evt_matches(KeyCombo.backspace)) {
    Some(Backspace);
  } else if (evt_matches(KeyCombo.delete)) {
    Some(Delete);
  } else if (evt_matches(KeyCombo.shift_tab)) {
    Some(ShiftTab);
  } else if (evt_matches(KeyCombo.tab)) {
    Some(Tab);
  } else if (evt_matches(KeyCombo.gt)) {
    Some(GT);
  } else if (evt_matches(KeyCombo.ampersand)) {
    Some(Ampersand);
  } else if (evt_matches(KeyCombo.vbar)) {
    Some(VBar);
  } else if (evt_matches(KeyCombo.left_parens)) {
    Some(LeftParen);
  } else if (evt_matches(KeyCombo.colon)) {
    Some(Colon);
  } else if (evt_matches(KeyCombo.equals)) {
    Some(Equals);
  } else if (evt_matches(KeyCombo.enter)) {
    Some(Enter);
  } else if (evt_matches(KeyCombo.shift_enter)) {
    Some(Shift_Enter);
  } else if (evt_matches(KeyCombo.backslash)) {
    Some(Backslash);
  } else if (evt_matches(KeyCombo.plus)) {
    Some(Plus);
  } else if (evt_matches(KeyCombo.minus)) {
    Some(Minus);
  } else if (evt_matches(KeyCombo.asterisk)) {
    Some(Asterisk);
  } else if (evt_matches(KeyCombo.slash)) {
    Some(Slash);
  } else if (evt_matches(KeyCombo.lt)) {
    Some(LT);
  } else if (evt_matches(KeyCombo.space)) {
    Some(Space);
  } else if (evt_matches(KeyCombo.comma)) {
    Some(Comma);
  } else if (evt_matches(KeyCombo.left_bracket)) {
    Some(LeftBracket);
  } else if (evt_matches(KeyCombo.semicolon)) {
    Some(Semicolon);
  } else if (evt_matches(KeyCombo.alt_L)) {
    Some(Alt_L);
  } else if (evt_matches(KeyCombo.alt_R)) {
    Some(Alt_R);
  } else if (evt_matches(KeyCombo.alt_C)) {
    Some(Alt_C);
  } else if (evt_matches(KeyCombo.ctrl_space)) {
    Some(Ctrl_Space);
  } else if (evt_matches(KeyCombo.ctrl_backspace)) {
    Some(Ctrl_Delete);
  } else if (evt_matches(KeyCombo.ctrl_s)) {
    Some(Ctrl_S);
  } else if (evt_matches(KeyCombo.ctrlOrCmd_z)) {
    Some(CtrlOrCmd_Z);
  } else if (evt_matches(KeyCombo.ctrlOrCmd_shift_z)) {
    Some(CtrlOrCmd_Shift_Z);
  } else if (evt_matches(KeyCombo.up)) {
    Some(Up);
  } else if (evt_matches(KeyCombo.down)) {
    Some(Down);
  } else if (evt_matches(KeyCombo.left)) {
    Some(Left);
  } else if (evt_matches(KeyCombo.right)) {
    Some(Right);
  } else if (evt_matches(KeyCombo.home_key)) {
    Some(Home);
  } else if (evt_matches(KeyCombo.end_key)) {
    Some(End);
  } else if (evt_matches(KeyCombo.alt_up)) {
    Some(Alt_Up);
  } else if (evt_matches(KeyCombo.alt_down)) {
    Some(Alt_Down);
  } else if (evt_matches(KeyCombo.alt_left)) {
    Some(Alt_Left);
  } else if (evt_matches(KeyCombo.alt_right)) {
    Some(Alt_Right);
  } else if (evt_matches(KeyCombo.shift_up)) {
    Some(Shift_Up);
  } else if (evt_matches(KeyCombo.shift_down)) {
    Some(Shift_Down);
  } else if (evt_matches(KeyCombo.shift_left)) {
    Some(Shift_Left);
  } else if (evt_matches(KeyCombo.shift_right)) {
    Some(Shift_Right);
  } else if (evt_matches(KeyCombo.ctrl_up)) {
    Some(Ctrl_Up);
  } else if (evt_matches(KeyCombo.ctrl_down)) {
    Some(Ctrl_Down);
  } else if (evt_matches(KeyCombo.ctrl_left)) {
    Some(Ctrl_Left);
  } else if (evt_matches(KeyCombo.ctrl_right)) {
    Some(Ctrl_Right);
  } else if (single_key != None) {
    switch (single_key) {
    | None => None
    | Some(single_key) => Some(Single(single_key))
    };
  } else {
    None;
  };
};

let name = (combo: t): string => KeyCombo.name(get_details(combo));
