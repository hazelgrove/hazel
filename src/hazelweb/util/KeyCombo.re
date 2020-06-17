module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let get_code = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.code, () => assert(false)));

let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

module Key = {
  type recognition_method =
    // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
    | Code(string)
    // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
    | Key(string);

  let code_of_letter = letter => "Key" ++ String.uppercase_ascii(letter);

  type t = {
    plain_name: string,
    recognition_methods: list(recognition_method),
  };

  let code1 = (plain_name, code) => {
    plain_name,
    recognition_methods: [Code(code)],
  };
  let code2 = (plain_name, code1, code2) => {
    plain_name,
    recognition_methods: [Code(code1), Code(code2)],
  };

  let key1 = (plain_name, key) => {
    plain_name,
    recognition_methods: [Key(key)],
  };

  let the_letter_code = letter => code1(letter, code_of_letter(letter));
  let the_code = code => code1(code, code);
  let the_key = key => key1(key, key);

  let recognize = (evt: Js.t(Dom_html.keyboardEvent), r) =>
    switch (r) {
    | Code(c) =>
      let code = get_code(evt);
      String.equal(code, c);
    | Key(k) =>
      let key = get_key(evt);
      String.equal(String.uppercase_ascii(key), String.uppercase_ascii(k));
    };

  let matches = (k, evt: Js.t(Dom_html.keyboardEvent)) => {
    let recognition_methods = k.recognition_methods;
    ListUtil.any(recognition_methods, recognize(evt));
  };
};

module Details = {
  type t = {
    mod_keys: ModKeys.t,
    key: Key.t,
  };

  let mk = (mod_keys, key) => {mod_keys, key};
  let plain = key => {mod_keys: ModKeys.not_held, key};
  let no_ctrl_alt_meta = key => {mod_keys: ModKeys.no_ctrl_alt_meta, key};
  let shift = key => {mod_keys: ModKeys.shift, key};
  let ctrl = key => {mod_keys: ModKeys.ctrl, key};
  let alt = key => {mod_keys: ModKeys.alt, key};
  let meta = key => {mod_keys: ModKeys.meta, key};
  let ctrl_shift = key => {mod_keys: ModKeys.ctrl_shift, key};
  let ctrl_alt = key => {mod_keys: ModKeys.ctrl_alt, key};
  let meta_shift = key => {mod_keys: ModKeys.meta_shift, key};

  let matches = (kc, evt: Js.t(Dom_html.keyboardEvent)) =>
    ModKeys.matches(kc.mod_keys, evt) && Key.matches(kc.key, evt);

  let name = kc => {
    let mod_prefix = ModKeys.mod_prefix(kc.mod_keys);
    mod_prefix ++ kc.key.plain_name;
  };

  let enter = plain(Key.code2("Enter", "Enter", "NumpadEnter"));
  let escape = plain(Key.the_code("Escape"));
  let backspace = plain(Key.the_code("Backspace"));
  let delete = plain(Key.the_code("Delete"));
  let tab = plain(Key.the_code("Tab"));
  let shift_tab = shift(Key.the_code("Tab"));
  let space = plain(Key.the_code("Space"));
  let lt = no_ctrl_alt_meta(Key.the_key("<"));
  let gt = no_ctrl_alt_meta(Key.the_key(">"));
  let colon = no_ctrl_alt_meta(Key.the_key(":"));
  let backslash = no_ctrl_alt_meta(Key.the_key("\\"));
  let left_parens = no_ctrl_alt_meta(Key.the_key("("));
  let right_parens = no_ctrl_alt_meta(Key.the_key(")"));
  let left_bracket = no_ctrl_alt_meta(Key.the_key("["));
  let right_bracket = no_ctrl_alt_meta(Key.the_key("]"));
  let qmark = no_ctrl_alt_meta(Key.the_key("?"));
  let equals = no_ctrl_alt_meta(Key.the_key("="));
  let pound = no_ctrl_alt_meta(Key.the_key("#"));
  let plus = no_ctrl_alt_meta(Key.the_key("+"));
  let minus = no_ctrl_alt_meta(Key.the_key("-"));
  let asterisk = no_ctrl_alt_meta(Key.the_key("*"));
  let slash = no_ctrl_alt_meta(Key.the_key("/"));
  let semicolon = no_ctrl_alt_meta(Key.the_key(";"));
  let comma = no_ctrl_alt_meta(Key.the_key(","));
  let vbar = no_ctrl_alt_meta(Key.the_key("|"));
  let ampersand = no_ctrl_alt_meta(Key.the_key("&"));
  let dollar = no_ctrl_alt_meta(Key.the_key("$"));
  let amp = no_ctrl_alt_meta(Key.the_key("&"));
  let alt_L = alt(Key.the_key("l"));
  let alt_R = alt(Key.the_key("r"));
  let alt_C = alt(Key.the_key("c"));
  let alt_PageUp = alt(Key.the_key("PageUp"));
  let alt_PageDown = alt(Key.the_key("PageDown"));
  let alt_T = alt(Key.the_key("T"));
  let alt_F = alt(Key.the_key("F"));
  let ctrl_z = ctrl(Key.the_key("z"));
  let ctrl_shift_z = ctrl_shift(Key.the_key("Z"));
  let ctrl_alt_i = ctrl_alt(Key.the_key("i"));
  let ctrl_alt_k = ctrl_alt(Key.the_key("k"));
  let ctrl_alt_j = ctrl_alt(Key.the_key("j"));
  let ctrl_alt_l = ctrl_alt(Key.the_key("l"));
  let meta_z = ctrl(Key.the_key("z"));
  let meta_shift_z = ctrl_shift(Key.the_key("Z"));
};

[@deriving sexp]
type t =
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
  | Ctrl_Z
  | Ctrl_Shift_Z
  | Ctrl_Alt_I
  | Ctrl_Alt_K
  | Ctrl_Alt_J
  | Ctrl_Alt_L
  | Meta_Z
  | Meta_Shift_Z;

let get_details =
  fun
  | Pound => Details.pound
  | Escape => Details.escape
  | Backspace => Details.backspace
  | Delete => Details.delete
  | ShiftTab => Details.shift_tab
  | Tab => Details.tab
  | GT => Details.gt
  | Ampersand => Details.ampersand
  | VBar => Details.vbar
  | LeftParen => Details.left_parens
  | Colon => Details.colon
  | Equals => Details.equals
  | Enter => Details.enter
  | Backslash => Details.backslash
  | Plus => Details.plus
  | Minus => Details.minus
  | Asterisk => Details.asterisk
  | Slash => Details.slash
  | LT => Details.lt
  | Space => Details.space
  | Comma => Details.comma
  | LeftBracket => Details.left_bracket
  | Semicolon => Details.semicolon
  | Alt_L => Details.alt_L
  | Alt_R => Details.alt_R
  | Alt_C => Details.alt_C
  | Ctrl_Z => Details.ctrl_z
  | Ctrl_Shift_Z => Details.ctrl_shift_z
  | Ctrl_Alt_I => Details.ctrl_alt_i
  | Ctrl_Alt_K => Details.ctrl_alt_k
  | Ctrl_Alt_J => Details.ctrl_alt_j
  | Ctrl_Alt_L => Details.ctrl_alt_l
  | Meta_Z => Details.meta_z
  | Meta_Shift_Z => Details.meta_shift_z;

let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
  let evt_matches = details => Details.matches(details, evt);
  if (evt_matches(Details.pound)) {
    Some(Pound);
  } else if (evt_matches(Details.ctrl_z)) {
    Some(Ctrl_Z);
  } else if (evt_matches(Details.ctrl_shift_z)) {
    Some(Ctrl_Shift_Z);
  } else if (evt_matches(Details.meta_z)) {
    Some(Meta_Z);
  } else if (evt_matches(Details.meta_shift_z)) {
    Some(Meta_Shift_Z);
  } else if (evt_matches(Details.escape)) {
    Some(Escape);
  } else if (evt_matches(Details.backspace)) {
    Some(Backspace);
  } else if (evt_matches(Details.delete)) {
    Some(Delete);
  } else if (evt_matches(Details.shift_tab)) {
    Some(ShiftTab);
  } else if (evt_matches(Details.tab)) {
    Some(Tab);
  } else if (evt_matches(Details.gt)) {
    Some(GT);
  } else if (evt_matches(Details.ampersand)) {
    Some(Ampersand);
  } else if (evt_matches(Details.vbar)) {
    Some(VBar);
  } else if (evt_matches(Details.left_parens)) {
    Some(LeftParen);
  } else if (evt_matches(Details.colon)) {
    Some(Colon);
  } else if (evt_matches(Details.equals)) {
    Some(Equals);
  } else if (evt_matches(Details.enter)) {
    Some(Enter);
  } else if (evt_matches(Details.backslash)) {
    Some(Backslash);
  } else if (evt_matches(Details.plus)) {
    Some(Plus);
  } else if (evt_matches(Details.minus)) {
    Some(Minus);
  } else if (evt_matches(Details.asterisk)) {
    Some(Asterisk);
  } else if (evt_matches(Details.slash)) {
    Some(Slash);
  } else if (evt_matches(Details.lt)) {
    Some(LT);
  } else if (evt_matches(Details.space)) {
    Some(Space);
  } else if (evt_matches(Details.comma)) {
    Some(Comma);
  } else if (evt_matches(Details.left_bracket)) {
    Some(LeftBracket);
  } else if (evt_matches(Details.semicolon)) {
    Some(Semicolon);
  } else if (evt_matches(Details.alt_L)) {
    Some(Alt_L);
  } else if (evt_matches(Details.alt_R)) {
    Some(Alt_R);
  } else if (evt_matches(Details.alt_C)) {
    Some(Alt_C);
  } else if (evt_matches(Details.ctrl_alt_i)) {
    Some(Ctrl_Alt_I);
  } else if (evt_matches(Details.ctrl_alt_k)) {
    Some(Ctrl_Alt_K);
  } else if (evt_matches(Details.ctrl_alt_j)) {
    Some(Ctrl_Alt_J);
  } else if (evt_matches(Details.ctrl_alt_l)) {
    Some(Ctrl_Alt_L);
  } else {
    None;
  };
};
