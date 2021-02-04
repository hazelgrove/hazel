module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type t = {
  mod_keys: ModKeys.t,
  key: Key.t,
};

let plain = key => {mod_keys: ModKeys.not_held, key};
let no_ctrlOrCmd_alt = key => {mod_keys: ModKeys.no_ctrlOrCmd_alt, key};
let ctrl = key => {mod_keys: ModKeys.ctrl, key};
let shift = key => {mod_keys: ModKeys.shift, key};
let ctrl_shift = key => {mod_keys: ModKeys.ctrl_shift, key};
let ctrlOrCmd = key => {mod_keys: ModKeys.ctrlOrCmd, key};
let alt = key => {mod_keys: ModKeys.alt, key};
let ctrlOrCmd_shift = key => {mod_keys: ModKeys.ctrlOrCmd_shift, key};

let matches = (kc, evt: Js.t(Dom_html.keyboardEvent)) =>
  ModKeys.matches(kc.mod_keys, evt) && Key.matches(kc.key, evt);

let name = kc => {
  let mod_prefix = ModKeys.mod_prefix(kc.mod_keys);
  mod_prefix ++ kc.key.plain_name;
};

let enter = plain(Key.the_key("Enter"));
let escape = plain(Key.the_key("Escape"));
let backspace = plain(Key.the_key("Backspace"));
let delete = plain(Key.the_key("Delete"));
let tab = plain(Key.the_key("Tab"));
let shift_tab = shift(Key.the_key("Tab"));
let shift_enter = shift(Key.the_key("Enter")); // For creating another "CommentLine"
let space = plain(Key.key1("Space", " "));
let lt = no_ctrlOrCmd_alt(Key.the_key("<"));
let gt = no_ctrlOrCmd_alt(Key.the_key(">"));
let colon = no_ctrlOrCmd_alt(Key.the_key(":"));
let backslash = no_ctrlOrCmd_alt(Key.the_key("\\"));
let left_paren = no_ctrlOrCmd_alt(Key.the_key("("));
let right_paren = no_ctrlOrCmd_alt(Key.the_key(")"));
let left_bracket = no_ctrlOrCmd_alt(Key.the_key("["));
let right_brace = no_ctrlOrCmd_alt(Key.the_key("}"));
let right_square_bracket = no_ctrlOrCmd_alt(Key.the_key("]"));
let equals = no_ctrlOrCmd_alt(Key.the_key("="));
let pound = no_ctrlOrCmd_alt(Key.the_key("#"));
let plus = no_ctrlOrCmd_alt(Key.the_key("+"));
let minus = no_ctrlOrCmd_alt(Key.the_key("-"));
let asterisk = no_ctrlOrCmd_alt(Key.the_key("*"));
let slash = no_ctrlOrCmd_alt(Key.the_key("/"));
let semicolon = no_ctrlOrCmd_alt(Key.the_key(";"));
let comma = no_ctrlOrCmd_alt(Key.the_key(","));
let vbar = no_ctrlOrCmd_alt(Key.the_key("|"));
let ampersand = no_ctrlOrCmd_alt(Key.the_key("&"));
let alt_L = alt(Key.the_key("L"));
let alt_R = alt(Key.the_key("R"));
let alt_C = alt(Key.the_key("C"));
let tilde = no_ctrlOrCmd_alt(Key.the_key("~"));
let alt_PageUp = alt(Key.the_key("PageUp"));
let alt_PageDown = alt(Key.the_key("PageDown"));
let ctrl_space = ctrl(Key.key1("Space", " "));
let ctrl_s = ctrl(Key.the_key("S"));
let ctrl_shift_s = ctrl_shift(Key.the_key("S"));
let ctrlOrCmd_z = ctrlOrCmd(Key.the_key("Z"));
let ctrlOrCmd_shift_z = ctrlOrCmd_shift(Key.the_key("Z"));
let up = plain(Key.key1(Unicode.up_arrow_key, "ArrowUp"));
let down = plain(Key.key1(Unicode.down_arrow_key, "ArrowDown"));
let left = plain(Key.key1(Unicode.left_arrow_key, "ArrowLeft"));
let right = plain(Key.key1(Unicode.right_arrow_key, "ArrowRight"));
let home_key = plain(Key.the_key("Home"));
let end_key = plain(Key.the_key("End"));
let alt_up = alt(Key.key1(Unicode.up_arrow_key, "ArrowUp"));
let alt_down = alt(Key.key1(Unicode.down_arrow_key, "ArrowDown"));
let alt_left = alt(Key.key1(Unicode.left_arrow_key, "ArrowLeft"));
let alt_right = alt(Key.key1(Unicode.right_arrow_key, "ArrowRight"));
