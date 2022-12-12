module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html /* helpful tool: https://keycode.info/ */;

let get_key: Js.t(Dom_html.keyboardEvent) => string;

/**
 * A key recognized by Hazel
 */
type t = {
  /* displayed to user in action panel */
  plain_name: string,
  recognized_keys: list(string),
};

/**
 * Creates a `t` with a single Key recognition method
 */
let key1: (string, string) => t;

/**
 * Creates a `t` whose plain name is the same as the key
 */
let the_key: string => t;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;
