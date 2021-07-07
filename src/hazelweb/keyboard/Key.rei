module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

/* helpful tool: https://keycode.info/ */
let get_key: Js.t(Dom_html.keyboardEvent) => string;

type recognition_method =
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
  | Code(string)
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
  | Key(string);

/**
 * A key recognized by Hazel
 */
type t = {
  /* displayed to user in action panel */
  plain_name: string,
  recognition_methods: list(recognition_method),
};

/**
 * Creates a `t` with a single Key recognition method
 */
let key1: (string, string) => t;

/**
 * Creates a `t` with a single Code recognition method
 */
let with_code: (string, string) => t;

/**
 * Creates a `t` whose plain name is the same as the key
 */
let the_key: string => t;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;
