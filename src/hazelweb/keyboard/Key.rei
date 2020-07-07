module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let get_code: Js.t(Dom_html.keyboardEvent) => string;

let get_key: Js.t(Dom_html.keyboardEvent) => string;

type recognition_method =
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
  | Code(string)
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
  | Key(string);

let code_of_letter: string => string;

type t = {
  plain_name: string,
  recognition_methods: list(recognition_method),
};

let code1: (string, string) => t;

let code2: (string, string, string) => t;

let key1: (string, string) => t;

let the_letter_code: string => t;

let the_code: string => t;

let the_key: string => t;

let recognize: (Js.t(Dom_html.keyboardEvent), recognition_method) => bool;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;
