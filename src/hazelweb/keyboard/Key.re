module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let get_code = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.code, () => assert(false)));

let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

type recognition_method =
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
  | Code(string)
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
  | Key(string);

type t = {
  plain_name: string,
  recognition_methods: list(recognition_method),
};

let key1 = (plain_name, key) => {
  plain_name,
  recognition_methods: [Key(key)],
};

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
