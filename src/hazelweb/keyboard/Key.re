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
