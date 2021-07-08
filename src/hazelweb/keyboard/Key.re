module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

/**
 * Holding down the option key can result in special characters being typed instead of the intended character
 * This function tries to return the physical key that was typed in a way that respects alternative keyboard layouts
 */
let without_option_key =
  fun
  | "å" => "a"
  | "Å" => "A"
  | "∫" => "b"
  | "ı" => "B"
  | "ç" => "c"
  | "Ç" => "C"
  | "∂" => "d"
  | "Î" => "D"
  | "´" => "E"
  | "ƒ" => "f"
  | "Ï" => "F"
  | "©" => "g"
  | "˝" => "G"
  | "˙" => "h"
  | "Ó" => "H"
  | "^" => "i"
  | "ˆ" => "I"
  | "∆" => "j"
  | "Ô" => "J"
  | "˚" => "k"
  | "" => "K"
  | "¬" => "l"
  | "Ò" => "L"
  | "µ" => "m"
  | "Â" => "M"
  | "˜" => "N"
  | "ø" => "o"
  | "Ø" => "O"
  | "π" => "p"
  | "∏" => "P"
  | "œ" => "q"
  | "Œ" => "Q"
  | "®" => "r"
  | "‰" => "R"
  | "ß" => "s"
  | "Í" => "S"
  | "†" => "t"
  | "ˇ" => "T"
  | "¨" => "U"
  | "√" => "v"
  | "◊" => "V"
  | "∑" => "w"
  | "„" => "W"
  | "≈" => "x"
  | "˛" => "X"
  | "¥" => "y"
  | "Á" => "Y"
  | "Ω" => "z"
  | "¸" => "Z"
  | "¡" => "1"
  | "⁄" => "!"
  | "™" => "2"
  | "€" => "@"
  | "£" => "3"
  | "‹" => "#"
  | "¢" => "4"
  | "›" => "$"
  | "∞" => "5"
  | "ﬁ" => "%"
  | "§" => "6"
  | "ﬂ" => "^"
  | "¶" => "7"
  | "‡" => "&"
  | "•" => "8"
  | "°" => "*"
  | "ª" => "9"
  | "·" => "("
  | "º" => "0"
  | "‚" => ")"
  | "–" => "-"
  | "—" => "_"
  | "≠" => "="
  | "±" => "+"
  | "“" => "["
  | "”" => "{"
  | "‘" => "]"
  | "’" => "}"
  | "«" => "\\"
  | "»" => "|"
  | "…" => ";"
  | "Ú" => ":"
  | "æ" => "'"
  | "Æ" => "\""
  | "≤" => ","
  | "¯" => "<"
  | "≥" => "."
  | "˘" => ">"
  | "÷" => "/"
  | "¿" => "?"
  // https://en.wikipedia.org/wiki/Dead_key
  | "Dead" =>
    failwith(__LOC__ ++ ": Key cannot be recognized because it is a dead key")
  | other => other;

/**
 * Avoid Using!
 * Does not respect non-QWERTY keyboard layouts
 */
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
    let key = without_option_key(get_key(evt));
    String.equal(String.uppercase_ascii(key), String.uppercase_ascii(k));
  };

let matches = (k, evt: Js.t(Dom_html.keyboardEvent)) => {
  let recognition_methods = k.recognition_methods;
  ListUtil.any(recognition_methods, recognize(evt));
};
