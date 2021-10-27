module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

/**
 * Holding down the option key can result in special characters being typed instead of the intended character
 * This function tries to return the physical key that was typed in a way that respects alternative keyboard layouts
 */
let get_key_without_option = (evt: Js.t(Dom_html.keyboardEvent)) => {
  let key: string = get_key(evt);
  let option_held = IsMac.is_mac && Js.to_bool(evt##.altKey);

  if (option_held) {
    switch (key) {
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
      failwith(
        __LOC__ ++ ": Key cannot be recognized because it is a dead key",
      )
    | _ => key
    };
  } else {
    key;
  };
};

type t = {
  plain_name: string,
  recognized_keys: list(string),
};

let key1 = (plain_name, key) => {plain_name, recognized_keys: [key]};

let the_key = key => key1(key, key);

let matches = (key: t, evt: Js.t(Dom_html.keyboardEvent)) => {
  let recognize = key => {
    let evt_key = get_key_without_option(evt);
    String.equal(
      String.uppercase_ascii(evt_key),
      String.uppercase_ascii(key),
    );
  };

  ListUtil.any(key.recognized_keys, recognize);
};
