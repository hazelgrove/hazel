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

let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));

type t = {
  plain_name: string,
  recognized_keys: list(string),
};

let key1 = (plain_name, key) => {plain_name, recognized_keys: [key]};

let the_key = key => key1(key, key);

let matches = (key: t, evt: Js.t(Dom_html.keyboardEvent)) => {
  let recognize = key => {
    let evt_key = evt |> get_key |> without_option_key;
    String.equal(
      String.uppercase_ascii(evt_key),
      String.uppercase_ascii(key),
    );
  };

  ListUtil.any(key.recognized_keys, recognize);
};
