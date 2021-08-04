open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t)
  | Tag(string)
  | InvalidTextShape(string);

/* Eventually replace Ocaml's ___of_string_opt with our own rules */
/* Ocaml accepts _1 as a float */
let hazel_float_of_string_opt = (s: string): option(float) =>
  if (String.length(s) > 0 && s.[0] == '_') {
    None;
  } else {
    switch (s, String.lowercase_ascii(s)) {
    | ("NaN", _) => Some(nan)
    | ("Inf", _) => Some(infinity)
    /* TODO: NegInf is temporarily introduced until unary minus is introduced to Hazel */
    | ("NegInf", _) => Some(neg_infinity)
    | (_, "nan")
    | (_, "inf")
    | (_, "infinity") => None
    | _ => float_of_string_opt(s)
    };
  };

let is_majuscule = (c: char): bool => {
  let code = Char.code(c);
  Char.code('A') <= code && code <= Char.code('Z');
};

let is_minuscule = (c: char): bool => {
  let code = Char.code(c);
  Char.code('a') <= code && code <= Char.code('z');
};

let is_numeric_digit = (c: char): bool => {
  let code = Char.code(c);
  Char.code('0') <= code && code <= Char.code('9');
};

let is_tag_char = (c: char): bool => {
  is_majuscule(c) || is_minuscule(c) || is_numeric_digit(c);
};

let is_tag_chars = (cs: list(char)): bool => List.for_all(is_tag_char, cs);

let is_tag_name = (str: string): bool => {
  let chars = str |> String.to_seq |> List.of_seq;
  switch (chars) {
  | [] => false
  | [c] => is_majuscule(c)
  | [c, ...cs] => is_majuscule(c) && is_tag_chars(cs)
  };
};

let of_text = (text: string): t =>
  switch (
    int_of_string_opt(text),
    hazel_float_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
  ) {
  | (Some(_), _, _, _) => IntLit(text)
  | (_, Some(_), _, _) => FloatLit(text)
  | (_, _, Some(b), _) => BoolLit(b)
  | (_, _, _, Some(k)) => ExpandingKeyword(k)
  | (None, None, None, None) =>
    if (is_tag_name(text)) {
      Tag(text);
    } else if (text |> String.equal("_")) {
      Underscore;
    } else if (text |> Var.is_valid) {
      Var(text);
    } else {
      InvalidTextShape(text);
    }
  };
