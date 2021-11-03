open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | Keyword(Keyword.kw)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t)
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

let of_text = (text: string): t =>
  switch (
    int_of_string_opt(text),
    hazel_float_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
    Keyword.kw_of_string(text),
  ) {
  | (Some(_), _, _, _, _) => IntLit(text)
  | (_, Some(_), _, _, _) => FloatLit(text)
  | (_, _, Some(b), _, _) => BoolLit(b)
  | (_, _, _, Some(k), _) => ExpandingKeyword(k)
  | (_, _, _, _, Some(kw)) => Keyword(kw) //need to look up for the most recent unique id of assert
  | (None, None, None, None, None) =>
    if (text |> String.equal("_")) {
      Underscore;
    } else if (text |> Var.is_valid) {
      Var(text);
    } else {
      InvalidTextShape(text);
    }
  };
