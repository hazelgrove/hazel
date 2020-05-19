open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t)
  | InvalidText(string);

/* Eventually replace Ocaml's ___of_string_opt with our own rules */
/* Ocaml accepts _1 as a float */
let hazel_float_of_string_opt = (s: string): option(float) =>
  if (String.length(s) > 0 && s.[0] == '_') {
    None;
  } else {
    float_of_string_opt(s);
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
    if (text |> String.equal("_")) {
      Underscore;
    } else if (text |> Var.is_valid) {
      Var(text);
    } else {
      InvalidText(text);
    }
  };
