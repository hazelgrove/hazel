open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | AssertLit
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

/* Eventually replace Ocaml's ___of_string_opt with our own rules */
/* Ocaml accepts _1 as a float */
let hazel_float_of_string_opt = (s: string): option(float) =>
  if (String.length(s) > 0 && s.[0] == '_') {
    None;
  } else {
    float_of_string_opt(s);
  };

let is_assert = (text: string) => {
  String.equal("assert", text);
};

let of_text = (text: string): option(t) => {
  switch (
    int_of_string_opt(text),
    hazel_float_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
    is_assert(text),
  ) {
  | (Some(_), _, _, _, _) => Some(IntLit(text))
  | (_, Some(_), _, _, _) => Some(FloatLit(text))
  | (_, _, Some(b), _, _) => Some(BoolLit(b))
  | (_, _, _, Some(k), _) => Some(ExpandingKeyword(k))
  | (_, _, _, _, true) => Some(AssertLit)
  | (None, None, None, None, _) =>
    if (text |> String.equal("_")) {
      Some(Underscore);
    } else if (text |> Var.is_valid) {
      Some(Var(text));
    } else {
      None;
    }
  };
};
