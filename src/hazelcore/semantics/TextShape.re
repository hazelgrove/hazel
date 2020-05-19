open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  // | StringLit(string)
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

let of_text = (text: string): option(t) => {
  switch (
    int_of_string_opt(text),
    hazel_float_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
  ) {
  | (Some(_), _, _, _) => Some(IntLit(text))
  | (_, Some(_), _, _) => Some(FloatLit(text))
  | (_, _, Some(b), _) => Some(BoolLit(b))
  | (_, _, _, Some(k)) => Some(ExpandingKeyword(k))
  | (None, None, None, None) =>
    if (text |> String.equal("_")) {
      print_endline("Textshape36");
      Some(Underscore);
    } else if (text |> Var.is_valid) {
      print_endline("Textshape39");
      Some(Var(text));
    } else {
      print_endline("Textshape39");
      None;
    }
  };
};
