open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

let of_text = (text: string): option(t) =>
  switch (
    int_of_string_opt(text),
    float_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
  ) {
  | (Some(n), _, _, _) =>
    // OCaml accepts and ignores underscores
    // when parsing ints and floats from strings, we don't
    IntUtil.num_digits(n) == String.length(text) ? Some(IntLit(n)) : None
  | (_, Some(f), _, _) =>
    FloatUtil.num_digits(f) == String.length(text)
      ? Some(FloatLit(f)) : None
  | (_, _, Some(b), _) => Some(BoolLit(b))
  | (_, _, _, Some(k)) => Some(ExpandingKeyword(k))
  | (None, None, None, None) =>
    if (text |> String.equal("_")) {
      Some(Underscore);
    } else if (text |> Var.is_valid) {
      Some(Var(text));
    } else {
      None;
    }
  };
