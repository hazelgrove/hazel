open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | NumLit(int)
  | BoolLit(bool)
  | StringLit(string)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

let of_text = (text: string): option(t) =>
  switch (
    int_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
    Some(text),
  ) {
  | (Some(n), _, _, _) =>
    // OCaml accepts and ignores underscores
    // when parsing ints from strings, we don't
    IntUtil.num_digits(n) == String.length(text) ? Some(NumLit(n)) : None
  | (_, Some(b), _, _) => Some(BoolLit(b))
  | (_, _, Some(k), _) => Some(ExpandingKeyword(k))
  | (_, _, _, Some(s)) => Some(StringLit(s))
  | (None, None, None, None) =>
    if (text |> String.equal("_")) {
      Some(Underscore);
    } else if (text |> Var.is_valid) {
      Some(Var(text));
    } else {
      None;
    }
  };
