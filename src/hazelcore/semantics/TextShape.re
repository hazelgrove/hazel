open GeneralUtil;

type t =
  | NumLit(int)
  | BoolLit(bool)
  | Var(Var.t)
  | Underscore
  | Keyword(Keyword.t);

let of_text = (text: string): option(t) =>
  switch (int_of_string_opt(text), bool_of_string_opt(text)) {
  | (Some(n), _) =>
    // OCaml accepts and ignores underscores
    // when parsing ints from strings, we don't
    num_digits(n) == String.length(text) ? Some(NumLit(n)) : None
  | (_, Some(b)) => Some(BoolLit(b))
  | (None, None) =>
    if (text == "_") {
      Some(Underscore);
    } else if (text |> Var.is_let) {
      Some(Keyword(Let));
    } else if (text |> Var.is_case) {
      Some(Keyword(Case));
    } else if (text |> Var.is_valid) {
      Some(Var(text));
    } else {
      None;
    }
  };
