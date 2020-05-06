open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | IntLit(string)
  | FloatLit(string)
  | BoolLit(bool)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

let of_text = (text: string): option(t) => {
  let stripped_text = StringUtil.strip_underscores(text);
  print_endline(stripped_text);
  switch (
    int_of_string_opt(stripped_text),
    float_of_string_opt(stripped_text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
  ) {
  /* the num_digits double count with leading zeros when the int is evaluated to 0  */
  | (Some(n), _, _, _) when n == 0 =>
    StringUtil.num_leading_zeros(stripped_text)
    == String.length(stripped_text)
      ? Some(IntLit(stripped_text)) : None
  | (Some(n), _, _, _) =>
    // OCaml accepts and ignores underscores
    // when parsing ints and floats from strings, we don't
    print_endline(string_of_int(n));
    print_endline(
      string_of_int(
        StringUtil.num_leading_zeros(stripped_text) + IntUtil.num_digits(n),
      ),
    );
    print_endline(string_of_int(String.length(stripped_text)));
    StringUtil.num_leading_zeros(stripped_text)
    + IntUtil.num_digits(n) == String.length(stripped_text)
      ? Some(IntLit(stripped_text)) : None;
  /* 1 is subtracted from num_digits because Ocaml introduces extra 0 in front of the decimal when float is < 1 */
  | (_, Some(f), _, _) when Float.abs(f) < 1.0 =>
    StringUtil.num_leading_zeros(stripped_text)
    + FloatUtil.num_digits(f)
    - 1
    + StringUtil.num_trailing_zeros(stripped_text)
    == String.length(stripped_text)
      ? Some(FloatLit(stripped_text)) : None
  | (_, Some(f), _, _) =>
    print_endline("here");
    StringUtil.num_leading_zeros(stripped_text)
    + FloatUtil.num_digits(f)
    + StringUtil.num_trailing_zeros(stripped_text)
    == String.length(stripped_text)
      ? Some(FloatLit(stripped_text)) : None;
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
};
