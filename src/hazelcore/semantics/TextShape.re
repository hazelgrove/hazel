open Sexplib.Std;

[@deriving sexp]
type t =
  | Underscore
  | NumLit(int)
  | BoolLit(bool)
  // | StringLit(string)
  | ExpandingKeyword(ExpandingKeyword.t)
  | Var(Var.t);

let of_text = (text: string): option(t) =>
  // if (ExpandingKeyword.mk(text) == None) {
  //   print_endline("True");
  // } else {
  //   print_endline("False");
  switch (
    int_of_string_opt(text),
    bool_of_string_opt(text),
    ExpandingKeyword.mk(text),
  ) {
  | (Some(n), _, _) =>
    // OCaml accepts and ignores underscores
    // when parsing ints from strings, we don't
    print_endline("Textshape21");
    IntUtil.num_digits(n) == String.length(text) ? Some(NumLit(n)) : None;
  | (_, Some(b), _) =>
    print_endline("Textshape23");
    Some(BoolLit(b));
  | (_, _, Some(k)) =>
    print_endline("Textshape24");
    Some(ExpandingKeyword(k));
  | (None, None, None) =>
    print_endline("Textshape34");
    if (text |> String.equal("_")) {
      print_endline("Textshape36");
      Some(Underscore);
    } else if (text |> Var.is_valid) {
      print_endline("Textshape39");
      Some(Var(text));
    } else {
      print_endline("Textshape39");
      None;
    };
  };
// };
