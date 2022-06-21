open Sexplib.Std;

[@deriving sexp]
type t = string;

let to_string = s => s;

[@deriving sexp]
type valid_seq = UnescapedStringParser.valid_seq;

[@deriving sexp]
type invalid_seq = UnescapedStringParser.invalid_seq;

let from_string = s => s |> Lexing.from_string |> UnescapedStringParser.lex;

let from_string_unchecked = s => s;

let length = String.length;

let equal = String.equal;

let concat = (s1, s2) => s1 ++ s2;

[@deriving sexp]
type out_of_bounds_error = {
  idx: int,
  lower: int,
  upper: int,
};

[@deriving sexp]
type subscript_error =
  | StartIndexOutOfBounds(out_of_bounds_error)
  | EndIndexOutOfBounds(out_of_bounds_error)
  | BothIndicesOutOfBounds(out_of_bounds_error, out_of_bounds_error)
  | EndIndexBeforeStart(out_of_bounds_error)
  | EmptyString;

[@deriving sexp]
type subscript_result =
  | Ok(t)
  | Err(subscript_error);

let subscript = (s, n1, n2) => {
  let len = s |> length;
  if (len == 0) {
    Err(EmptyString);
  } else {
    let start_in_bounds = n1 >= 0 && n1 < len;
    let end_in_bounds = n2 >= 0 && n2 <= len;
    switch (start_in_bounds, end_in_bounds) {
    | (false, false) =>
      Err(
        BothIndicesOutOfBounds(
          {idx: n1, lower: 0, upper: len - 1},
          {idx: n2, lower: 0, upper: len},
        ),
      )
    | (false, true) =>
      Err(StartIndexOutOfBounds({idx: n1, lower: 0, upper: len - 1}))
    | (true, false) =>
      Err(EndIndexOutOfBounds({idx: n2, lower: 0, upper: len}))
    | (true, true) =>
      if (n2 < n1) {
        Err(EndIndexBeforeStart({idx: n2, lower: 0, upper: n1}));
      } else {
        Ok(String.sub(s, n1, n2 - n1));
      }
    };
  };
};
