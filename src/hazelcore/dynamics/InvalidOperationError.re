[@deriving sexp]
type t =
  | DivideByZero
  | SubscriptOutOfBounds(UnescapedString.subscript_error)
  | InvalidIntOfString
  | InvalidFloatOfString
  | InvalidBoolOfString;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"

  | SubscriptOutOfBounds(err) =>
    let oob_msg = (loc, idx, lower, upper) =>
      loc
      ++ " index "
      ++ string_of_int(idx)
      ++ " out of bounds ["
      ++ string_of_int(lower)
      ++ ", "
      ++ string_of_int(upper)
      ++ "]";

    let msg =
      switch (err) {
      | StartIndexOutOfBounds({idx, lower, upper}) =>
        oob_msg("Start", idx, lower, upper)
      | EndIndexOutOfBounds({idx, lower, upper}) =>
        oob_msg("End", idx, lower, upper)
      | BothIndicesOutOfBounds(
          {idx: idx1, lower: lower1, upper: upper1},
          {idx: idx2, lower: lower2, upper: upper2},
        ) =>
        oob_msg("Start", idx1, lower1, upper1)
        ++ ", "
        ++ oob_msg("end", idx2, lower2, upper2)
      | EndIndexBeforeStart({idx, lower, upper}) =>
        oob_msg("End", idx, lower, upper)
      | EmptyString => "Cannot subscript an empty string"
      };

    "Error: " ++ msg;

  | InvalidIntOfString => "Error: Cannot be parsed as Int"
  | InvalidFloatOfString => "Error: Cannot be parsed as Float"
  | InvalidBoolOfString => "Error: Cannot be parsed as Bool"
  };
