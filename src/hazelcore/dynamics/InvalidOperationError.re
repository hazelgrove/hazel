[@deriving sexp]
type t =
  | DivideByZero
  | SubscriptOutOfBounds(UnescapedString.subscript_error);

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | SubscriptOutOfBounds(err) =>
    let subscript_err_msg = (loc, idx, lower, upper) =>
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
        subscript_err_msg("start", idx, lower, upper)
      | EndIndexOutOfBounds({idx, lower, upper}) =>
        subscript_err_msg("end", idx, lower, upper)
      | BothIndicesOutOfBounds(
          {idx: idx1, lower: lower1, upper: upper1},
          {idx: idx2, lower: lower2, upper: upper2},
        ) =>
        subscript_err_msg("start", idx1, lower1, upper1)
        ++ ", "
        ++ subscript_err_msg("end", idx2, lower2, upper2)
      | EndIndexBeforeStart({idx, lower, upper}) =>
        subscript_err_msg("end", idx, lower, upper)
      };
    "Error: " ++ msg;
  };
