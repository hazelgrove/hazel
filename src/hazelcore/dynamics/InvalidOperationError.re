[@deriving sexp]
type t =
  | DivideByZero
  | SubscriptOutOfBounds(UnescapedString.subscript_error);

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "divide by zero"
  | SubscriptOutOfBounds(UnescapedString.StartIndexOutOfBounds) => "start index out of bounds"
  | SubscriptOutOfBounds(UnescapedString.EndIndexOutOfBounds) => "end index out of bounds"
  | SubscriptOutOfBounds(UnescapedString.BothIndicesOutOfBounds) => "start and end index both out of bounds"
  | SubscriptOutOfBounds(UnescapedString.EndIndexBeforeStart) => "end index before start index"
  };
