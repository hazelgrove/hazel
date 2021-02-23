open Sexplib.Std;

[@deriving sexp]
type t = string;

let to_string = x => x;

let unescaped = s =>
  StringLitLexer.stringlit_body(
    Lexing.from_string(StringLitBody.to_string(s)),
  );

let equal = String.equal;

let cat = (s1, s2) => s1 ++ s2;

[@deriving sexp]
type subscript_error =
  | StartIndexOutOfBounds
  | EndIndexOutOfBounds
  | BothIndicesOutOfBounds
  | EndIndexBeforeStart;

type subscript_result =
  | Error(subscript_error)
  | OK(t);

/** Implements s[start, end) */
let subscript: (t, int, int) => subscript_result =
  (s, start_, end_) => {
    let length = String.length(s);
    let start_in_bounds = start_ >= 0 && start_ < length;
    let end_in_bounds = end_ >= 0 && end_ <= length;
    if (!start_in_bounds) {
      if (!end_in_bounds) {
        Error(BothIndicesOutOfBounds);
      } else {
        Error(StartIndexOutOfBounds);
      };
    } else if (!end_in_bounds) {
      Error(EndIndexOutOfBounds);
    } else if (end_ <= start_) {
      Error(EndIndexBeforeStart);
    } else {
      OK(String.sub(s, start_, end_ - start_));
    };
  };

/** Implements s[start, end) */;
/* let subscript = (s, start_, end_) => {
     let length = String.length(s);
     /* TODO: convert negative start_ and end_ */
     if (start_ >= 0 && start_ < length && end_ <= length && end_ >= start_) {
       String.sub(s, start_, end_ - start_);
     } else if (start_ < 0) {
       1;
     } else if (start_ >= length) {
       2;
     } else if (end_ > length) {
       3;
     } else if (end_ < start) {
       4;
     };
   }; */
