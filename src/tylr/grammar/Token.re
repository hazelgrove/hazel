open Sexplib.Std;
open Util;
include String;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let split = (n, tok) => {
  let l = String.sub(tok, 0, n);
  let r = String.sub(tok, n, String.length(tok) - n);
  (l, r);
};

let unzip = (n, tok): Either.t(Dir.t, (t, t)) =>
  if (n <= 0) {
    L(L);
  } else if (n >= length(tok)) {
    L(R);
  } else {
    R(split(n, tok));
  };
