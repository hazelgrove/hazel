open Sexplib.Std;
open Util;
include String;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let is_empty = (==)(empty);

let split = (n, tok) => {
  let l = String.sub(tok, 0, n);
  let r = String.sub(tok, n, String.length(tok) - n);
  (l, r);
};

let unzip = (n, tok): Result.t((t, t), Dir.t) =>
  if (n <= 0) {
    Error(L);
  } else if (n >= length(tok)) {
    Error(R);
  } else {
    Ok(split(n, tok));
  };
