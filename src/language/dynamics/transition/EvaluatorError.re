open Util;

/* EvaluatorErrors should never be thrown, they indicate something wrong
   with Hazel's implementation. (As opposed to InvalidOperationErrors
   which indicate something wrong with the user's code.) */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InvalidBuiltin(string);

exception Exception(t);
