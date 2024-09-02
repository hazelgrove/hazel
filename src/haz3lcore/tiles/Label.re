open Util;

/* A label is the textual expression of a form's delimiters */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Token.t);
exception Empty_label;
