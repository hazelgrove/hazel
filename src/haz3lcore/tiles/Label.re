open Util;

/* A label is the textual expression of a form's delimiters */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Token.t);
exception Empty_label;

let length: t => int = List.length;

let rev: t => t = List.rev;

let hd_tl = (lbl: t): (Token.t, list(Token.t)) =>
  switch (lbl) {
  | [] => raise(Empty_label)
  | [hd, ...tl] => (hd, tl)
  };
