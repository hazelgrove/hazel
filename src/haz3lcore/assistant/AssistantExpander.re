/* Bit of a hack. We want to decorate suggestions which will trigger
   an expansion to telegraph that expansion. Easiest way metrics wise
   is to keep that deco in the syntax. Want to decorate with ellipses
   character, but OCaml string functions don't support unicode, so
   we use $, then swap it out for the unicode character in Code.
   Eventually replace this by extending the suggestion data structure */
let c = "$";

let is_expander_tok = (t: Token.t) =>
  String.sub(t, String.length(t) - 1, 1) == c;

let trim_last = (t: Token.t) => String.sub(t, 0, String.length(t) - 1);

let is_expander = (label: Label.t) =>
  switch (label) {
  | [t] => is_expander_tok(t)
  | _ => false
  };

let mark = (label: Label.t): Label.t =>
  is_expander(label) ? List.map(t => trim_last(t) ++ "…", label) : label;

let trim = (completion: Token.t): Token.t =>
  is_expander_tok(completion) ? trim_last(completion) : completion;
