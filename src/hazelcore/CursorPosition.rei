// TODO(undergrad): Rename to CaretPosition
// (including updating all variable
// naming that assumed cursor position).
// Talk to @d before starting.
[@deriving (sexp, show)]
type t =
  | OnText(CharIndex.t)
  | OnDelim(DelimIndex.t, Side.t)
  | OnOp(Side.t);

let text_cursors: int => list(t);

let delim_cursors_k: int => list(t);

let delim_cursors: int => list(t);
