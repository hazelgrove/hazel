// TODO(undergrad): Rename to CaretPosition
// (including updating all variable
// naming that assumed cursor position).
// Talk to @d before starting.
[@deriving (sexp, show)]
type t =
  | OnText(CharIndex.t)
  | OnDelim(DelimIndex.t, Side.t)
  | OnOp(Side.t);

let text_cursors = (len: int): list(t) =>
  ListUtil.range(len + 1) |> List.map(j => OnText(j));

let delim_cursors_k = (k: int): list(t) => [
  OnDelim(k, Before),
  OnDelim(k, After),
];
let delim_cursors = (num_delim: int): list(t) =>
  ListUtil.range(num_delim)
  |> List.map(k => delim_cursors_k(k))
  |> List.flatten;
