open GeneralUtil;

[@deriving sexp]
type t =
  | OnText(CharIndex.t)
  | OnDelim(DelimIndex.t, Side.t)
  | OnOp(Side.t);

let text_cursors = (len: int): list(t) =>
  range(len + 1) |> List.map(j => OnText(j));

let delim_cursors_k = (k: int): list(t) => [
  OnDelim(k, Before),
  OnDelim(k, After),
];
let delim_cursors = (num_delim: int): list(t) =>
  range(num_delim) |> List.map(k => delim_cursors_k(k)) |> List.flatten;
