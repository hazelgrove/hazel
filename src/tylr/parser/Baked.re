open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let fold = Chain.fold_left;

let height = Chain.length;

let is_eq = ((rels, toks): t) =>
  rels
  |> List.map(Rel.is_eq)
  |> OptUtil.sequence
  |> Option.map(cells => (cells, toks));
