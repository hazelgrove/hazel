open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let height = Chain.length;

let is_eq = ((rels, toks): t) =>
  rels
  |> List.map(Rel.is_eq)
  |> OptUtil.sequence
  |> Option.map(cells => (cells, toks));

module Fill = {
  type t = list(Cell.t);
  let empty = [];
  let cons = (cell, fill) =>
    switch (fill) {
    | _ when Cell.is_empty(cell) => fill
    | [] => [cell]
    | [hd, ...tl] =>
      if (Cell.has_space(cell)) {
        [Cell.pad(~onto=R, cell, hd), ...tl];
      } else if (Cell.has_space(hd)) {
        [Cell.pad(~onto=L, cell, hd), ...tl];
      } else {
        [cell, ...fill];
      }
    };
};
