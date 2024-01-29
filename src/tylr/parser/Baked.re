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
        [Cell.pad(~side=L, ~pad=cell, hd), ...tl];
      } else if (Cell.has_space(hd)) {
        [Cell.pad(cell, ~pad=hd, ~side=R), ...tl];
      } else {
        [cell, ...fill];
      }
    };
  let init = cell => cons(cell, empty);
  let faces =
    fun
    | [] => Molded.Label.(space, space)
    | [hd, ..._] as fill =>
      Cell.(face(~side=L, hd), face(~side=R, ListUtil.last(fill)));
};
