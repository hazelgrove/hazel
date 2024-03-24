open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let fold = Chain.fold_left;

let height = Chain.length;

let is_eq = ((rels, toks): t) =>
  rels
  |> List.map(Rel.is_eq)
  |> OptUtil.sequence
  |> Option.map(cells => (cells, toks));

module Fill = {
  type t = list(Meld.t);
  let empty = [];
  // todo: move somewhere better
  let rec pad = (~side as d: Dir.t, ~pad as p: Token.t, m: Meld.t) =>
    switch (Meld.is_space(m)) {
    | Some(spc) =>
      let (l, r) = Dir.order(d, (p, spc));
      let spc = Token.merge_text(l, r);
      Meld.of_space(spc);
    | None =>
      let M(l, w, r) = m;
      let (c_d, c_b) = Dir.order(d, (l, r));
      let put = Cell.put(c_d.mold, c_d.mtrl);
      let c_d =
        switch (Cell.get(c_d)) {
        | None => put(Meld.of_space(p))
        | Some(m) => put(pad(~side=d, ~pad=p, m))
        };
      let (l, r) = Dir.order(d, (c_d, c_b));
      Meld.M(l, w, r);
    };
  let cons = (~from: Dir.t, meld, fill) =>
    switch (fill) {
    // | _ when Cell.is_empty(cell) => fill
    | [] => [meld]
    | [hd, ...tl] =>
      let (l, r) = Dir.order(from, (meld, hd));
      switch (Meld.(is_space(l), is_space(r))) {
      | (Some(spc_l), Some(spc_r)) =>
        let spc = Token.merge_text(spc_l, spc_r);
        [Meld.of_space(spc), ...tl];
      | (Some(spc), _) => [pad(~side=L, ~pad=spc, r), ...tl]
      | (_, Some(spc)) => [pad(l, ~pad=spc, ~side=R), ...tl]
      | (None, None) => [meld, ...fill]
      };
    };
  // let init = cell => cons(cell, empty);
  let faces =
    fun
    | [] => ((Mtrl.Space, Mold.Space.of_t), (Mtrl.Space, Mold.Space.of_t))
    | [hd, ..._] as fill =>
      Meld.(face(~side=L, hd), face(~side=R, ListUtil.last(fill)));
};
