open Util;

type t = list(Meld.t);

let empty = [];
let is_empty: t => bool = (==)(empty);

let faces =
  fun
  | [] => Space.Molded.(t, t)
  | [hd, ..._] as fill =>
    Meld.(face(~side=L, hd), face(~side=R, ListUtil.last(fill)));

let squash = _ => failwith("todo: squash");

let get_space =
  fun
  | [] => Some(Space.Token.empty)
  | [m] => Space.Meld.get(m)
  | [_, ..._] => None;

let rec pad_meld = (~side as d: Dir.t, spc: Token.t, m: Meld.t) =>
  switch (Space.Meld.get(m)) {
  | Some(spc') =>
    let (l, r) = Dir.order(d, (spc, spc'));
    let spc = Token.merge_text(l, r);
    Space.Meld.mk(spc);
  | None =>
    let M(l, w, r) = m;
    let (c_d, c_b) = Dir.order(d, (l, r));
    let c_d = pad_cell(~side=d, spc, c_d);
    let (l, r) = Dir.order(d, (c_d, c_b));
    Meld.M(l, w, r);
  }
and pad_cell = (~side: Dir.t, spc: Token.t, c: Cell.t) => {
  let m =
    switch (Cell.get(c)) {
    | None => Space.Meld.mk(spc)
    | Some(m) => pad_meld(~side, spc, m)
    };
  Cell.put(~padding=c.padding, m);
};

let mtrl = (nt: Bound.t(Molded.NT.t)) =>
  nt |> Bound.map(Molded.NT.mtrl) |> Bound.get(~root=Mtrl.Sorted.root);
let padding = (nt: Bound.t(Molded.NT.t)) =>
  nt |> Bound.map(Molded.NT.padding) |> Bound.get(~root=Padding.root);

let fill_default = (nt: Bound.t(Molded.NT.t)) =>
  switch (mtrl(nt)) {
  | Space => Cell.empty
  | (Grout | Tile(_)) as s =>
    Cell.put(~padding=padding(nt), Grout.Meld.op_(s))
  };

// assumes precedence-correctness already checked
let fill = (~l=false, ~r=false, fill: t, nt: Bound.t(Molded.NT.t)): Cell.t => {
  let fill = squash(fill);
  switch (get_space(fill)) {
  | Some(spc) => fill_default(nt) |> pad_cell(~side=L, spc)
  | None =>
    assert(!is_empty(fill));
    let s = mtrl(nt);
    let cells =
      [
        l ? [Cell.empty] : [],
        List.map(Grout.Cell.put, fill),
        r ? [Cell.empty] : [],
      ]
      |> List.concat;
    let toks =
      Grout.Token.[
        l ? [pre(s)] : [],
        List.init(List.length(fill) - 1, _ => in_(s)),
        r ? [pos(s)] : [],
      ]
      |> List.concat;
    let meld =
      switch (toks) {
      | [] => Option.get(Cell.get(List.hd(cells)))
      | [_, ..._] =>
        let (l, cells) = ListUtil.split_first(cells);
        let (cells, r) = ListUtil.split_last(cells);
        Meld.M(l, Wald.mk(toks, cells), r);
      };
    Cell.put(~padding=padding(nt), meld);
  };
};
