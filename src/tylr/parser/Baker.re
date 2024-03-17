open Util;

let faces =
  fun
  | [] => Molded.Labeled.(space, space)
  | cs =>
    Cell.(face(~side=L, List.hd(cs)), face(~side=R, ListUtil.last(cs)));

let mtrl = (sort: Bound.t(Molded.Sorted.t)) =>
  sort
  |> Bound.map(Molded.mtrl_)
  |> Bound.to_opt
  |> Option.value(~default=Mtrl.Sorted.root);

let fill_cell = (~l=false, ~r=false, ~fill=[], sort: Mtrl.Sorted.t) =>
  switch (fill) {
  | [] =>
    switch (sort) {
    | Space => Cell.empty
    | Grout
    | Tile(_) => Cell.put(Meld.mk_grout(sort))
    }
  | [cell] when Cell.has_space(cell) =>
    switch (sort) {
    | Space => cell
    | Grout
    | Tile(_) =>
      Cell.put(Meld.mk_grout(sort)) |> Cell.pad(~side=L, ~pad=cell)
    }
  | [_, ..._] =>
    let n = List.length(fill);
    let mid = List.init(n - 1, _ => Token.mk_grout(~l=true, sort, ~r=true));
    if (l && r) {
      let l = Token.mk_grout(sort, ~r=true);
      let r = Token.mk_grout(~l=true, sort);
      let meld = Meld.mk(Wald.mk([l] @ mid @ [r], fill));
      Cell.put(meld);
    } else if (l) {
      let l = Token.mk_grout(sort, ~r=true);
      let (fill, cell) = ListUtil.split_last(fill);
      let meld = Meld.mk(Wald.mk([l, ...mid], fill), ~r=cell);
      Cell.put(meld);
    } else if (r) {
      let r = Token.mk_grout(~l=true, sort);
      let (cell, fill) = ListUtil.split_first(fill);
      let meld = Meld.mk(~l=cell, Wald.mk(mid @ [r], fill));
      Cell.put(meld);
    } else if (n > 1) {
      let (l, fill) = ListUtil.split_first(fill);
      let (fill, r) = ListUtil.split_last(fill);
      let meld = Meld.mk(~l, Wald.mk(mid, fill), ~r);
      Cell.put(meld);
    } else {
      List.hd(fill);
    };
  };

let bake_eq = (~fill=[], sort: Bound.t(Molded.Sorted.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let (l, r) = Walk.(height(w_l) > 2, height(w_r) > 2);
  let cell = fill_cell(~l, ~r, ~fill, mtrl(sort));
  Rel.Eq(cell);
};

let bake_lt =
    (
      ~fill=[],
      bound: Bound.t(Molded.Sorted.t),
      sort: Bound.t(Molded.Sorted.t),
    ) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let+ _w_l = ListUtil.hd_opt(Walker.enter(~from=L, bound, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let cell = fill_cell(~r=Walk.height(w_r) > 2, ~fill, mtrl(sort));
  Rel.Neq(cell);
};

let bake_gt =
    (
      ~fill=[],
      sort: Bound.t(Molded.Sorted.t),
      bound: Bound.t(Molded.Sorted.t),
    ) => {
  open OptUtil.Syntax;
  let (l, r) = faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(l)))
  and+ _w_r = ListUtil.hd_opt(Walker.enter(~from=R, bound, Node(r)));
  let cell = fill_cell(~r=Walk.height(w_l) > 2, ~fill, mtrl(sort));
  Rel.Neq(cell);
};

let bake_stride = (~fill=[], ~from: Dir.t, str: Walk.Swing.t) => {
  let fill = Dir.pick(from, (Fun.id, List.rev), fill);
  switch (from) {
  | _ when Walk.Swing.height(str) <= 1 =>
    bake_eq(~fill, Walk.Swing.bot(str))
  | L => bake_lt(~fill, Walk.Swing.top(str), Walk.Swing.bot(str))
  | R => bake_gt(~fill, Walk.Swing.bot(str), Walk.Swing.top(str))
  };
};

let bake =
    (~from: Dir.t, ~fill: list(Cell.t)=[], w: Walk.t): option(Baked.t) =>
  w
  |> Chain.map_link(Token.mk)
  |> Chain.unzip
  |> Oblig.Delta.minimize(((pre, str: Walk.Swing.t, suf)) => {
       open OptUtil.Syntax;
       let bake_tl = ((toks, strs)) =>
         List.combine(toks, strs)
         |> List.map(((tok, str)) => {
              let+ cell = bake_stride(~from, str);
              (tok, cell);
            })
         |> OptUtil.sequence
         |> Option.map(List.split);
       let+ cell = bake_stride(~fill, ~from, str)
       and+ pre = bake_tl(pre)
       and+ suf = bake_tl(suf);
       Chain.zip(~pre, cell, ~suf);
     });
