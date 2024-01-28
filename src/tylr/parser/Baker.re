open Util;

let faces =
  fun
  | [] => Molded.Label.(space, space)
  | cs =>
    Cell.(face(~side=L, List.hd(cs)), face(~side=R, ListUtil.last(cs)));

let mtrl = (sort: Bound.t(Molded.Sort.t)) =>
  sort
  |> Bound.map(Molded.mtrl_)
  |> Bound.to_opt
  |> Option.value(~default=Mtrl.Sort.root);

let regrout = (~l=false, ~r=false, ~fill=[], sort: Mtrl.Sort.t) =>
  switch (fill) {
  | [] =>
    switch (sort) {
    | Space => Some(Cell.empty)
    | Grout
    | Tile(_) => Some(Cell.mk(~meld=Meld.mk_grout(sort), ()))
    }
  | [cell] when Cell.is_space(cell) =>
    switch (sort) {
    | Space => Some(cell)
    | Grout
    | Tile(_) => Some(Cell.mk(~meld=Meld.mk_grout(sort), ()))
    }
  | [_, ..._] =>
    let n = List.length(fill);
    let mid = List.init(n - 1, _ => Token.mk_grout(~l=true, sort, ~r=true));
    if (l && r) {
      let l = Token.mk_grout(sort, ~r=true);
      let r = Token.mk_grout(~l=true, sort);
      let meld = Meld.mk(Wald.mk([l] @ mid @ [r], fill));
      Some(Cell.mk(~meld, ()));
    } else if (l) {
      let l = Token.mk_grout(sort, ~r=true);
      let (fill, cell) = ListUtil.split_last(fill);
      let meld = Meld.mk(Wald.mk([l, ...mid], fill), ~r=cell);
      Some(Cell.mk(~meld, ()));
    } else if (r) {
      let r = Token.mk_grout(~l=true, sort);
      let (cell, fill) = ListUtil.split_first(fill);
      let meld = Meld.mk(~l=cell, Wald.mk(mid @ [r], fill));
      Some(Cell.mk(~meld, ()));
    } else if (n > 1) {
      let (l, fill) = ListUtil.split_first(fill);
      let (fill, r) = ListUtil.split_last(fill);
      let meld = Meld.mk(~l, Wald.mk(mid, fill), ~r);
      Some(Cell.mk(~meld, ()));
    } else {
      Some(List.hd(fill));
    };
  };

let bake_eq = (~fill=[], sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let* w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)));
  let* w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let (l, r) = Walk.(height(w_l) > 2, height(w_r) > 2);
  let+ cell = Cell.fill(~l, ~r, fill, sort);
  Rel.Eq(cell);
};

let bake_lt =
    (~fill=[], bound: Bound.t(Molded.Sort.t), sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let* _w_l = ListUtil.hd_opt(Walker.enter(~from=L, bound, Node(f_l)));
  let* w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let+ cell = Cell.fill(~r=Walk.height(w_r) > 2, fill, sort);
  Rel.Neq(cell);
};

let bake_gt =
    (~fill=[], sort: Bound.t(Molded.Sort.t), bound: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) = faces(fill);
  let* w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(l)));
  let* _w_r = ListUtil.hd_opt(Walker.enter(~from=R, bound, Node(r)));
  let+ cell = Cell.fill(~r=Walk.height(w_l) > 2, fill, sort);
  Rel.Neq(cell);
};

let bake_stride = (~fill=[], ~from: Dir.t, str: Walk.Stride.t) =>
  switch (from) {
  | _ when Walk.Stride.height(str) <= 1 =>
    bake_eq(~fill, Walk.Stride.bot(str))
  | L => bake_lt(~fill, Walk.Stride.top(str), Walk.Stride.bot(str))
  | R => bake_gt(~fill, Walk.Stride.bot(str), Walk.Stride.top(str))
  };

let bake = (~from: Dir.t, ~fill: list(Cell.t)=[], w: Walk.t): option(Bake.t) =>
  w
  |> Chain.map_link(Token.mk)
  |> Chain.unzip
  |> Oblig.Delta.minimize(((pre, str: Walk.Stride.t, suf)) => {
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
