open Util;

let bake_eq = (~fill=[], sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) =
    switch (fill) {
    | [] => Molded.Label.(space, space)
    | [_, ..._] =>
      Meld.(face(L, List.hd(fill)), face(R, ListUtil.last(fill)))
    };
  let+ w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, sort, Node(l)))
  and+ w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, sort, Node(r)));
  let (h_l, h_r) = (Walk.height(w_l) > 2, Walk.height(w_r) > 2);
  let (i_l, i_r) = Walk.(intermediates(w_l) > 0, intermediates(w_r) > 0);
  Cell.fill(~req=i_l || i_r, ~l=h_l, ~r=h_r, fill, sort);
};

let bake_lt = (~fill=[], bound: Bound.t(Molded.Sort.t), sort: Molded.Sort.t) => {
  open OptUtil.Syntax;
  let (l, r) =
    switch (fill) {
    | None => Molded.Label.(space, space)
    | Some(m) => Meld.faces(m)
    };
  let+ _w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, bound, Node(l)))
  and+ w_r =
    ListUtil.hd_opt(Walker.walk_into(~from=R, Node(sort), Node(r)));
  let h_r = Walk.height(w_r) > 2;
  let i_r = Walk.intermediates(w_r) > 0;
  Cell.fill(~req=i_r, ~r=h_r, fill, sort);
};

let bake_gt = (~fill=[], sort: Molded.Sort.t, bound: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) =
    switch (fill) {
    | None => Molded.Label.(space, space)
    | Some(m) => Meld.faces(m)
    };
  let+ w_l =
    ListUtil.hd_opt(Walker.walk_into(~from=L, Node(sort), Node(l)))
  and+ _w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, bound, Node(r)));
  let h_l = Walk.height(w_l) > 2;
  let i_l = Walk.intermediates(w_l) > 0;
  Cell.fill(~req=i_l, ~r=h_l, fill, sort);
};

let bake_stride =
    (~fill=[], ~from: Dir.t, str: Stride.t): option(Cell.t(Walk.Stride.t)) =>
  switch (from) {
  | _ when Stride.height(str) <= 1 => bake_eq(~fill, Stride.hd(stride))
  | L => bake_lt(~fill, Stride.hd(str), Stride.ft(str))
  | R => bake_gt(~fill, Stride.ft(str), Stride.hd(str))
  };

let bake = (~from: Dir.t, ~fill=[], w: Walk.t): option(Bake.t) =>
  w
  |> Chain.map_link(Token.mk)
  |> Chain.unzip
  |> Oblig.Delta.minimize(((pre, str: Walk.Stride.t, suf)) => {
       open OptUtil.Syntax;
       let bake_tl = tl =>
         tl
         |> List.map(((tok, str)) => {
              let+ cell = bake_stride(~from, str);
              (tok, cell);
            })
         |> OptUtil.sequence;
       let+ cell = bake_stride(~fill, ~from, str)
       and+ pre = bake_tl(pre)
       and+ suf = bake_tl(suf);
       Chain.zip(pre, cell, suf);
     });
