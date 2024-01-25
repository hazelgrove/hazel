open Util;

let faces =
  fun
  | [] => Molded.Label.(space, space)
  | ms =>
    Meld.(face(~side=L, List.hd(ms)), face(~side=R, ListUtil.last(ms)));

let bake_eq = (~fill=[], sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let* w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, sort, Node(f_l)));
  let* w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, sort, Node(f_r)));
  let (l, r) = Walk.(height(w_l) > 2, height(w_r) > 2);
  let+ cell = Cell.fill(~l, ~r, fill, sort);
  Rel.Eq(cell);
};

let bake_lt =
    (~fill=[], bound: Bound.t(Molded.Sort.t), sort: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = faces(fill);
  let* _w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, bound, Node(f_l)));
  let* w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, sort, Node(f_r)));
  let+ cell = Cell.fill(~r=Walk.height(w_r) > 2, fill, sort);
  Rel.Neq(cell);
};

let bake_gt =
    (~fill=[], sort: Bound.t(Molded.Sort.t), bound: Bound.t(Molded.Sort.t)) => {
  open OptUtil.Syntax;
  let (l, r) = faces(fill);
  let* w_l = ListUtil.hd_opt(Walker.walk_into(~from=L, sort, Node(l)));
  let* _w_r = ListUtil.hd_opt(Walker.walk_into(~from=R, bound, Node(r)));
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

let bake = (~from: Dir.t, ~fill=[], w: Walk.t): option(Bake.t) =>
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
       Chain.zip(pre, cell, suf);
     });
