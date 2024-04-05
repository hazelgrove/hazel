open Util;

let bake_eq =
    (~fill=[], sort: Bound.t(Molded.NT.t)): option(Rel.t(Cell.t, Cell.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let (l, r) = Walk.(height(w_l) > 2, height(w_r) > 2);
  let cell = Filling.fill(~l, fill, sort, ~r);
  Rel.Eq(cell);
};

let bake_lt =
    (~fill=[], bound: Bound.t(Molded.NT.t), sort: Bound.t(Molded.NT.t))
    : option(Rel.t(Cell.t, Cell.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ _w_l = ListUtil.hd_opt(Walker.enter(~from=L, bound, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let cell = Filling.fill(fill, sort, ~r=Walk.height(w_r) > 2);
  Rel.Neq(cell);
};

let bake_gt =
    (~fill=[], sort: Bound.t(Molded.NT.t), bound: Bound.t(Molded.NT.t))
    : option(Rel.t(Cell.t, Cell.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)))
  and+ _w_r = ListUtil.hd_opt(Walker.enter(~from=R, bound, Node(f_r)));
  let cell = Filling.fill(~l=Walk.height(w_l) > 2, fill, sort);
  Rel.Neq(cell);
};

let bake_swing =
    (~fill=Filling.empty, ~from: Dir.t, sw: Walk.Swing.t)
    : option(Rel.t(Cell.t, Cell.t)) => {
  let fill = Dir.pick(from, (Fun.id, List.rev), fill);
  switch (from) {
  | _ when Walk.Swing.height(sw) <= 1 => bake_eq(~fill, Walk.Swing.bot(sw))
  | L => bake_lt(~fill, Walk.Swing.top(sw), Walk.Swing.bot(sw))
  | R => bake_gt(~fill, Walk.Swing.bot(sw), Walk.Swing.top(sw))
  };
};

let bake = (~from: Dir.t, ~fill=Filling.empty, w: Walk.t): option(Baked.t) =>
  w
  |> Chain.map_link(((mtrl, mold)) => Token.mk(mtrl, mold))
  |> Chain.unzip
  // choose swing to fill that minimizes obligations.
  // currently simply chooses a single swing to fill even when there are
  // multiple fill elements. ideally this choice would distribute multiple
  // melds across multiple swings.
  |> Oblig.Delta.minimize(((pre, sw: Walk.Swing.t, suf)) => {
       open OptUtil.Syntax;
       let bake_tl = ((toks, strs)) =>
         List.combine(toks, strs)
         |> List.map(((tok, sw)) => {
              let+ cell = bake_swing(~from, sw);
              (tok, cell);
            })
         |> OptUtil.sequence
         |> Option.map(List.split);
       let+ cell = bake_swing(~fill, ~from, sw)
       and+ pre = bake_tl(pre)
       and+ suf = bake_tl(suf);
       Chain.zip(~pre, cell, ~suf);
     });
