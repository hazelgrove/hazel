open Util;

let put = failwith("todo: clear empty meld");

let bake_cell = failwith("todo: make default cell given molded nt");

// assumes precedence-correctness already checked
let fill_cell = (~l=false, ~r=false, ~fill=[], sort: Bound.t(Molded.NT.t)) => {
  let mtrl =
    sort
    |> Bound.map(fst)
    |> Bound.map(snd)
    |> Bound.get(~root=Mtrl.Sorted.root);
  let mold = Bound.map(snd, sort);
  let empty = Cell.empty(mold, mtrl);
  let put = put(mold, mtrl);

  switch (merge_spaces(fill)) {
  | Some(spc) => bake_cell(mold, mtrl) |> Cell.pad(~side=L, ~pad=spc)
  | None =>
    assert(fill != []);
    let s = failwith("todo");
    let (hd, tl) = ListUtil.split_first(fill);
    let toks = List.map(_ => Grout.Token.in_(s), tl);
    let cells = Grout.Cell.[put_hd(hd), ...List.map(put_tl, tl)];
    let (cells, toks) =
      l
        ? ([Grout.Cell.pad_l, ...cells], [Grout.Token.pre, ...toks])
        : (cells, toks);
    let (cells, toks) =
      r ? (cells @ [Grout.Cell.pad_r], toks @ [pad_r]) : (cells, toks);
    switch (cells) {
    | [] => failwith("bug: expected fill non-empty")
    | [c] => put(Cell.get(c))
    | [hd, ...tl] =>
      let (mid, ft) = ListUtil.split_last(tl);
      put(M(hd, W((toks, mid)), ft));
    };
  };
};

let bake_eq = (~fill=[], sort: Bound.t(Molded.NT.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let (l, r) = Walk.(height(w_l) > 2, height(w_r) > 2);
  let cell = fill_cell(~l, ~r, ~fill, sort);
  Rel.Eq(cell);
};

let bake_lt =
    (~fill=[], bound: Bound.t(Molded.NT.t), sort: Bound.t(Molded.NT.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ _w_l = ListUtil.hd_opt(Walker.enter(~from=L, bound, Node(f_l)))
  and+ w_r = ListUtil.hd_opt(Walker.enter(~from=R, sort, Node(f_r)));
  let cell = fill_cell(~r=Walk.height(w_r) > 2, ~fill, sort);
  Rel.Neq(cell);
};

let bake_gt =
    (~fill=[], sort: Bound.t(Molded.NT.t), bound: Bound.t(Molded.NT.t)) => {
  open OptUtil.Syntax;
  let (f_l, f_r) = Filling.faces(fill);
  let+ w_l = ListUtil.hd_opt(Walker.enter(~from=L, sort, Node(f_l)))
  and+ _w_r = ListUtil.hd_opt(Walker.enter(~from=R, bound, Node(f_r)));
  let cell = fill_cell(~l=Walk.height(w_l) > 2, ~fill, sort);
  Rel.Neq(cell);
};

let bake_swing = (~fill=Filling.empty, ~from: Dir.t, sw: Walk.Swing.t) => {
  let fill = Dir.pick(from, (Fun.id, List.rev), fill);
  switch (from) {
  | _ when Walk.Swing.height(sw) <= 1 => bake_eq(~fill, Walk.Swing.bot(sw))
  | L => bake_lt(~fill, Walk.Swing.top(sw), Walk.Swing.bot(sw))
  | R => bake_gt(~fill, Walk.Swing.bot(sw), Walk.Swing.top(sw))
  };
};

let bake = (~from: Dir.t, ~fill=Filling.empty, w: Walk.t): option(Baked.t) =>
  w
  |> Chain.map_link(Token.mk)
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
