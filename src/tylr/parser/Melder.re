open Util;

open Walker;

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

let bake_stride = (~fill=[], ~from: Dir.t, str: Stride.t): option(Cell.t) =>
  switch (from) {
  | _ when Stride.height(str) <= 1 => bake_eq(~fill, Stride.hd(stride))
  | L => bake_lt(~fill, Stride.hd(str), Stride.ft(str))
  | R => bake_gt(~fill, Stride.ft(str), Stride.hd(str))
  };

let bake =
    (~from: Dir.t, ~fill=[], w: Walk.t)
    : option(Chain.t(Cell.t(Stride.t), Token.t)) =>
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

module Wald = {
  let rec meld =
          (~from: Dir.t, l: Wald.t, ~fill=[], r: Wald.t)
          : option((Wald.t, Baked.t)) => {
    let (src, dst) = Dir.choose(from, l, r);
    let fill = Dir.choose(from, fill, List.rev(fill));
    let rec go = (~init=false, src, fill) => {
      open OptUtil.Syntax;
      let/ () =
        // try removing ghost
        switch (Wald.unlink(src)) {
        | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
          let fill = Option.to_list(cell.content) @ fill;
          switch (go(tl, fill)) {
          | Some((_, baked)) as r when Baked.height(baked) == 1 =>
            Effect.perform(Remove(hd));
            r;
          | _ => None
          };
        | _ => None
        };
      let baked =
        walk(~from, Wald.face(src).mtrl, Wald.face(dst).mtrl)
        |> Oblig.Delta.minimize(bake(~fill));
      return((src, baked));
    };
    go(~init=true, src, fill);
  };
};

module Terr = {
  module R = {
    include Terr.R;

    let connect = (t: Terr.R.t, baked) =>
      baked
      |> Chain.fold_left(
           cell => Meld.M(t.cell, t.wald, cell),
           (meld, tok, cell) => Meld.link(cell, tok, meld),
         )
      |> Meld.rev;

    let round = (~fill=[], terr: Terr.R.t) => {
      let exited = Walker.exit(R, Terr.face(terr));
      switch (Oblig.Delta.minimize(bake(~fill), exited)) {
      | Some(baked) => [connect(terr, baked)]
      | None =>
        let exited =
          ListUtil.hd_opt(exited)
          |> OptUtil.get_or_fail("bug: expected at least one exit");
        let baked =
          bake(exited)
          |> OptUtil.get_or_fail(
               "bug: bake expected to succeed if no fill required",
             );
        [connect(terr, baked), ...fill];
      };
    };
  };
};

module Slope = {
  module Dn = {
    let rec meld =
            (~top=Bound.Root, dn: Slope.Dn.t, ~fill=[], w: Wald.t)
            : Result.t(Slope.Dn.t, list(Meld.t)) =>
      switch (dn) {
      | [] =>
        open Result.Syntax;
        let* walked =
          Walker.walk_neq(~from=L, Root, Wald.face(w).mtrl)
          |> Result.of_option(~error=fill);
        let+ baked = bake(~fill, walked) |> Result.of_option(~error=fill);
        connect([], baked, w);
      | [hd, ...tl] =>
        switch (Wald.meld(~from=L, hd.wald, ~fill, w)) {
        | None => meld(~top, tl, ~fill=Terr.R.round(hd, ~fill), w)
        | Some((wald, baked)) =>
          Ok(connect([{...hd, wald}, ...tl], baked, w))
        }
      };
  };
};

module Ctx = {
  open OptUtil.Syntax;

  let push =
      (~onto: Dir.t, w: Wald.t, ~cell=Cell.empty, ctx: Ctx.t): option(Ctx.t) =>
    switch (onto, Ctx.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ dn = Slope.Dn.push(dn, ~slot, w);
      Ctx.unit((dn, up));
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~cell, w)) {
      | Some(dn) => Some(Ctx.unit((dn, up)))
      | None =>
        let+ w = Wald.eq(l.wald, ~cell=Slope.Dn.roll(dn), w);
        let (dn, up) = ([{...l, wald: w}], [r, ...up]);
        Ctx.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    | (R, _) => failwith("todo: symmetric to L")
    };

  let bridge = (~sel=Ziggurat.empty, well: Stepwell.t): Stepwell.t => {
    print_endline("Stepwell.assemble");
    let (pre, suf) = get_slopes(well);
    // separate siblings that belong to the selection
    let (pre_lt_sel, pre_geq_sel) = Ziggurat.split_lt(pre, sel);
    let (sel_leq_suf, sel_gt_suf) = Ziggurat.split_gt(sel, suf);
    well
    |> Stepwell.put_slopes(Slopes.empty)
    |> bridge_slopes((pre_lt_sel, sel_gt_suf))
    |> push_slopes((pre_geq_sel, sel_leq_suf));
  };

  let pull_lexeme = (~char=false, ~from: Dir.t, well: Stepwell.t) =>
    switch (Slopes.pull_lexeme(~char, ~from, Stepwell.get_slopes(well))) {
    | Some((a, sib)) => Some((a, Stepwell.put_slopes(sib, well)))
    | None =>
      open OptUtil.Syntax;
      let+ (slopes, bridge, well) = Chain.unlink(well);
      let (lx, slopes') = Bridge.pull_lexeme(~char, ~from, bridge);
      let well =
        well |> push_slopes(Slopes.cat(slopes, slopes')) |> assemble;
      (lx, well);
    };
};
