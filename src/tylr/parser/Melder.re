open Util;

let zip = (~from: Dir.t, l: Wald.t, r: Wald.t): option(Wald.t) => {
  let (src, dst) = Dir.choose(from, l, r);
  let (hd_src, tl_src) = Wald.split_hd(src);
  let (hd_dst, tl_dst) = Wald.split_hd(dst);
  let (hd_l, hd_r) = Dir.choose(from, hd_src, hd_dst);
  Token.zip(hd_l, hd_r)
  |> Option.map(z => Wald.zip(tl_dst, z, tl_src));
};

let walk = (~from, src, dst) => {
  let (src, dst) = (Wald.hd(src), Wald.hd(dst));
  Walker.walk(~from, Node(src))
  |> Walker.Index.find(dst);
};

let meld = (~from: Dir.t, l: Wald.t, ~fill=[], r: Wald.t): option(Chain.t(Cell.t, Token.t)) => {
  let (src, dst) = Dir.choose(from, l, r);
  let fill = Dir.choose(from, fill, List.rev(fill));
  let rec go = (~init=false, src, fill) => {
    open OptUtil.Syntax;
    let/ () =
      // try removing ghost
      switch (Wald.unlink(src)) {
      | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
        let fill = Option.to_list(cell.content) @ fill;
        go(tl, fill) |> Effects.perform_if(Remove(hd))
      | _ => None
      };
    walk(~from, src, dst)
    // require eq if ghost has been removed
    |> (init ? Fun.id : List.filter(Walk.is_eq))
    |> Oblig.Delta.minimize(bake(~fill));
  };
  go(~init=true, src, fill);
};

module Wald = {
  let lt =
      (l: Bound.t(Wald.t), ~cell=Cell.empty, r: Wald.t): option(Slope.Dn.t) => {
    let hd_l = l |> Bound.map(Wald.hd) |> Bound.map(Token.mtrl_);
    let hd_r = Wald.hd(r).mtrl;
    Walker.lt(hd_l, hd_r)
    |> Oblig.Delta.minimize(Slope.Dn.bake(~face=r, ~fill=cell));
  };

  let gt =
      (l: Wald.t, ~cell=Cell.empty, r: Bound.t(Wald.t)): option(Slope.Up.t) => {
    let hd_l = Wald.hd(l).mtrl;
    let hd_r = r |> Bound.map(Wald.hd) |> Bound.map(Token.mtrl_);
    Walker.gt(hd_l, hd_r)
    |> Oblig.Delta.minimize(Slope.Up.bake(~face=l, ~fill=cell));
  };

  let rec eq = (~merge, ~onto: Dir.t, l: Wald.t, ~cell=Cell.empty, r: Wald.t) => {
    open OptUtil.Syntax;
    let (hd_l, tl_l) = Wald.split_hd(l);
    let (hd_r, tl_r) = Wald.split_hd(r);
    let (w_onto, w_from) = Dir.choose(onto, l, r);
    // try zipping
    let/ () = {
      let+ zipped = Token.zip(hd_l, hd_r);
      Wald.unit(zipped)
      |> Fun.flip(Wald.append, tl_foot)
      |> Wald.prepend(tl_face);
    };
    // try passing
    let/ () =
      switch (onto, tl_l, tl_r) {
      | (L, ([c, ...cs], ts), _) when Token.passes(onto, hd_l, hd_r) =>
        let l = Wald.mk(ts, cs);
        let cell = merge(c, cell);
        eq(~merge, ~onto, l, ~cell, r) |> Effects.perform_if(Remove(hd_l));
      | (R, _, ([c, ...cs], ts)) when Token.passes(onto, hd_l, hd_r) =>
        let r = Wald.mk(ts, cs);
        let cell = merge(cell, c);
        eq(~merge, ~onto, l, ~cell, r) |> Effects.perform_if(Remove(hd_r));
      | _ => None
      };
    // try walking
    Walker.eq(~from=onto, hd_l.mtrl, hd_r.mtrl)
    |> Oblig.Delta.minimize(
         Wald.bake(~face=w_from, ~fill=cell, ~foot=w_onto),
       );
  };
};

module Slope = {
  module Dn = {
    let rec push_wald =
            (~top=Bound.Root, dn: Slope.Dn.t, ~cell=Cell.empty, w: Wald.t)
            : Result.t(Slope.Dn.t, Cell.t) =>
      switch (dn) {
      | [] => Wald.lt(top, ~cell, w) |> Result.of_option(~error=cell)
      | [hd, ...tl] =>
        switch (
          Wald.eq(hd.wald, ~cell, w),
          Wald.lt(Node(hd.wald), ~cell, w),
          Wald.gt(hd.wald, ~cell, Node(w)),
        ) {
        | (Some(eq), _, _) => Ok([{...hd, wald: eq}, ...tl])
        | (_, Some(lt), _) => Ok(Slope.Dn.cat(dn, lt))
        | (_, _, Some(gt)) =>
          let cell = Slope.Up.roll(~cell=hd.cell, gt);
          push_wald(~top, tl, ~cell, w);
        | (None, None, None) =>
          open Result.Syntax;
          let g = Token.mk(Grout((Concave, Concave)));
          let* dn = push_wald(~top, dn, ~cell, Wald.unit(g));
          push_wald(~top, dn, w);
        }
      };

    let push = (~top=Bound.Root, dn, ~cell=Cell.empty, t) =>
      push_wald(~top, dn, ~cell, Wald.unit(t));
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
