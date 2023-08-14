exception Incomparable(Matter.m, Matter.m);

module Wald = {
  include Wald;
  let cmp =
      (l: p, ~kid=Meld.empty(), r: p)
      : Comparator.Result.t(Slope.Dn.p, Wald.p, Slope.Up.p) => {
    let m_l = Piece.mold(face(R, l));
    let m_r = Piece.mold(face(L, r));
    Comparator.cmp(m_l, ~kid=?Meld.sort(kid), m_r)
    |> OptUtil.get_or_raise(Incomparable(m_l, m_r))
    |> Comparator.Result.map(
         Slope.Dn.bake(~l, ~kid, ~r),
         Wald.bake(~l=l.wal, ~kid, ~r=r.wal),
         Slope.Up.bake(~l, ~kid, ~r),
       );
  };
};

module Slope = {
  module Dn = {
    include Slope.Dn;
    let rec push_wald =
            (dn: p, ~kid=Meld.empty(), w: Wald.p): Result.t(p, Meld.p) => {
      let kid = Meld.pad(~l=dn.space, kid);
      switch (dn.terrs) {
      | [] => Error(kid)
      | [hd, ...tl] =>
        let tl = Slope.Dn.mk(tl);
        // left-to-right: tl hd kid w
        switch (Wald.cmp(hd.wal, ~kid, w)) {
        | Lt(lt) =>
          let lt = Slope.map_top(Terrace.put_mel(hd.mel), lt);
          Ok(Slope.Dn.cat(tl, lt));
        | Eq(eq) => Ok(Slope.Dn.cons(tl, Terrace.put_mel(hd.mel, eq)))
        | Gt(gt) =>
          let (gt, _w) = Slope.Up.split_top(gt);
          // connectedness invariant ensures that gt has single terrace
          // combine this terrace with hd.mel to form new kid
          let kid = failwith("todo");
          push_wald(tl, ~kid, w);
        };
      };
    };
  };
  module Up = {
    include Slope.Up;
  };
};

module Stepwell = {
  exception Pushed_beyond_slopes;

  let push_space = (~onto: Dir.t, s) =>
    Stepwell.map_slopes(Slopes.push_space(~onto, s));

  // doesn't bother pushing beyond nearest slopes bc any pushed content
  // should have been molded by or melded to something in nearest prefix slope
  // (possibly put there by deconstructing some previously existing bridge)
  let push_wald = (~onto: Dir.t, w: Wald.t, well: Stepwell.t): Stepwell.t => {
    let (dn, up) = get_slopes(well);
    switch (onto) {
    | L =>
      let dn =
        Slope.Dn.push_wald(dn, w)
        |> Result.to_option
        |> OptUtil.get_or_raise(Pushed_beyond_slopes);
      Stepwell.put_slopes((dn, up), well);
    | R =>
      let dn =
        Slope.Up.push_wald(w, up)
        |> Result.to_option
        |> OptUtil.get_or_raise(Pushed_beyond_slopes);
      Stepwell.put_slopes((dn, up), well);
    };
  };

  let push_lexeme = (~onto: Dir.t, lx: Lexeme.t(Piece.t)) =>
    switch (lx) {
    | S(s) => push_space(~onto, s)
    | T(p) => push_wald(~onto, Wald.of_piece(p))
    };

  let rec bridge_slopes = ((l, r) as slopes, well) =>
    switch (Slope.Dn.uncons(l), Slope.Up.unsnoc(r)) {
    | (None, _)
    | (_, None) => push_slopes(slopes, well)
    | (Some((hd_l, tl_l)), Some((tl_r, hd_r))) =>
      switch (Terrace.cmp(hd_l, hd_r)) {
      | None => failwith("expected cmp")
      | Some(Eq(_)) =>
        well |> cons_bridge((hd_l, hd_r)) |> unzip_slopes((tl_l, tl_r))
      | Some(Lt(_)) =>
        well
        |> push_slopes(Slopes.mk(~l=Slope.of_terr(hd_l), ()))
        |> bridge_slopes((tl_l, r))
      | Some(Gt(_)) =>
        well
        |> push_slopes(Slopes.mk(~r=Slope.of_terr(hd_r), ()))
        |> bridge_slopes((l, tl_r))
      }
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
