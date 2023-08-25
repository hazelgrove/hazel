exception Incomparable(Material.molded, Material.molded);

exception Empty_slope;

module Piece = {
  include Piece;

  let replace = (l: t, r: t): option(t) => {
    let replaced_l = add_paths(List.map(_ => 0, l.paths), r);
    let replaced_r = add_paths(List.map(_ => length(l), r.paths), l);
    switch (l.material, r.material) {
    | (Grout((l, _)), Grout((r, _))) when Tip.consistent(l, r) =>
      Some(replaced_l)
    | (Grout((_, l)), Grout((_, r))) when Tip.consistent(l, r) =>
      Some(replaced_r)
    | (Grout((l, _)), Tile(m)) when Mold.consistent(~l, m) =>
      Some(replaced_l)
    | (Tile(m), Grout((_, r))) when Mold.consistent(m, ~r) =>
      Some(replaced_r)
    | (Tile(m), Tile(r)) when !is_finished(l) && Mold.eq(m, r) =>
      Some(replaced_l)
    | (Tile(l), Tile(m)) when Mold.eq(l, m) && !is_finished(r) =>
      Some(replaced_r)
    | _ => None
    };
  };

  let merge = (l: t, r: t): option(t) =>
    switch (l.material, r.material) {
    | (Grout((l, _)), Grout((_, r))) =>
      Some({...l, material: Grout((l, r)), token: l.token ++ r.token})
    | _ => None
    };

  let fuse = (l: t, r: t): option(t) => {
    open OptUtil.Syntax;
    let/ () = zip(l, r);
    let/ () = replace(l, r);
    merge(l, r);
  };
};

module Wald = {
  include Wald;

  let rec lt = (l: p, ~kid=None, r: p): option(Slope.Dn.p) => {
    let (p_l, p_r) = Wald.(face(R, l), face(L, r));
    let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
    Comparator.lt(m_l, ~kid=Kid.profile(kid), m_r)
    |> Option.map(bake(~top=l, ~kid, ~bot=r));
  }
  and gt = (l: p, ~kid=None, r: p): option(Slope.Up.p) => {
    let (p_l, p_r) = Wald.(face(R, l), face(L, r));
    let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
    Comparator.gt(m_l, ~kid=Kid.profile(kid), m_r)
    |> Option.map(bake(~bot=l, ~kid, ~top=r));
  }
  and bake = (~top, ~kid, ~bot, s: Slope.m): Slope.p => {
    // upgrades molded tokens to pieces
    // replaces top and bottom walds with top and bot
    // finds slot to insert kid and applies lt and gt with neighbors to complete
    failwith("todo bake");
  };


  let eq = (l: p, ~kid=None, r: p): option(p) =>
    l
    |> Chain.fold_right(
         (p, kid) => Option.map(link(p, kid)),
         p_l => {
          let p_r = Wald.fst(r);
          let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
           Comparator.eq(m_l, ~kid=Kid.profile(kid), m_r)
           |> Option.map(Wald.bake(~l, ~lt, ~kid, ~gt, ~r))
           |> Option.map(w =>
             switch (Chain.unlink(r)) {
             | None => w
             | Some((_, kid, tl)) => Wald.append(w, ~kid, tl)
             }
           )
         },
       );

  let cmp =
      (l: p, ~kid=None, r: p): Ziggurat.p =>
    switch (lt(l, ~kid, p), eq(l, ~kid, p), gt(l, ~kid, p)) {
    | (_, Some(eq), _) => Ziggurat.mk(eq)
    | (Some(lt), _, _) => Lt(lt)
    | (_, _, Some(gt)) => Gt(gt)
    | (None, None, None) => In
    };
};

// [(] [1 *] [2]     _     [let x =]
//                  inc
// [(] [1 *]         2     [><] [let x =]
//                   gt
// [let y =]       1 * 2   [><] [let x =]
//                   lt

// complete single-line core:
// rework types to handle incomparability
// restore walks
// modify grammar stepping to account for grout
// do some character-level plumbing for pulling/pushing characters
// adjust to maintain meld whitespace model (fix later)

// 2d core:
// pass over measured
// add 2d movement
// other QoL stuff like move to next hole

// rest of system:
// rework transformation to view
// rework make term

// module Terrace = {
//   include Terrace;

//   let lt = (l: R.p, ~kid=None, r: L.p): option((Slope.Dn.p, Kid.p)) =>
//     Wald.lt(l.wal, ~kid, r.wal)
//     |> Option.map(Slope.map_top(Terrace.put_mel(l.mel)))
//     |> Option.map(s => (s, r.mel));

//   let gt = (l: R.p, ~kid=None, r: L.p): option((Kid.p, Slope.Up.p)) =>
//     Wald.gt(l.wal, ~kid, r.wal)
//     |> Option.map(Slope.map_top(Terrace.put_mel(r.mel)))
//     |> Option.map(s => (l.mel, s));

//   let eq = (l: R.p, ~kid=None, r: L.p): option((Meld.p, Wald.p, Meld.p)) =>
//     Wald.eq(l.wal, ~kid, r.wal)
//     |> Option.map(w => (l.mel, w, r.mel));

//   module L = {
//     include L;
//     let patch = (t: p): Slope.Dn.p
//   }

//   // let cmp =
//   //     (l: R.p, ~kid=None, r: L.p): Comparator.Result.t()
// };

let rec hsup =
    (z: Ziggurat.p, ~kid=None, w: Wald.p): Ziggurat.p =>
  // todo: handle whitespace on z.dn
  switch (z.dn) {
  | [] =>
    let c = Wald.cmp(top, ~kid, w);
    switch (c.up) {
    | [] => {...c, up: z.up}
    | [_, ..._] => Ziggurat.map_up((@)(z.up), c)
    };
  | [hd, ...tl] =>
    // left to right: z.up z.top tl hd.mel hd.wal kid w
    let c = Wald.cmp(hd.wal, ~kid, w);
    // left to right: z.up z.top tl hd.mel c.up c.top c.dn
    switch (c.up) {
    | [] =>
      let dn = c.dn @ [{...hd, wal: c.top}, ...tl];
      {...z, dn};
    | [_, ..._] =>
      let z_tl = {...z, dn: tl};
      Slope.Up.to_walds(c.up) @ [top]
      |> List.fold_left((z, w) => hsup_wald(z, w), z_tl)
      |> Ziggurat.map_dn(dn' => dn' @ dn)
    }
  };

let rec push =
    (w: Wald.p, ~kid=None, z: Ziggurat.p): Ziggurat.p =>
  // todo: handle whitespace on z.dn
  switch (z.up) {
  | [] =>
    let c = Wald.cmp(w, ~kid, top);
    switch (c.dn) {
    | [] => {...c, dn: z.dn}
    | [_, ..._] => Ziggurat.map_dn(dn' => dn' @ z.dn, c)
    };
  | [hd, ...tl] =>
    // left to right: w kid hd.wal hd.mel tl z.top z.dn
    let c = Wald.cmp(w, ~kid, hd.wal);
    // left to right: c.up c.top c.dn hd.mel tl z.top z.dn
    switch (c.dn) {
    | [] =>
      let up = c.up @ [{...hd, wal: c.top}, ...tl];
      {...z, up};
    | [_, ..._] =>
      let z_tl = {...z, up: tl};
      List.fold_right(
        (w, z) => push(w, z),
        [top, ...Slope.Dn.to_walds(dn)],
        z_tl,
      )
      |> Ziggurat.map_up(up' => dn' @ dn)
    }
  };

module Slope = {
  module Dn = {
    include Slope.Dn;

    let rec push_terr = (dn: p, ~kid=None, t: Terrace.L.p): Result.t(p, Meld.p) => {
      let kid = Meld.pad(~l=dn.space, kid);
      switch (dn.terrs) {
      | [] => Error(kid)
      | [hd, ...tl] =>
        switch (Terrace.cmp(hd, ~kid, t)) {
        | In =>
          let g = Terrace.mk(Wald.singleton(g, failwith("mk concave grout piece")));

          let (s, m) =
            Terrace.lt(g, t)
            |> OptUtil.get_or_fail("think incomparability enforces this but not 100% confident");
          let
            |> Slope.top
            |> OptUtil.get_or_fail(Empty_slope);
          push_terr(dn, ~kid, t);
        | Lt(lt) =>

        }
      }
    };



    let rec push_wald =
            (dn: p, ~kid=Meld.empty(), ~in_=false, w: Wald.p): Result.t(p, Meld.p) => {
      let kid = Meld.pad(~l=dn.space, kid);
      switch (dn.terrs) {
      | [] => Error(kid)
      | [hd, ...tl] when in_ =>
        if (Comparator.has_match(R, Terrace.face(Piece.molded(r)))) {

        } else {
        }


        let tl = Slope.Dn.mk(tl);
        // left-to-right: tl hd kid w
        switch (Wald.cmp(hd.wal, ~kid, w)) {
        | In(kid) =>
          In()

        }


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
