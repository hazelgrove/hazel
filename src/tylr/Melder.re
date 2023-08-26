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
    failwith(
      "todo bake",
    );
  };

  let rec eq = (l: p, ~kid=None, r: p): option(p) => {
    open OptUtil.Syntax;
    let/ () =
      l
      |> Chain.fold_right(
           (p, kid) => Option.map(link(p, kid)),
           p_l => {
             let p_r = Wald.fst(r);
             let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
             Comparator.eq(m_l, ~kid=Kid.profile(kid), m_r)
             |> Option.map(bake(~l, ~kid, ~r))
             |> Option.map(w =>
                  switch (Chain.unlink(r)) {
                  | None => w
                  | Some((_, kid, tl)) => Wald.append(w, ~kid, tl)
                  }
                );
           },
         );
    switch (unknil(l), unlink(r)) {
    | (Some((tl, k, p)), _) when !Piece.is_finished(p) =>
      let kid = Kid.merge(k, kid);
      eq(tl, ~kid, r);
    | (_, Some((p, k, tl))) when !Piece.is_finished(p) =>
      let kid = Kid.merge(kid, k);
      eq(l, ~kid, tl);
    | _ => None
    };
  };

  let cmp = (l: p, ~kid=None, r: p): Ziggurat.p =>
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

module Ziggurat = {
  include Ziggurat;

  let rec hsup = (z: Ziggurat.p, ~kid=None, w: Wald.p): Ziggurat.p =>
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
      | [_, ..._] => hsup_zigg({...z, dn: tl}, c)
      };
    }
  and hsup_zigg = (z: Ziggurat.p, {up, top, dn}: Ziggurat.p) =>
    Slope.Up.to_walds(up)
    @ [top]
    |> List.fold_left((z, w) => hsup(z, w), z)
    |> Ziggurat.map_dn(Slope.cat(dn));

  let rec push = (w: Wald.p, ~kid=None, z: Ziggurat.p): Ziggurat.p =>
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
      | [_, ..._] => push_zigg(c, {...z, up: tl})
      };
    }
  and push_zigg = ({up, top, dn}: Ziggurat.p, z: Ziggurat.p) =>
    List.fold_right(
      (w, z) => push(w, z),
      [top, ...Slope.Dn.to_walds(dn)],
      z,
    )
    |> Ziggurat.map_up(Slope.cat(up));
};

// module Slope = {
//   include Slope;
//   module Dn = {
//     include Slope.Dn;
//     let push = (dn: p, w: Wald.p):
//   }
// }

module Stepwell = {
  include Stepwell;
  let push = (~onto: Dir.t, w: Wald.t, well: t): t =>
    switch (unlink(well)) {
    | Error((dn, up)) =>
      let root = failwith("todo: mk synthetic root wald");
      switch (onto) {
      | L =>
        let z = Ziggurat.(hsup(mk(root, ~dn), w));
        assert(Slope.is_empty(z.up));
        assert(Wald.eq(root, z.top));
        singleton((z.dn, up));
      | R =>
        let z = Ziggurat.(push(w, mk(~up, root)));
        assert(Wald.eq(root, z.top));
        assert(Slope.is_empty(z.dn));
        singleton((dn, z.up));
      };
    | Ok(((dn, up), (l, r), well)) =>
      switch (onto) {
      | L =>
        let z = Ziggurat.(hsup(mk(l, ~dn), w));
        switch (z.up) {
        | [] when Wald.eq(z.top, l) =>
          // lt root
          link((z.dn, up), (z.top, r), well)
        | _ =>
          well
          |> push_zigg(~onto=L, z)
          |> push_zigg(~onto=R, Ziggurat.mk(~up, r))
        };
      }
    }
  and push_zigg = (~onto: Dir.t, z: Ziggurat.p, well: t): t =>
    switch (onto) {
    | L =>
      Slope.Up.to_walds(z.up)
      @ [z.top]
      |> List.fold_left((well, w) => hsup(~onto, w, well), well)
      |> map_slopes(((dn, up)) => (Slope.cat(z.dn, dn), up))
    | R =>
      List.fold_right(
        (w, well) => push(~onto, w, well),
        [z.top, ...Slope.Dn.to_walds(z.dn)],
        well,
      )
      |> map_slopes(((dn, up)) => (dn, Slope.cat(z.up, up)))
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
