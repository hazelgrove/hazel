module Piece = {
  include Piece;

  let zips = (l: Piece.t, r: Piece.t): option(Piece.t) => {
    open OptUtil.Syntax;
    let* () = OptUtil.of_bool(Id.eq(l.id, r.id));
    let+ l =
      switch (Piece.label(l), Piece.label(r)) {
      | (Grout (), Tile(_))
      | (Tile(_), Grout ()) => None
      | (Grout (), Grout ()) => Some(l)
      | (Tile(lbl_l), Tile(lbl_r)) =>
        let+ lbl = Label.zip(lbl_l, lbl_r);
        put_label(lbl, l);
      };
    l
    |> put_token(l.token ++ r.token)
    |> put_paths(l.paths @ List.map(Path.shift(token_length(l)), r.paths));
  };
};

module Wald = {
  include Wald;

  let rec lt = (l: p, ~slot=None, r: p): option(Slope.Dn.p) => {
    let (p_l, p_r) = Wald.(face(R, l), face(L, r));
    let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
    Comparator.lt(m_l, ~slot=Kid.profile(kid), m_r)
    |> Option.map(bake(~slot, ~face=r));
  }
  and gt = (l: p, ~slot=None, r: p): option(Slope.Up.p) => {
    let (p_l, p_r) = Wald.(face(R, l), face(L, r));
    let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
    Comparator.gt(m_l, ~slot=Kid.profile(kid), m_r)
    |> Option.map(bake(~face=l, ~slot));
  }
  and bake = (~slot, ~face, s: Slope.m): Slope.p => {
    // upgrades molded tokens to pieces
    // replaces top and bottom walds with top and bot
    // finds slot to insert kid and applies lt and gt with neighbors to complete
    failwith(
      "todo bake",
    );
  };

  let zips = (l: p, r: p): option(p) => {
    open OptUtil.Syntax;
    let+ p = Piece.zips(p_l, p_r);
    l
    |> Chain.fold_right(
         (p, kid) => link(p, ~slot),
         _ => put_face(~side=L, p, r),
       );
  };

  let rec eq = (l: p, ~slot=None, r: p): option(p) => {
    open OptUtil.Syntax;
    // check whether they zip
    let/ () = {
      let+ w = zips(l, r);
      assert(kid == None);
      w;
    };
    // check whether they eq
    let (p_l, p_r) = (face(~side=R, l), face(~side=L, r));
    let (m_l, m_r) = Piece.(molded(p_l, p_r));
    let/ () =
      Comparator.eq(m_l, ~slot, m_r)
      |> Option.map(bake(~l, ~slot, ~r))
      |> Option.map(w =>
           switch (Chain.unlink(r)) {
           | None => w
           | Some((_, kid, tl)) => Wald.append(w, ~slot, tl)
           }
         );
    // strip unfinished faces and try again
    switch (unknil(l), unlink(r)) {
    | (Some((tl, k, p)), _) when !Piece.is_finished(p) =>
      eq(tl, ~slot=Kid.merge(k, kid), r)
    | (_, Some((p, k, tl))) when !Piece.is_finished(p) =>
      eq(l, ~slot=Kid.merge(kid, k), tl)
    | _ => None
    };
  };

  // todo: rename meld
  let cmp = (l: p, ~slot=None, r: p): Ziggurat.p =>
    switch (lt(l, ~slot, p), eq(l, ~slot, p), gt(l, ~slot, p)) {
    | (_, Some(top), _) => Ziggurat.mk(top)
    | (Some(dn), _, _) => Ziggurat.mk(top, ~dn)
    | (_, _, Some(up)) => Ziggurat.mk(~up, top)
    | (None, None, None) => failwith("todo: incomparable meld")
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

module Ziggurat = {
  include Ziggurat;

  let rec hsup = (z: Ziggurat.p, ~slot=None, w: Wald.p): Ziggurat.p =>
    // todo: handle whitespace on z.dn
    switch (z.dn) {
    | [] =>
      let c = Wald.cmp(top, ~slot, w);
      switch (c.up) {
      | [] => {...c, up: z.up}
      | [_, ..._] => Ziggurat.map_up((@)(z.up), c)
      };
    | [hd, ...tl] =>
      // left to right: z.up z.top tl hd.mel hd.wal kid w
      let c = Wald.cmp(hd.wal, ~slot, w);
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

  let rec push = (w: Wald.p, ~slot=None, z: Ziggurat.p): Ziggurat.p =>
    // todo: handle whitespace on z.dn
    switch (z.up) {
    | [] =>
      let c = Wald.cmp(w, ~slot, top);
      switch (c.dn) {
      | [] => {...c, dn: z.dn}
      | [_, ..._] => Ziggurat.map_dn(dn' => dn' @ z.dn, c)
      };
    | [hd, ...tl] =>
      // left to right: w kid hd.wal hd.mel tl z.top z.dn
      let c = Wald.cmp(w, ~slot, hd.wal);
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
