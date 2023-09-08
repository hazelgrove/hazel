module Piece = {
  include Piece;

  let zip = (l: Piece.t, r: Piece.t): option(Piece.t) => {
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

  let cmp = (l: Piece.t, ~slot, r: Piece.t) => {
    let (m_l, m_r) = (l.material, r.material);
    switch (Comparator.cmp(m_l, ~slot=Slot.Baked.sort(slot), m_r)) {
    // comparable
    | Some(zigg) => Ziggurat.bake(~l, ~slot, ~r, zigg)
    // incomparable
    | None =>
      let up =
        [Comparator.walk_gt(R, m_l)]
        |> Terrace.bake(~face=l, ~slot)
        |> Option.;
      let terr_r = Comparator.walk_gt(L, m_r) |> Terrace.bake(~face=r);
      if (Mold.(is_convex(R, m_l) && is_convex(L, m_r))) {
        assert(Slot.is_empty(slot));
        let g = Piece.mk(Grout((Concave, Concave)));
        Ziggurat.singleton(~up=[terr_l], g, ~dn=[terr_r]);
      } else if (Mold.is_convex(R, m_l)) {
        assert(Slot.is_empty(slot));
        let g = Piece.mk(Grout((Concave, Convex)));
        Ziggurat.singleton(~up=[terr_l, Terrace.singleton(g)], r)
      } else if (Mold.is_convex(R, m_r)) {
        assert(Slot.is_empty(slot));
        let g = Piece.mk(Grout((Convex, Concave)));
        Ziggurat.singleton(l, ~dn=[Terrace.singleton(g), terr_r]);
      } else {
        let up =
          Comparator.walk_gt(R, m_l)
          |> Terrace.bake(~face=l, ~slot)
          |>

      }
    };
  };

  let regrout = (l: Piece.t, ~slot, r: Piece.t) =>
    switch (slot) {
    | Empty =>
      switch (l.material, r.material) {
      | (Grout((tip_l, _)), Tile(m)) =>
        let tip_r = Mold.is_convex(L, m) ? Concave : Convex;
        ({...l, material: Grout((tip_l, tip_r))}, r);
      | (Tile(m), Grout((_, tip_r))) =>
        let tip_l = Mold.is_convex(R, m) ? Concave : Convex;
        (l, {...r, material: Grout((tip_l, tip_r))});
      }
    | Full(_) =>
      let l =
        switch (l.material) {
        | Grout((t, _)) => {...l, material: Grout((t, Concave))}
        | Tile(_) => l
        };
      let r =
        switch (r.material) {
        | Grout((_, t)) => {...r, material: Grout((Concave, t))}
        | Tile(_) => r
        };
      (l, r);
    };

  let merge = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t) =>
    switch (l.material, slot, r.material) {
    | (Grout((tip_l, _)), Empty, Grout((_, tip_r))) =>
      // merge grout (probably handled entirely by relexing)
      let mold = Material.Grout((tip_l, tip_r));
      let token = l.token ++ r.token;
      Some(Ziggurat.singleton({...l, mold, token}));
    | _ => None
    };

  let replace = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t) =>
    switch (l.material, Slot.Baked.has_no_tiles(slot), r.material) {
    | (Tile(m_l), Some(t), Tile(m_r))
        when Token.is_empty(l.token) && Mold.eq(m_l, m_r) =>
      if (Mold.is_convex(L, m_r)) {
        let g = Piece.mk(~token=t, Grout((Convex, Concave)));
        Some(Ziggurat.singleton(g, ~dn=[Terrace.singleton(r)]));
      } else {
        let g = Piece.mk(~token=t, Grout((Concave, Convex)));
        Some(Ziggurat.singleton(~up=[Terrace.singleton(g)], r));
      };
    | (Tile(m_l), Some(t), Tile(m_r))
        when Token.is_empty(r.token) && Mold.eq(m_l, m_r) =>
      if (Mold.is_convex(R, m_l)) {
        let g = Piece.mk(~token=t, Grout((Concave, Convex)));
        Some(Ziggurat.singleton(~up=[Terrace.singleton(l)], g));
      } else {
        let g = Piece.mk(~token=t, Grout((Concave, Convex)));
        Some(Ziggurat.singleton(l, ~dn=[Terrace.singleton(g)]));
      };
    | _ => None
    };

  let fuse = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t) => {
    open OptUtil.Syntax;
    let/ () = zip(l, ~slot, r);
    let/ () = merge(l, ~slot, r);
    replace(l, ~slot, r);
  };

  let rec meld = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t): Ziggurat.Baked.t =>
    switch (l.material, (slot, Slot.Baked.has_tile(slot)), r.material) {
    // zips
    | _ when Id.eq(l.id, r.id) =>
      assert(Slot.is_empty(slot));
      failwith("todo zips case")

    // merge grout (probably handled entirely by relexing)
    | (Grout((tip_l, _)), , Grout((_, tip_r))) =>
      assert(Slot.is_empty(slot));
      let mold = Material.Grout((tip_l, tip_r));
      let token = l.token ++ r.token;
      Ziggurat.singleton({...l, mold, token});

    // remove empty tiles
    | (Tile(m_l), Some(t), Tile(m_r))
        when Token.is_empty(l.token) && Mold.eq(m_l, m_r) =>
      // to be reshaped as needed in recursive call
      meld(Piece.mk(~token=t, Grout((Convex, Convex))), r)
    | (Tile(m_l), Some(t), Tile(m_r))
        when Token.is_empty(r.token) && Mold.eq(m_l, m_r) =>
      // to be reshaped as needed in recursive call
      meld(l, Piece.mk(~token=t, Grout((Convex, Convex))))

    | _ =>
      let (l, r) = meld;

    };


  let meld = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t) => Ziggurat.Baked.t =>
    switch (l.material, slot, r.material) {
    | (Grout((tip_l, _)), Empty, Grout((_, tip_r))) =>
      // merge grout (probably handled entirely by relexing)
      let mold = Material.Grout((tip_l, tip_r));
      let token = l.token ++ r.token;
      Ziggurat.singleton({...l, mold, token});
    | (Grout((tip))) => failwith("todo")

    };

  let meld = (l: p, ~slot=Slot.Empty, r: p): option(Ziggurat.p) => {
    let lt = (l, ~slot, r) =>
      Ziggurat.mk(Wald.singleton(l), ~dn=[Terrace.singleton(~slot, r)]);
    let gt = (l, ~slot, r) =>
      Ziggurat.mk(~up=[Terrace.singleton(l, ~slot)], Wald.singleton(r));
    let neq = (~lt_if) => lt_if ? lt: gt;

    switch (l.mold, r.mold) {
    | (Grout((tip_l, _)), Grout((_, tip_r))) when Slot.is_empty(slot) =>
      // merge grout (probably handled entirely by relexing)
      let mold = Material.Grout((tip_l, tip_r));
      let token = l.token ++ r.token;
      Ziggurat.singleton({...l, mold, token});
      switch (slot) {
      | Empty =>
      | Full(_) =>
        let l = {...l, mold: Grout((tip_l, Concave))};
        let r = {...r, mold: Grout((Concave, tip_r))};
        Piece.(is_hole(l) == is_hole(r))
        ? Ziggurat.mk(Wald.mk([l, r], [slot]))
        : neq(~lt_if=Piece.is_hole(l), l, ~slot, r);
      }

    | (Grout((tip_l, _)), Tile(m)) =>
      let tip_r =
        Slot.is_full(slot) || Mold.is_convex(L, m) ? Concave : Convex;
      let l = {...l, mold: Grout((tip_l, tip_r))};
      neq(~lt_if=Molded.is_hole(l) && tip_r == Concave, l, ~slot, r);

    | (Tile(m), Grout((_, tip_r))) =>
      let tip_l =
        Slot.is_full(slot) || Mold.is_convex(R, m) ? Concave : Convex;
      let r = Mold.minimize_holes({...r, mold: Grout((tip_l, tip_r))});
      neq(~lt_if=!(Mold.is_hole(r) && tip_l == Concave), l, ~slot, r);

    | (Tile(m_l), Tile(m_r)) =>
      switch (Slot.Baked.has_no_tiles(slot)) {
      // replace l
      | Some(t) when Token.is_empty(l.token) && Mold.eq(m_l, m_r) =>
        if (Mold.is_convex(L, m_r)) {
          let g = Piece.mk(~token=t, Grout((Convex, Concave)));
          Ziggurat.singleton(g, ~dn=[Terrace.singleton(r)]);
        } else {
          let g = Piece.mk(~token=t, Grout((Concave, Convex)));
          Ziggurat.singleton(~up=[Terrace.singleton(g)], r);
        };
      // replace r
      | Some(t) when Token.is_empty(r.token) && Mold.eq(m_l, m_r) =>
        if (Mold.is_convex(R, m_l)) {
          let g = Piece.mk(~token=t, Grout((Concave, Convex)));
          Ziggurat.singleton(~up=[Terrace.singleton(l)], g);
        } else {
          let g = Piece.mk(~token=t, Grout((Concave, Convex)));
          Ziggurat.singleton(l, ~dn=[Terrace.singleton(g)]);
        }
      | _ => failwith("todo")
      }

    }
  };

};

module Wald = {
  include Wald;

  let meld = (l: p, ~slot=Slot.Empty, r: p): Ziggurat.p => {
    let (p_l, p_r) = Wald.(face(R, l), face(L, r));
    let (m_l, m_r) = Piece.(molded(p_l), molded(p_r));
    let slot_all_grout =
      switch (slot) {
      | Empty => Some(Token.empty)
      | Full(m) => Meld.all_grout(m)
      };
    switch (m_l.mold, slot_all_grout, m_r.mold) {
    | (Grout((tip_l, _)), Some(t), Grout((_, tip_r))) =>

    }
  };



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
      Wald.cmp(top, ~slot, w) |> Ziggurat.map_up(Slope.Up.cat(z.up), c)
    | [hd, ...tl] =>
      // left to right: z.up z.top tl hd.mel hd.wal kid w
      let c = Wald.cmp(hd.wald, ~slot, w);
      // left to right: z.up z.top tl hd.mel c.up c.top c.dn
      switch (c.up) {
      | [] =>
        let dn = c.dn @ [{...hd, wald: c.top}, ...tl];
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
    switch (z.up) {
    | [] =>
      Wald.cmp(w, ~slot, top)
      |> Ziggurat.map_dn(Fun.flip(Slope.Dn.cat(z.dn)))
    | [hd, ...tl] =>
      // left to right: w kid hd.wal hd.mel tl z.top z.dn
      let c = Wald.cmp(w, ~slot, hd.wald);
      // left to right: c.up c.top c.dn hd.mel tl z.top z.dn
      switch (c.dn) {
      | [] =>
        // geq
        let up = c.up @ [{...hd, wald: c.top}, ...tl];
        {...z, up};
      | [_, ..._] => push_zigg(c, {...z, up: tl}) // lt
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
