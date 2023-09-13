open Util;

module Mold = {
  include Mold;
  // exists a match across alternatives
  let matches = (~side: Dir.t, m: Mold.t) => GWalker.walk_eq(~side, m) != [];
  // is convex across all alternatives
  let is_convex = (~side: Dir.t, m: Mold.t) =>
    GWalker.walk_lt(~side, m) == [];
};

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

  let meld = (l: Piece.t, ~slot=Slot.Empty, r: Piece.t): Ziggurat.t => {
    let eq_ = Ziggurat.mk(Wald.mk([l, r], [slot]));
    let lt_ = Ziggurat.singleton(l, ~dn=[Terrace.singleton(~slot, r)]);
    let gt_ = Ziggurat.singleton(~up=[Terrace.singleton(l, ~slot)], r);

    switch (l.material, r.material) {
    // zips
    | _ when Id.eq(l.id, r.id) =>
      assert(Slot.is_empty(slot));
      failwith("todo zips case");

    // whitespace
    | (Space, Space) =>
      assert(Slot.is_empty(slot));
      Ziggurat.singleton({...l, token: l.token ++ r.token});
    | (Space, _) => gt_
    | (_, Space) => lt_

    // grout-grout
    | (Grout((_, Convex)), Grout((Convex, _))) =>
      assert(Slot.is_empty(slot));
      let (up, dn) = Terrace.([singleton(l)], [singleton(r)]);
      Ziggurat.singleton(~up, Piece.mk_grout(Concave, Concave), ~dn);
    | (Grout((_, Convex)), Grout((Concave, _))) => gt_
    | (Grout((_, Concave)), Grout((Convex, _))) => lt_
    | (Grout((_, Concave)), Grout((Concave, _))) => eq_

    // compare tile molds
    | (Tile(m_l), Tile(m_r)) =>
      switch (Mold.compare(m_l, ~slot=Slot.map(Meld.Baked.sort, slot), m_r)) {
      | Some(zigg) => Ziggurat.bake(~l, ~slot, ~r, zigg)
      | None =>
        if (is_convex(m_l, ~side=R) && !is_convex(~side=L, m_r)) {
          assert(Slot.is_empty(slot));
          let g = Terrace.singleton(Piece.mk_grout(Concave, Convex));
          let t_l = GWalker.exit(m_l, ~side=R) |> Terrace.bake(~face=l);
          Ziggurat.mk(~up=[t_l, g], Wald.singleton(r));
        } else if (!is_convex(m_l, ~side=R) && is_convex(~side=L, m_r)) {
          assert(Slot.is_empty(slot));
          let g = Terrace.singleton(Piece.mk_grout(Convex, Concave));
          let t_r = GWalker.exit(~side=L, m_r) |> Terrace.bake(~face=r);
          Ziggurat.mk(Wald.singleton(l), ~dn=[t_r, g]);
        } else {
          let g = Wald.singleton(Piece.mk_grout(Concave, Concave));
          let t_l = GWalker.exit(m_l, ~side=R) |> Terrace.bake(~face=l);
          let t_r = GWalker.exit(~side=L, m_r) |> Terrace.bake(~face=r);
          Ziggurat.mk(~up=[t_l], g, ~dn=[t_r]);
        }
      }
    };
  };
};

module Wald = {
  include Wald;

  let rec meld = (l: Wald.t, ~slot=Slot.Empty, r: Wald.t): Ziggurat.t => {
    let melded_tl_l =
      switch (Wald.unknil(l)) {
      | Ok((tl, s, p)) when Piece.is_unfinished(p) =>
        [meld(tl, ~slot=Slot.merge(s, slot), r)]
        |> List.filter(z => Ziggurat.height(z) == 1)
      | _ => []
      };
    let melded_tl_r =
      switch (Wald.unlink(r)) {
      | Ok((p, s, tl)) when Piece.is_unfinished(p) =>
        [meld(l, ~slot=Slot.merge(slot, s), r)]
        |> List.filter(z => Ziggurat.height(z) == 1)
      | _ => []
      };
    let melded_hds =
      Piece.meld(Wald.face(l, ~side=R), ~slot, Wald.face(~side=L, r))
      |> Ziggurat.extend(~side=L, tl_l)
      |> Ziggurat.extend(~side=R, tl_r);
    Scorer.pick([melded_hds] @ melded_tl_l @ melded_tl_r);
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

module Bound = {
  type t =
    | Root
    | Terr(Terrace.t);
};

module Bounded = {
  type t = (Slope.t, Bound.t);
};

// module Result = {
//   include Result;
//   type t = Result.t(Bounded.t, Slot.t);
// };

module Slope = {
  include Slope;
  module Up = {
    include Up;
    let rec push = (w: Wald.t, ~slot=Slot.Empty, up: Up.t): Result.t((Bounded.t, Up.t), Slot.t) =>
      switch (up) {
      | [] => Error(slot)
      | [hd, ...tl] =>
        // L2R: w slot hd.wald hd.slot tl
        switch (Wald.meld(w, ~slot, hd.wald)) {
        | {up, top, dn: []} => Ok(((up, Terr({...hd, wald: top})), tl))
        | {up, top, dn: [_, ..._]} =>
          // todo: consider how bounded slopes can be unified with roll
          let expected = Wald.expected_sorts(top, ~side=R);
          let slot = Dn.roll(~expected, dn, ~slot=hd.slot);
          push(top, ~slot, tl) |> Result.map(~f=Slope.cat(up));
        }
      };
  };
  module Dn = {
    include Dn;
    let rec hsup = (dn: t, ~slot=Slot.Empty, w: Wald.t): Result.t((Dn.t, Dn.t), Slot.t) =>
      switch (dn) {
      | [] => Error(slot)
      | [hd, ...tl] =>
        // L2R: tl hd.slot hd.wald slot w
        switch (Wald.meld(hd.wald, ~slot, w)) {
        | {up: [], top, dn} => Ok(([{...hd, wald: top}, ...tl], dn))
        | {up: [_, ..._], top, dn} =>
          let expected = Wald.expected_sort(~side=L, top);
          let slot = Up.roll(~slot=hd.slot, up, ~expected);
          hsup(tl, ~slot, top)
          |> Result.map(~f=((tl, bounded)) => (tl, Slope.cat(dn, bounded)));
        }
      };
  };
};

module Ziggurat = {
  include Ziggurat;

  let push = (w: Wald.t, ~slot=Slot.Empty, z: Ziggurat.t) =>
    switch (Slope.Up.push(w, ~slot, z.up)) {
    | Ok(up) => {...z, up}
    | Error(slot) =>
      Wald.meld(w, ~slot, z.top) |> Ziggurat.map_dn(Slope.cat(z.dn))
    };

  let hsup = (z: Ziggurat.t, ~slot=Slot.Empty, w: Wald.t) =>
    switch (Slope.Dn.hsup(z.dn, ~slot, w)) {
    | Ok(dn) => {...z, dn}
    | Error(slot) =>
      Wald.meld(z.top, ~slot, w) |> Ziggurat.map_up(Slope.cat(z.up))
    };

  // init flag indicates which ziggurat serves as the initial accumulator
  // onto which we push the decomposition of the other. only impact is
  // performance: meld(~init=L, l, r) == meld(~init=R, l, r) for all l and r
  let meld = (~init=Dir.R, l: Ziggurat.t, r: Ziggurat.t) =>
    switch (init) {
    | L =>
      let (ws, slots) = Slope.split(r.up);
      Chain.mk(ws @ [r.top], slots)
      |> Chain.fold_left(
           w => hsup(l, w),
           (l, slot, w) => hsup(l, ~slot, w),
         )
      |> Ziggurat.map_dn(Slope.cat(r.dn));
    | R =>
      let (ws, slots) = Slope.split(l.dn);
      Chain.mk(ws @ [l.top], slots)
      |> Chain.fold_left(
           w => push(w, r),
           (w, slot, r) => push(w, ~slot, r),
         )
      |> Ziggurat.map_up(Slope.cat(l.up));
    };
};

module Stepwell = {
  include Stepwell;

  // push wald against left slope of stepwell until either bound
  // or stable bridge is encountered
  let hsup =
      (well: Stepwell.t, ~slot=Slot.Empty, w: Wald.t)
      : (Stepwell.t, Result.t(Slope.Dn.t, Slot.t)) => {
    let (dn, up) = Stepwell.get_slopes(well);
    switch (Slope.Dn.hsup(dn, ~slot, w)) {
    | Ok((dn, bounded)) => (put_slopes(dn, up), Ok(bounded))
    | Error(slot) =>
      switch (Stepwell.unlink(well)) {
      | Error(_) => (put_slopes(([], up), well), Error(slot))
      | Ok((_, b, tl)) =>
        // break bridge and recurse
        let tl =
          tl
          |> map_slopes(Slopes.cat(Bridge.to_slopes(b)))
          |> map_slopes(Slopes.cat(([], up)));
        switch (hsup(tl, ~slot, w)) {
        // optimization: preserve bridge if it was untouched
        | (tl, Ok(dn)) when face(~side=L, tl) == Bridge.face(~side=L, b) =>
          (put_slopes(([], up), well), Ok(dn))
        | (_, Ok(_)) as r => r
        | (_, Error(_)) as r when Bridge.is_stable(b) => r
        | (tl, Error(slot)) => hsup(tl, ~slot, w)
        };
      }
    };
  };



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
