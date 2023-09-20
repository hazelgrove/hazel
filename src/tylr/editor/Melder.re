open Util;

module Req = {
  type t =             // corresponding obligations when unmet:
    | Comparable       // infix grout
    | Sorts_consistent // prefix/postfix grout
    | Tiles_finished   // unfinished tiles
    | Slots_filled;    // convex grout

  // each requisite has lower-rank requisites as its "prereqs".
  // ordered such that, the higher the rank, the closer the
  // corresponding obligations appear to the leaves of the program
  // (relative to the melded pieces)
  let rank =
    fun
    | Comparable => 0
    | Sorts_consistent => 1
    | Tiles_finished => 2
    | Slots_filled => 3;

  let compare = (l, r) => Int.compare(rank(l), rank(r));

  let max = (l, r) => compare(l, r) <= 0 ? r : l;
};

module Mold = {
  include Mold;
  // exists a match across alternatives
  let matches = (~side: Dir.t, m: Mold.t) => GWalker.walk_eq(~side, m) != [];
  // is convex across all alternatives
  let is_convex = (~side: Dir.t, m: Mold.t) =>
    GWalker.walk_lt(~side, m) == [];

  let lt = (~req=Req.Comparable, l: Bound.t, ~slot=Slot.Empty, r: Mold.t) =>
    GWalker.exit(~side=L, r)
    // looks up sort dependencies based on req.
    // filters walks to those that satisfy the bound (no dangling kid or precedence valid)
    |> List.find_opt(GWalker.satisfies(~req, ~bound=l, ~slot));

  let gt = (~req=Req.Comparable, l: Mold.t, ~slot=Slot.Empty, r: Bound.t) =>
    GWalker.exit(l, ~side=R)
    |> List.find_opt(GWalker.satisfies(~req, ~bound=r, ~slot));

  let eq = (l: Mold.t, r: Mold.t) =>
    GWalker.walk(l, ~toward=R)
    |> List.find_opt(GWalker.satisfies(~req, ~face=r));
};

module Obligation = {
  type t =
    | Slot_unfilled
    | Tile_unfinished
    | Sort_inconsistent
    | Incomparable;
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

  let lt = (~req=Req.Comparable, l: Bound.t, ~slot=Slot.Empty, r: Piece.t): option(ETerrace.R.t) => {
    let ok = Some(ETerrace.singleton(~slot, r));
    switch (l) {
    | Root => root_lt(~req, ~slot, r)
    | Piece(l) =>
      switch (l.material, r.material) {
      | (Space, _) => None
      | (_, Space) => ok

      | (Grout((_, Concave)), Grout((Convex, _))) => ok
      | (Grout(_), Grout(_)) => None

      | (Grout((_, Convex)), Tile(_)) => None
      | (Grout((_, Concave)), Tile(m)) =>
        // exit terraces collectively characterize mold tips
        GWalker.exit(~side=L, m)
        // limit to terraces without matches
        |> List.filter(GWalker.satisfies(Req.max(req, Tiles_finished)))
        |> GWalker.pick(~slot=ESlot.sort(slot))
        |> Option.map(ETerrace.bake(~slot, ~face=r));

      // slightly weird cases in that I'm exiting from the bound
      | (Tile(Convex), _) => None
      | (Tile(Concave()))
      | (Tile(m), Grout((Concave, _))) =>
        // verify that m always matches on its right
        GWalker.exit(m, ~side=R)
        |> List.for_all(t => !GTerrace.satisfies(Tiles_finished, t))
        |> OptUtil.of_bool
        |> OptUtil.and_then(_ => ok)
      | (Tile(m), Grout((Convex, _))) =>
        // check that m takes a kid
        GWalker.exit(m, ~side=R)
        |> List.find_opt(t => !GTerrace.satisfies(Slots_filled, t))
        |> OptUtil.and_then(_ => ok)
      }

    }
  };

  // strict upper bounding
  let bound = (~side: Dir.t, ~sort: Sort.t, ~bound=?, ~slot=Slot.Empty, p: Piece.t): option(Terrace.t) =>
    switch (p.material) {
    | Space
    | Grout(_) => Some(Terrace.singleton(~slot, p))
    | Tile(m) =>
      m
      |> GWalker.bound(~side, ~sort, ~bound?)
      |> Option.map(Terrace.bake(~slot, ~face))
    };

  // todo: probably need transitivity flags
  let root_lt = (~slot=Slot.Empty, r: Piece.t): option(ETerrace.t) => failwith("todo");

  let root_bound = (~req=Req.Comparable, ~side: Dir.t, ~slot=Slot.Empty, p: Piece.t) => {
    let ok = Some(Terrace.singleton(~slot, p));
    switch (p.material) {
    | Space =>
      assert(Slot.is_empty(slot));
      ok;
    | Grout(tips) =>
      switch (req, Dir.choose(side, tips)) {
      | (Comparable, Concave) => None
      | _ => ok
      }
    | Tile(m) =>
      let bake = ETerrace.(side == L ? R.bake : L.bake);
      GWalker.enter(~side, Sort.root)
      // pick: (~trans=?, ~slot: Slot.t(Material.t(Sort.t)), ~face: Mold.t) => option(GTerrace.t)
      // todo: determine whether slot is hard or soft constraint
      |> GTerrace.pick(~req, ~slot, ~face=m)
      |> Option.map(bake(~slot, ~face=p));
    };
  };
  let root_lt = root_bound(~side=L);
  let gt_root = root_bound(~side=R);

  let lt = (~req=Req.Comparable, l: Bound.t, ~slot=Slot.Empty, r: Piece.t): option(ETerrace.R.t) =>
    switch (l) {
    | Root => root_lt(~req?, ~slot, r)
    | Piece(l) =>
      let ok = Some(ETerrace.singleton(~slot, r));
      switch (l.material, r.material) {
      | (_, Space) => ok
      | (Space, _) => None

      | (Grout((_, Concave)), Grout((Convex, _))) => ok
      | (Grout(_), Grout(_)) => None

      | (Grout((_, Convex)), Tile(_)) => None
      | (Grout((_, Concave)), Tile(m)) =>
        GWalker.exit(~side=L, m)
        |> List.find_opt(
          GTerrace.satisfies(~req=Req.max(req, Tiles_finished), ~slot, ~face=m)
        )
        |> Option.map(ETerrace.R.bake(~slot, ~face=p));

      | (Tile(m), Grout((Concave, _))) =>
        // check that m has a match on its right side
        GWalker.exit(m, ~side=R)
        |> List.find_opt(t => !GTerrace.satisfies(~req=Tiles_finished, t))
        |> OptUtil.and_then(_ => ok)
      | (Tile(m), Grout((Convex, _))) =>
        GWalker.exit(m, ~side=R)
        |> List.find_opt(t => !GTerrace.satisfies(~req=Slots_filled, t))
        |> OptUtil.and_then(_ => ok)

      | (Tile(m_l), Tile(m_r)) =>
        Mold.lt(~req, m_l, ~slot=ESlot.sort(slot), m_r)
        |> Option.map(ETerrace.bake(~slot, ~face=r))
      }
    };

  let lt = (l: Bound.t, ~slot=Slot.Empty, r: Piece.t): option(ETerrace.R.t) => {
    open OptUtil.Syntax;
    let* () = OptUtil.of_bool(lt_slot(l, slot));
    switch (l) {
    | Root => root_lt(~slot, r)
    | Piece(l) =>
      let ok = Some(ETerrace.singleton(~slot, r));
      switch (l.material, r.material) {
      | (_, Space)
      | (Grout((_, Concave)), Grout((Convex, _))) => ok
      | (Grout((_, Concave)), Tile(m)) when !Mold.exists_match(~side=L, m) => ok
      | (Tile(m), Grout((Concave, _))) when Mold.exists_match(m, ~side=R) => ok
      | (Tile(m), Grout((Convex, _))) when !Mold.forall_convex(m, ~side=R) => ok
      | (Tile(m_l), Tile(m_r)) =>
        Mold.lt(m_l, ~slot=Slot.map(EMeld.sort, slot), m_r)
        |> Option.map(Terrace.bake(~slot, ~face=r))
      | _ => None
      }
    };
  }
  and lt_slot = (l: Bound.t, slot: ESlot.t) =>
    switch (slot) {
    | Empty => true
    | Full(M(_, w, _)) =>
      Option.is_some(lt(l, Wald.face(~side=L, w)))
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

  let bound_l = (~l=?, ~slot=Slot.Empty, r: Piece.t): option(Slope.Dn.t) =>
    switch (l) {
    | Some(l) =>
      switch (meld(l, ~slot, r)) {
      | {up: [], top, dn}
          when Wald.face(top, ~side=R) == l => Some(dn)
      | _ => None
      }
    | None =>
      switch (p.material) {
      | Space
      | Grout(_) => Some([Terrace.singleton(~slot, p)])
      | Tile(m) =>
        m
        |> GWalker.enter(~from=side, ~sort, ~bound?)
        |> Option.map(Terrace.bake(~slot, ~face=p))
        |> Option.map(t => [t])
      }
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
    let melded_hds = {
      let (tl_l, hd_l) = Wald.split_lst(l);
      let (hd_r, tl_r) = Wald.split_fst(r);
      Piece.meld(hd_l, ~slot, hd_r)
      |> Ziggurat.extend(~side=L, tl_l)
      |> Ziggurat.extend(~side=R, tl_r);
    };
    Scorer.pick([melded_hds] @ melded_tl_l @ melded_tl_r);
  };

  let bound_l = (~l=?, ~slot=Slot.Empty, r: Wald.t): option(Slope.Dn.t) =>
    switch (l) {
    | Some(l) =>
      switch (meld(l, ~slot, r)) {
      | {up: [], top, dn}
          when Wald.face(top, ~side=R) == Wald.face(b, ~side=R) => Some(dn)
      | _ => None
      }
    | None =>
      let (hd, tl) = Wald.split_fst(~side, w);
      switch (hd.material) {
      | Space
      | Grout(_) => Some([Terrace.singleton(~slot, w)])
      | Tile(m) =>
        Mold.lt(~slot=Slot.sort(slot), m)
        |> Option.map(Terrace.bake(~slot, ~face=hd))
        |> Option.map(Terrace.R.extend(tl))
        |> Option.map(t => [t])
      }
    };

  let bound = (~side: Dir.t, ~bound=?, ~slot=Slot.Empty, w: Wald.t): option(Slope.t) => failwith("todo");
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

module Slope = {
  include Slope;
  module Up = {
    include Up;
    let rec push =
            (w: Wald.t, ~slot=Slot.Empty, up: Up.t)
            : (Result.t(Up.t, Slot.t), Up.t) =>
      switch (up) {
      | [] => (Error(slot), up)
      | [hd, ...tl] =>
        // L2R: w slot hd.wald hd.slot tl
        switch (Wald.meld(w, ~slot, hd.wald)) {
        | {up, top, dn: []} => (Ok(up), [{...hd, wald: top}, ...tl])
        | {up, top, dn: [_, ..._]} =>
          let slot = Dn.roll(dn, ~slot=hd.slot);
          let (r, bounded) = push(top, ~slot, tl);
          (r, Slope.cat(up, bounded));
        }
      };
  };
  module Dn = {
    include Dn;

    let rec hsup =
            (~repair=true, ~top=Face.Root, dn: t, ~slot=Slot.Empty, w: Wald.t)
            : (Dn.t, Result.t(Dn.t, Slot.t)) =>
      switch (dn) {
      | [] =>
        // error-correcting?
        // yeah i guess that's fine so long as bound is preserved
        switch (Wald.bound_l(~l=top, ~slot, w)) {
        | None =>
        }


      };

    let rec hsup =
            (~top=Face.Root, dn: t, ~slot=Slot.Empty, w: Wald.t)
            : (Dn.t, Result.t(Dn.t, Slot.t)) =>
      switch (dn) {
      | [] =>
        // todo: return ok if top bounds w
        (dn, Error(slot))
      | [hd, ...tl] =>
        // L2R: tl hd.slot hd.wald slot w
        let default = () => {
          switch (Wald.meld(hd.wald, ~slot, w)) {
          | {up: [], top, dn} => ([{...hd, wald: top}, ...tl], Ok(dn))
          | {up: [_, ..._], top, dn} =>
            // melding should have taken care of any complementing/grouting
            let slot = Up.roll(~slot=hd.slot, up);
            let (bounded, r) = hsup(tl, ~slot, top);
            (Slope.cat(dn, bounded), r);
          };
        };
        switch (
          Slot.has_no_tiles(hd.slot),
          Wald.has_no_tiles(hd.wald),
          Slot.has_no_tiles(slot),
        ) {
        | (_, None, _) =>
        | (Some(s_hd_slot), Some(s_hd_wald), Some(s_slot)) =>
          let s = Piece.mk_space(s_hd_slot ++ s_hd_wald ++ s_slot);
          switch (
            hsup(~top=Slope.face(~top, tl), [], ~slot=Slot.singleton(s), w),
          ) {
          | (_empty, Error(_)) => default()
          | (_empty, Ok(_) as ok) => (tl, ok)
          }
        | ()
        };
      };
  };
};

module Slopes = {
  include Slopes;
  let push =
      (~onto: Dir.t, w: Wald.t, ~slot=Slot.Empty, (dn, up): t)
      : (Result.t(Slope.t, Slot.t), t) =>
    switch (onto) {
    | L =>
      let (dn, r) = Slope.Dn.hsup(dn, ~slot, w);
      (r, (dn, up));
    | R =>
      let (r, up) = Slope.Up.push(w, ~slot, up);
      (r, (dn, up));
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

  let push =
      (~onto: Dir.t, w: Wald.t, ~slot=Slot.Empty, well: t)
      : (Result.t(Slope.t, Slot.t), t) =>
    switch (Slopes.push(~onto, w, ~slot, get_slopes(well))) {
    | (Ok(_) as ok, slopes) => (ok, put_slopes(slopes, well))
    | (Error(slot), slopes) =>
      switch (Stepwell.unlink(well)) {
      | Error(_) =>
        let r =
          Wald.bound(~side=onto, ~slot, w)
          |> Result.of_option(~error=slot);
        (r, put_slopes(slopes, well));
      | Ok((_, (l, r), tl)) =>
        let (r, b') = Slopes.push(~onto, w, ~slot, Bridge.to_slopes(b));
        let tl' = map_slopes(Slopes.(cat(cat(slopes, b'))), tl);
        switch (r) {
        | Ok(_) =>
          // optimization: preserve bridge if it was untouched
          let well' =
            Slopes.face(~side=onto, b') == Bridge.face(~side=onto, b)
              ? put_slopes(slopes, well) : tl';
          (r, well');
        | Error(slot) =>
          // only recurse past unstable bridges
          Bridge.is_stable(b) ? (r, tl') : push(~onto, w, ~slot, tl')
        };
      }
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
