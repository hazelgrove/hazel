module Piece = {
  include Piece;

  let zip = (l: Piece.t, r: Piece.t) =>
    Id.eq(l.id, r.id) ? Some({...l, token: l.token ++ r.token}) : None;

  let eq = (l: Piece.t, ~slot=ESlot.Empty, r: Piece.t) =>
    switch (zip(l, r)) {
    | Some(p) =>
      assert(Slot.is_empty(slot));
      EWald.singleton(p);
    | None =>
      GWalker.walk(R, l)
      |> GWalker.Walked.find(r.material)
      |> ETerrace.R.pick_and_bake(~slot, ~face=r)
      |> Option.map(t => EWald.link(l, ~slot=t.slot, t.wald))
    };

  let lt = (~without=[], l: Bound.t(Piece.t), ~slot=ESlot.Empty, r: Piece.t) =>
    l
    |> Bound.map(p => p.material)
    |> GWalker.descend(R)
    |> GWalker.Descended.find(r.material)
    |> ESlope.Dn.pick_and_bake(~slot, ~face=r);

  let gt = (~without=[], l: Piece.t, ~slot=ESlot.Empty, r: Bound.t(Piece.t)) =>
    r
    |> Bound.map(p => p.material)
    |> GWalker.descend(L)
    |> GWalker.Descended.find(l.material)
    |> ESlope.Up.pick_and_bake(~face=l, ~slot);
};

module Wald = {
  let eq = (~without=[], l: EWald.t, ~slot=ESlot.Empty, r: EWald.t) => {
    let (hd_l, tl_l) = EWald.split_face(l, ~side=R);
    let (hd_r, tl_r) = EWald.split_face(~side=L, r);
    Piece.eq(~without, hd_l, ~slot, hd_r)
    |> Option.map(EWald.extend_face(~side=L, tl_l))
    |> Option.map(EWald.extend_face(~side=R, tl_r));
  };

  let lt = (~without=[], l: Bound.t(EWald.t), ~slot=ESlot.Empty, r: EWald.t) => {
    let l = Bound.map(Wald.face(~side=R), l);
    let (hd_r, tl_r) = EWald.split_face(~side=L, r);
    Piece.lt(~without, l, ~slot, hd_r)
    |> Option.map(ETerrace.R.extend_face(tl_r));
  };
  let leq = (~without=[], l: EWald.t, ~slot=ESlot.Empty, r: EWald.t) =>
    switch (eq(l, ~slot, r)) {
    | Some(eq) => Some((eq, ESlope.Dn.empty))
    | None => lt(~without, Piece(l), ~slot, r) |> Option.map(lt => (l, lt))
    };

  let gt = (~without=[], l: EWald.t, ~slot=ESlot.Empty, r: Bound.t(EWald.t)) => {
    let (hd_l, tl_l) = EWald.split_face(l, ~side=R);
    let r = Bound.map(Wald.face(~side=L), r);
    Piece.gt(~without, hd_l, ~slot, r)
    |> Option.map(ETerrace.L.extend_face(tl_l));
  };
  let geq = (~without=[], l: EWald.t, ~slot=ESlot.Empty, r: EWald.t) =>
    switch (eq(l, ~slot, r)) {
    | Some(eq) => Some((ESlope.Up.empty, eq))
    | None => gt(~without, l, ~slot, Piece(r)) |> Option.map(gt => (gt, r))
    };
};

module Slope = {
  module Dn = {
    let rec meld =
            (~top=Bound.Root, dn: ESlope.Dn.t, ~slot=ESlot.Empty, w: EWald.t) =>
      switch (dn) {
      | [] => Wald.lt(top, ~slot, w) |> Option.map(t => [t])
      | [hd, ...tl] =>
        switch (Wald.leq(hd.wald, ~slot, w)) {
        | Some((eq, lt)) =>
          Some(ESlope.cat(lt, [{...hd, wald: eq}, ...tl]))
        | None =>
          let slot =
            switch (Wald.gt(hd.wald, ~slot, Piece(w))) {
            | Some(gt) => ESlope.Up.roll(~slot=hd.slot, gt)
            | None => ESlot.Full(EMeld.mk(~l=hd.slot, hd.wald, ~r=slot))
            };
          meld(~top, tl, ~slot, w);
        }
      };
  };
  module Up = {
    // L2R: w slot up top
    let rec meld =
            (~top=Bound.Root, w: EWald.t, ~slot=ESlot.Empty, up: ESlope.Up.t) =>
      switch (up) {
      | [] => Wald.gt(w, ~slot, bound) |> Option.map(t => [t])
      | [hd, ...tl] =>
        switch (Wald.geq(w, ~slot, hd.wald)) {
        | Some((gt, eq)) =>
          Some(ESlope.cat(gt, [{...hd, wald: eq}, ...tl]))
        | None =>
          let slot =
            switch (Wald.lt(Piece(w), ~slot, hd.wald)) {
            | Some(lt) => ESlope.Dn.roll(lt, ~slot=hd.slot)
            | None => ESlot.Full(EMeld.mk(~l=slot, hd.wald, ~r=hd.slot))
            };
          meld(w, ~slot, tl, ~bound);
        }
      };
  };
};

module Stepwell = {
  let meld =
      (~onto: Dir.t, w: EWald.t, well: EStepwell.t): option(EStepwell.t) => {
    open OptUtil.Syntax;
    let (l, (dn, up), r) = EStepwell.get_bounded_slopes(well);
    let+ slopes =
      switch (onto) {
      | L =>
        let+ dn = Slope.Dn.meld(~bound=l, dn, w);
        (dn, up);
      | R =>
        let+ up = Slope.Up.meld(w, up, ~bound=r);
        (dn, up);
      };
    EStepwell.put_slopes(slopes, well);
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
          Wald.bound(~side=onto, ~slot, w) |> Result.of_option(~error=slot);
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
