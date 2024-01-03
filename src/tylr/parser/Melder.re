let lt = (l: Bound.t(EWald.t), ~cell=ECell.empty, r: EWald.t) => {
  // assume walds arranged so heads are nearest
  l
  |> Bound.map(w => EWald.hd(w).material)
  |> GWalker.walk(R)
  |> GWalker.Walked.find(EWald.hd(r).material)
  |> List.filter(GWalker.Walk.lt)
  |> List.filter_map(
    ESlope.Dn.bake(
      ~fill=(l, r) => {

      },
      ~face=r,
    )
  )
}


let bake_r = (w: GWalk.R.t, ~cell: ECell.t, ~face: Piece.t): (ESlope.Dn.t, Oblig.Delta.t) => {
  GWalk.R.to_slope(w)
  |> List.fold_left_map(
    (c, t: GTerr.R.t) => {
    },
    Some(cell),
  )
};


let bake_l = (~face: Piece.t, ~cell: ECell.t, w: GWalk.L.t): (ESlope.Up.t, Oblig.Delta.t) =>
  failwith("todo");

let bake = (~face: Piece.t, ~cell: ECell.t, d: Dir.t, w: GWalk.t): (ESlope.t, Oblig.Delta.t) =>
  switch (d) {
  | L => // geq walk
    List.fold_right(
      (gterr, (eslope, delta)) => {

      },
      GWalk.to_slope(w),
      (ESlope.Up.empty, Oblig.Delta.empty),
    )
  };

module Material = {
  include Material;

  let memo = f =>
    FunUtil.uncurry2(f)
    |> Core.Memo.general
    |> FunUtil.curry2;

  let eq = memo((l: Material.t, r: Material.t) =>
    GWalker.walk(R, l)
    |> GWalker.Walked.find(r.material)
    |> List.filter(GWalker.Walk.eq)
  );

  let lt = memo((l: Bound.t(Material.t), r: Material.t) =>
    GWalker.walk(R, l)
    |> GWalker.Walked.find(r.material)
    |> List.filter_map(GWalk.lt)
  );
  let lt = (l, ~sort, r) => List.filter(GSlope.takes(sort), lt(l, r));

  let gt = memo((l: Material.t, r: Bound.t(Material.t)) =>
    GWalker.walk(L, r)
    |> GWalker.Walked.find(l.material)
    |> List.filter(GWalker.Walk.gt)
  );
};

let rec lt =
    (
      ~repair_walk=true,
      ~repair_cell=true,
      l: Bound.t(EWald.t),
      ~cell=ECell.empty,
      r: EWald.t
    )
    : option((ESlope.Dn.t, Oblig.Delta.t)) =>
  l
  |> Bound.map(w => EWald.hd(w).mtrl)
  |> Fun.flip(Mtrl.lt(~sort=ECell.sort(cell)), r.mtrl)
  |> List.map(ESlope.bake)
  |> List.map(
    switch (ECell.has_substance(cell)) {
    | None => ESlope.Dn.pad(cell)
    | Some(m) =>
      ESlope.Dn.fill(
        ~top=l,
        ~fill=(l, )
      )
    }

    switch (ECell.sort(cell)) {
    | Space => pad(cell)
    | Grout() | Tile(_) =>

    }


  )
  |> List.stable_sort(((_, l), (_, r)) => Oblig.Delta.compare(l, r))
  |> ListUtil.hd_opt
and bound = (
  ~repair=true,
  ~l: Bound.t(EWald.t),
  M(c_l, w, c_r): EMeld.t,
  ~r: Bound.t(EWald.t),
) => {

}
and repair_cell = (
  ~l: Bound.t(EWald.t),
  cell: ECell.t,
  ~r: Bound.t(EWald.t),
): option(ECell.t) => {
  // assumes input cell
  open OptUtil.Syntax;
  let* M(cell_l, w, cell_r) as m = cell.content;
  let* (dn, delta_lt) = lt(~repair_cell=false, l, ~cell=cell_l, w);
  let (hd_dn, tl_dn) = List.(hd(dn), tl(dn));
  let* (up, delta_gt) = gt(~repair_cell=false, hd_dn.wald, ~cell=cell_r, r);
  let (hd_up, tl_up) = List.(hd(up), tl(up));
  let cell = ECell.mk(EMeld.mk(~l=hd_dn.cell, hd_up.wald, ~r=hd_up.cell));
  return(zip(tl_dn, cell, tl_up));
};

let rec lt =
    (~repair_cell=true, l: Bound.t(EWald.t), ~cell=ECell.empty, r: EWald.t): option((ESlope.Dn.t, Oblig.Delta.t)) => {
  l
  |> Bound.map(w => EWald.hd(w).mtrl)
  |> Fun.flip(Mtrl.lt, r.mtrl)
  |> List.filter_map(
    bake_dn(~repair_cell, ~cell, ~face=r)
  )
  |> List.stable_sort(((_, l), (_, r)) => Oblig.Delta.compare)
  |> ListUtil.hd_opt
}
and bake_dn =
    (~repair_cell, ~cell, ~face, w: GWalk.R.t): option((ESlope.Dn.t, Oblig.Delta.t)) => {
  let (slope, (cell, delta)) =
    GWalk.R.to_slope(w)
    |> List.fold_left_map(((cell, delta), gterr) => {
        let (eterr, (cell, delta')) = bake_r(~repair_cell, ~cell, gterr);
        (eterr, (cell, Oblig.Delta.merge(delta, delta')));
      },
      cell
    );
  Cell.is_empty(cell) ? Some((put_face(face, slope), delta)) : None;
}
and bake_r = (~repair_cell, ~cell, t: GTerr.R.t): (ETerr.R.t, Oblig.Delta.t) => {
  let (sorts, mtrls) = GTerr.R.split(t);
  // todo: produce delta
  let pcs = List.map(Piece.bake, mtrls);
  switch (ECell.sort(cell)) {
  | Space =>
    // space-only cells are padded into leftmost cell of given grammar walk
    let cells = List.map(ECell.bake, sorts);
    (pad(cell, combine(cells, pcs)), None);
  | Grout() | Tile(_) =>
    // non-space cells replace a sort-consistent cell of given grammar walk

    // todo: fold_left_map over sort-mtrl pairs of gterr,
    // passing forward the last baked piece to the next
    // pair to use for bounding the potentially inserted cell.
    // when bounding

    let (cells, fill) =
      sorts
      |> List.fold_left_map(
        (cell, sort) =>
          ECell.(!is_empty(cell) && consistent(cell, sort))
          ? (cell, ECell.empty)
          : (ECell.bake(sort), cell),
        cell,
      );
    (combine(cells, pcs), fill);
  };
}


module Piece = {
  include Piece;

  let zip = (l: Piece.t, r: Piece.t) =>
    Id.eq(l.id, r.id) ? Some({...l, token: l.token ++ r.token}) : None;

  let replace = (l: Piece.t, r: Piece.t) =>
    if (Material.eq(l.material, r.material) && Piece.is_unfinished(l)) {
      Some(r)
    } else if (Material.eq(l.material, r.material) && Piece.is_unfinished(r)) {
      Some(l)
    } else {
      None
    };

  let eq = (l: Piece.t, ~slot=ESlot.Empty, r: Piece.t): option(EWald.t) => {
    open OptUtil.Syntax;
    let/ () = zip(l, r);
    let/ () = replace(l, r);
    let+ t =
      Material.eq(l.material, r.material)
      |> ETerrace.R.pick_and_bake(~slot, ~face=r);
    EWald.link(l, ~slot=t.slot, t.wald);
  };

  let lt =
      (l: Bound.t(Piece.t), ~slot=ESlot.Empty, r: Piece.t)
      : option(ESlope.Dn.t) =>
    Material.lt(Bound.map(p => p.material, l), r.material)
    |> ESlope.Dn.pick_and_bake(~slot, ~face=r);

  let gt =
      (l: Piece.t, ~slot=ESlot.Empty, r: Bound.t(Piece.t))
      : option(ESlope.Up.t) =>
    Material.gt(l.material, Bound.map(p => p.material, r))
    |> ESlope.Up.pick_and_bake(~slot, ~face=l);
};

module Wald = {
  let rec eq_l = (l: EWald.t, ~slot=ESlot.Empty, r: Piece.t) => {
    open OptUtil.Syntax;
    let/ () =
      switch (EWald.unknil(l)) {
      | Ok((tl, sl, p)) when Piece.is_unfinished(p) =>
        let slot = failwith("todo: merge slot and sl");
        eq(tl, ~slot, r);
      | Error(p)
          when
            Piece.is_unfinished(p)
            && p.material == EWald.face(~side=L, r).material
            && ESlot.has_no_tiles(slot) =>
        return(EWald.singleton(r))
      | _ => None
      };
    // L2R: tl_l hd_l
    let (hd_l, tl_l) = EWald.split_face(l, ~side=R);
    Piece.eq(hd_l, ~slot, r)
    |> Option.map(EWald.extend(~side=L, tl_l));
  };
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
          // todo: if not leq, then need to check for any ghosts to replace
          // in hd.slot (assuming error-correcting flag is on)
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
          // todo: if not geq... shouldn't ever need to check for ghosts
          // if insertion always goes to the left
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
