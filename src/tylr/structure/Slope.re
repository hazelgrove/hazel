type t = list(Terr.t);

let empty = [];
let singleton = t => [t];
// let of_piece = p => of_terr(Terr.of_piece(p));
let height = List.length;

let fold = List.fold_left;

let bake = List.map(Terr.bake);

// Dn and Up slopes named based on left-to-right order of terraces
// as displayed on screen, but terraces are always maintained
// in list order low-to-high
module Dn = {
  // let bake = (~cell=ECell.empty, ~face: Piece.t, w: GWalk.R.t) => {
  //   let (slope, cell) =
  //     GWalk.R.to_slope(w)
  //     |> List.fold_left_map(cell => ETerr.bake(~cell), cell);
  //   assert(Cell.is_empty(cell));
  //   put_face(face, slope);
  // };

  // L2R: dn slot
  let rec roll = (~cell=Cell.Empty, dn: t) =>
    switch (dn) {
    | [] => cell
    | [hd, ...tl] =>
      let m = Meld.mk(~l=hd.slot, hd.wald, ~r=slot);
      roll(tl, ~slot=Full(m));
    };

  // checks for strict
  let bake_with = (~slot=ESlot.Empty, ~face=?, dn: GSlope.Dn.t) => {
    let rec go = (~slot, dn) =>
      switch (GSlope.Dn.pull_top(dn)) {
      | None => raise(baked_empty)
      | Some((t, [])) =>
        ETerrace.bake_with(~slot, ~face?, t) |> Option.map(t => [t])
      | Some((top, [_, ..._] as dn)) =>
        switch (ETerrace.bake_with(~slot, top)) {
        | Some(t) => Some(ESlope.Dn.push_top(t, bake(~face?, dn)))
        | None =>
          go(~slot, dn)
          |> Option.map(ESlope.Dn.push_top(ETerrace.bake(top)))
        }
      };
    go(~slot, dn);
  };

  let pick_and_bake = (~slot: ESlot.t, ~face: Piece.t, ss: GSlope.Set.t) =>
    ESlot.degrout(slot)
    |> List.fold_left(
         (baked, sort) =>
           switch (baked) {
           | Some(_) => baked
           | None =>
             GSlope.Set.elements(ss)
             |> List.filter_map(bake_with(~slot, ~face))
             |> ListUtil.hd_opt
           },
         None,
       );

  let pull = (~char=false, dn: p): option((p, Piece.t)) =>
    switch (dn) {
    | [] => None
    | [hd, ...tl] =>
      let (rest, p) = Terr.R.pull(~char, hd);
      Some((rest @ tl, p));
    };
};

module Up = {
  include Slope.Up;

  let rec roll = (~slot=ESlot.Empty, up: t) =>
    switch (up) {
    | [] => slot
    | [hd, ...tl] =>
      let m = EMeld.mk(~l=slot, hd.wald, ~r=hd.slot);
      roll(~slot=Full(m), tl);
    };

  let pull = (~char=false, up: p): option((Piece.t, p)) =>
    switch (up) {
    | [] => None
    | [hd, ...tl] =>
      let (p, rest) = Terr.L.pull(~char, hd);
      Some((p, rest @ tl));
    };
};
