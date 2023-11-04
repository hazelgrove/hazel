include Slope;

type t = Slope.t(Piece.t, ECell.t);

let bake = List.rev_map(ETerr.bake);

module Dn = {
  include Dn;

  let bake = (~cell=ECell.empty, ~face: Piece.t, w: GWalk.R.t) => {
    let (slope, cell) =
      GWalk.R.to_slope(w)
      |> List.fold_left_map(cell => ETerr.bake(~cell), cell);
    assert(Cell.is_empty(cell));
    put_face(face, slope);
  };

  // L2R: dn slot
  let rec roll = (~slot=ESlot.Empty, dn: t) =>
    switch (dn) {
    | [] => slot
    | [hd, ...tl] =>
      let m = EMeld.mk(~l=hd.slot, hd.wald, ~r=slot);
      roll(tl, ~slot=Full(m));
    };

  let bake = (~face=?) =>
    fun
    | [] => raise(baked_empty)
    | [hd, ...tl] => [
        ETerrace.bake(~face?, hd),
        ...List.map(ETerrace.bake, tl),
      ];

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
};
