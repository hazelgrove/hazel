include Terr;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Terr.t(Piece.t, ECell.t);

let bake = ({cell, wald}: GTerr.t) =>
  {cell: ECell.bake(cell), wald: EWald.bake(wald)};

module L = {
  include L;
};

module R = {
  include R;

  let bake = (~cell=ECell.empty, t: GTerr.R.t): (ETerr.R.t, ECell.t) => {
    let (sorts, mtrls) = GTerr.R.split(t);
    let pcs = List.map(Piece.bake, mtrls);
    switch (ECell.sort(cell)) {
    | Space =>
      // space-only cells are padded into leftmost cell of given grammar walk
      let cells = List.map(ECell.bake, sorts);
      (pad(cell, combine(cells, pcs)), None);
    | Grout() | Tile(_) =>
      // non-space cells replace a sort-consistent cell of given grammar walk
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
  };

  let bake_with = (~gt=?, ~slot=ESlot.Empty, ~face=?, t: t) =>
    switch (ESlot.consistent(slot, t.slot)) {
    | Some(slot) => Some({slot, wald: EWald.bake(t.wald, ~r=?face)})
    | None =>
      EWald.bake_with(~slot, t.wald, ~r=?face) |> Option.map(mk(~slot))
    };

  // assuming ts was already filtered to those matching face.material
  let pick_and_bake = (~slot: ESlot.t, ~face: Piece.t, ts: GTerrace.Set.t) =>
    ESlot.degrout(slot)
    |> List.fold_left(
         (baked, slot) =>
           switch (baked) {
           | Some(_) => baked
           | None =>
             GTerrace.Set.elements(ts)
             |> List.filter_map(bake_with(~slot, ~face))
             |> ListUtil.hd_opt
           },
         None,
       );
};
