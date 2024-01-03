include Terr;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Terr.t(Piece.t, ECell.t);

let bake = ({cell, wald}: GTerr.t) => {
  cell: ECell.bake(cell),
  wald: EWald.bake(wald),
};

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
    | Grout ()
    | Tile(_) =>
      // non-space cells replace a sort-consistent cell of given grammar walk
      let (cells, fill) =
        sorts
        |> List.fold_left_map(
             (cell, sort) =>
               ECell.(!is_empty(cell) && consistent(cell, sort))
                 ? (cell, ECell.empty) : (ECell.bake(sort), cell),
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
  // let split_face = (t: p): (Chain.t(Slot.p, Piece.t), Piece.t) => {
  //   let ((ps, slots), p) = Wald.split_lst(t.wald);
  //   (Chain.mk([t.slot, ...slots], ps), p);
  // };
  // let face = t => snd(split_face(t));
  // let pull = (~char=false, t: p): (list(p), Piece.t) => {
  //   let (rest, face) = split_face(t);
  //   let n = Piece.token_length(face);
  //   switch (Piece.unzip(n - 1, face)) {
  //   | Ok((rest_face, c)) when char =>
  //     let (slots, ps) = Chain.(loops(rest), links(rest));
  //     (Option.to_list(combine(slots, ps @ [rest_face])), c);
  //   | _ =>
  //     let (top, slot) =
  //       switch (Chain.unknil(rest)) {
  //       | Error(slot) => ([], slot)
  //       | Ok((tl, p, slot)) =>
  //         let (slots, ps) = Chain.(loops(tl), links(tl));
  //         (Option.to_list(combine(slots, ps @ [p])), slot);
  //       };
  //     (s_of_slot(slot) @ top, face);
  //   };
  // };
};
