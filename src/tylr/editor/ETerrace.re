include Terrace;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Terrace.t(Piece.t, EMeld.t);

module L = {
  include L;
};

module R = {
  include R;

  let bake = (~face=?, {slot, wald}: GTerrace.t) => {
    slot: ESlot.bake(slot),
    wald: EWald.bake(wald, ~r=?face),
  };

  let bake_with = (~slot=ESlot.Empty, ~face=?, t: GTerrace.t) =>
    switch (ESlot.consistent(slot, t.slot)) {
    | Some(slot) => Some({slot, wald: EWald.bake(t.wald, ~r=?face)})
    | None =>
      EWald.bake_with(~slot, t.wald, ~r=?face) |> Option.map(mk(~slot))
    };

  // assuming ts was already filtered to those matching face.material
  let pick_and_bake = (~slot: ESlot.t, ~face: Piece.t, ts: GTerrace.Set.t) => {
    open OptUtil.Syntax;
    let degrouted = ESlot.degrout(slot);
    let/ () =
      degrouted
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

};
