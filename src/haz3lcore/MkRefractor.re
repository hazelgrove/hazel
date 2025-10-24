open Util;

let mk = (~model=?, kind, id): Base.projector => {
  let (module P) = ProjectorInit.to_module(kind);
  let seg = [Piece.mk_grout(~id=Id.invalid, Convex)];
  let piece = Segment.parenthesize(seg);
  let any =
    MakeTerm.for_projection(seg)
    |> OptUtil.get_or_fail("Refractor.mk: maketerm");
  let model =
    (
      switch (model) {
      | Some(m) => Some(m)
      | None => P.init(any)
      }
    )
    |> OptUtil.get_or_fail("Refractor.mk: init");
  ProjectorCore.mk(~id, kind, piece, model);
};

let add_single = (~model=?, id: Id.t, z: Zipper.t): Zipper.t => {
  let p = mk(~model?, Probe, Id.transform_variant(id));
  Zipper.update_manuals(Id.Map.add(id, p), z);
};
