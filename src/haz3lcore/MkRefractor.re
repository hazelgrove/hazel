open Util;

let mk = (kind, id): Base.projector => {
  let (module P) = ProjectorInit.to_module(kind);
  let seg = [Piece.mk_grout(~id=Id.invalid, Convex)];
  let piece = Segment.parenthesize(seg);
  let any =
    MakeTerm.for_projection(seg)
    |> OptUtil.get_or_fail("Refractor.mk: maketerm");
  let model = P.init(any) |> OptUtil.get_or_fail("Refractor.mk: init");
  ProjectorCore.mk(~id, kind, piece, model);
};

let add_single = (id: Id.t, z: Zipper.t): Zipper.t => {
  let p = mk(Probe, Id.transform_variant(id));
  Zipper.update_manuals(Id.Map.add(id, p), z);
};
