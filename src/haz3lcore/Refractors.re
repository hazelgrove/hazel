open Util;

let mk_probe = (): option(Base.projector) => {
  open OptUtil.Syntax;
  let kind = ProjectorCore.Kind.Probe;
  let (module P) = ProjectorInit.to_module(kind);
  let seg: Segment.t = [Piece.mk_grout(~id=Id.invalid, Convex)];
  let piece: Base.piece = Segment.parenthesize(seg);
  let* any = MakeTerm.for_projection(seg);
  let+ model = P.init(any);
  ProjectorCore.mk(kind, piece, model);
};

let update_refractors = (f, z: Zipper.t): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    map: f(z.refractors.map),
  },
};

let add' = (id: Id.t, z: Zipper.t): Zipper.t => {
  switch (Id.Map.find_opt(id, z.refractors.map)) {
  | Some(_) => update_refractors(Id.Map.remove(id), z)
  | None =>
    switch (mk_probe()) {
    | None => z
    | Some(p) => update_refractors(Id.Map.add(id, p), z)
    }
  };
};

let add = (z: Zipper.t): Zipper.t =>
  switch (Indicated.index(z)) {
  | None => z
  | Some(id) => add'(id, z)
  };
