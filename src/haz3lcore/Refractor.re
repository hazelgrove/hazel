open Util;

type refractor_mapping = list((Id.t, Id.t));

let mapping = (z: Zipper.t): refractor_mapping =>
  z.refractors
  |> Id.Map.to_list
  |> List.map(((id, p: Base.projector)) => (id, p.id));

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
  refractors: f(z.refractors),
};

let add = (z: Zipper.t): Zipper.t =>
  switch (Indicated.index(z)) {
  | None => z
  | Some(id) =>
    switch (Id.Map.find_opt(id, z.refractors)) {
    | Some(_) => update_refractors(Id.Map.remove(id), z)
    | None =>
      switch (mk_probe()) {
      | None => z
      | Some(p) => update_refractors(Id.Map.add(id, p), z)
      }
    }
  };
