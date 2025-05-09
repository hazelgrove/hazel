/* Projector data which is dependent on semantics,
 * separated out for dependency reasons */

let mk_info =
    (
      type p,
      p: Piece.projector(p),
      ~statics: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
    )
    : ProjectorBase.info => {
  id: p.id,
  statics: Statics.Map.lookup(p.id, statics),
  dynamics: Dynamics.Map.lookup(p.id, dynamics),
};

module ShapeMapSemantics = {
  let from_semantics =
      (
        type ed,
        type ed_a,
        ~ed_str,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        p: Base.projector(ProjectorCore.model(ed, ed_a)),
      )
      : ProjectorShape.t => {
    let ProjectorCore.V(kind, model) = p.model;
    let methods = ProjectorCore.to_module(kind);
    /* Projector data which is dependent on semantics,
     * separated out for dependency reasons */

    methods.placeholder(~ed_str, model, mk_info(p, ~statics, ~dynamics));
  };

  let mk =
      (
        type p,
        ~shape_of_projector,
        proj_map: Id.Map.t(Base.projector(p)),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
      )
      : Id.Map.t(ProjectorShape.t) =>
    Id.Map.map(shape_of_projector(statics, dynamics), proj_map);
};
