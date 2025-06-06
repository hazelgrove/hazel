/* Projector data which is dependent on semantics,
 * separated out for dependency reasons */

module ShapeMapSemantics = {
  let from_semantics =
      (
        type ed,
        type ed_a,
        type ed_f,
        ~ed_size,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        p: Base.projector(ProjectorCore.model(ed, ed_a, ed_f)),
      )
      : ProjectorShape.t => {
    let ProjectorCore.V(kind, model, _) = p.model;
    let methods = ProjectorCore.to_module(kind);
    /* Projector data which is dependent on semantics,
     * separated out for dependency reasons */

    methods.placeholder(
      ~ed_size,
      model,
      ProjectorCore.mk_info(~id=p.id, ~statics, ~dynamics),
    );
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
