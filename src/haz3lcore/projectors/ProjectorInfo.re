open Language;

/* Projector data which is dependent on semantics,
 * separated out for dependency reasons */

module ShapeMapSemantics = {
  let from_semantics =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        p: Base.projector(ProjectorCore.model(ed_m, ed_a, ed_f)),
      )
      : Util.ProjectorShape.t => {
    let ProjectorCore.V(kind, model, _) = p.model;
    let methods = ProjectorCore.to_module(editor_module, kind);
    /* Projector data which is dependent on semantics,
     * separated out for dependency reasons */
    let placeholder =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed_m,
          m,
        ) => {
          Methods.placeholder(
            m,
            ProjectorCore.mk_info(~id=p.id, ~statics, ~dynamics),
          );
        }
      );
    placeholder(model);
  };

  let mk =
      (
        type p,
        ~shape_of_projector,
        proj_map: Id.Map.t(Base.projector(p)),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
      )
      : Id.Map.t(Util.ProjectorShape.t) =>
    Id.Map.map(shape_of_projector(statics, dynamics), proj_map);
};
