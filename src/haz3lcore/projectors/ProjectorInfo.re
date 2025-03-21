/* Projector data which is dependent on semantics,
 * separated out for dependency reasons */

/* Gather utility functions/values to be sspaed to the projector.
 * See ProjectorBase.utility definition for more information */
let utility: ProjectorBase.utility = {
  let seg_to_term = MakeTerm.for_projection;
  let term_to_seg =
    ExpToSegment.any_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
        show_unknown_as_hole: false,
      },
    );
  let lift_syntax =
      (fn: Any.t => Any.t, seg: Base.segment): option(Base.segment) =>
    switch (seg |> seg_to_term) {
    | None => None
    | Some(s) => Some(s |> fn |> term_to_seg)
    };
  {
    term_to_seg,
    seg_to_term,
    lift_syntax,
  };
};

let mk_info =
    (p: Piece.projector, ~statics: Statics.Map.t, ~dynamics: Dynamics.Map.t)
    : ProjectorBase.info => {
  id: p.id,
  syntax: Piece.unparenthesize(p.syntax),
  statics: Statics.Map.lookup(p.id, statics),
  dynamics: Dynamics.Map.lookup(p.id, dynamics),
  utility,
};

module ShapeMapSemantics = {
  let from_semantics =
      (statics: Statics.Map.t, dynamics: Dynamics.Map.t, p: Base.projector)
      : ProjectorCore.Shape.t => {
    let (module P) = ProjectorInit.to_module(p.kind);
    P.placeholder(p.model, mk_info(p, ~statics, ~dynamics));
  };

  let mk =
      (
        proj_map: Id.Map.t(Base.projector),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
      )
      : Id.Map.t(ProjectorCore.Shape.t) =>
    Id.Map.map(from_semantics(statics, dynamics), proj_map);
};
