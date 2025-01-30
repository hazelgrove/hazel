/* Gather utility functions/values to be sspaed to the projector.
 * See ProjectorBase.utility definition for more information */
let utility: ProjectorBase.utility = {
  let seg_to_term = MakeTerm.any;
  let term_to_seg =
    ExpToSegment.any_to_pretty(
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
  {term_to_seg, seg_to_term, lift_syntax};
};

let mk_info =
    (p: Piece.projector, ~statics: Statics.Map.t, ~dynamics: Dynamics.Map.t)
    : ProjectorBase.info => {
  id: p.id,
  syntax: Segment.unparenthesize_or_wrap(p.syntax),
  statics: Statics.Map.lookup(p.id, statics),
  dynamics: Dynamics.Map.lookup(p.id, dynamics),
  utility,
};

module Shape = {
  let of_map =
      (statics: Statics.Map.t, dynamics: Dynamics.Map.t, p: Base.projector)
      : ProjectorCore.shape => {
    let (module P) = ProjectorInit.to_module(p.kind);
    P.placeholder(p.model, mk_info(p, ~statics, ~dynamics));
  };

  let of_map_default = of_map(Id.Map.empty, Id.Map.empty);
};
