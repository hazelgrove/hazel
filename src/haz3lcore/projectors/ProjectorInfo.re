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
  let term_to_syntax = (any: Any.t): Base.piece =>
    switch (term_to_seg(any)) {
    | [e] => e
    | seg => Segment.parenthesize(seg)
    };
  let lift_syntax = (fn: Any.t => Any.t, piece: Base.piece): Base.piece =>
    [piece] |> seg_to_term |> fn |> term_to_syntax;
  {term_to_seg, seg_to_term, lift_syntax};
};

let adjust_syntax = (syntax: Base.piece): Base.piece =>
  /* Given that syntax is unconditionally parenthesize on projection,
   * (for technical reasons), and we don't want to make individual
   * projectors have to deal with this extra wrapping level, we
   * unparenthesize when possible (this may be ill-advised) */
  switch (Segment.unparenthesize(syntax)) {
  | Some([e]) => e
  | Some(_) => syntax
  | None =>
    //prerr_endline("WARNING: asjust_syntax: not parenthesized");
    syntax
  };

let unparenthesize = (piece: Piece.t): Segment.t =>
  switch (Segment.unparenthesize(piece)) {
  | Some(seg) => seg
  | _ =>
    //prerr_endline("WARNING: Unparenthesize: not parenthesized");
    [piece]
  };

let mk_info =
    (p: Piece.projector, ~statics: Statics.Map.t, ~dynamics: Dynamics.Map.t)
    : ProjectorBase.info => {
  id: p.id,
  syntax: adjust_syntax(p.syntax),
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
