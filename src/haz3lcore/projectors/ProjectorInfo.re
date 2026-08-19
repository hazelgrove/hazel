open Language;

/* Projector data which is dependent on semantics,
 * separated out for dependency reasons */

/* Gather utility functions/values to be sspaed to the projector.
 * See ProjectorBase.utility definition for more information */
let utility: ProjectorBase.utility = {
  let seg_to_term = MakeTerm.for_projection;
  let term_to_seg = (inline, any) =>
    ExpToSegment.any_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline, CoreSettings.off),
        show_unknown_as_hole: false,
        hole_tiles: false,
        fold_fn_bodies: `NoFold,
        project_tables: false,
        project_html: false,
      },
      any,
    );
  let lift_syntax =
      (inline, fn: Any.t => Any.t, seg: Base.segment): option(Base.segment) => {
    switch (seg |> seg_to_term) {
    | None => None
    | Some(s) =>
      let result = s |> fn |> term_to_seg(inline);
      /* When not inline (projector syntax rewrites like table operations),
         append a trailing newline so the expression doesn't extend to
         the edge of the screen, leaving room for probe values */
      if (!inline) {
        let newline: Base.piece =
          Secondary({
            content: Whitespace(Token.linebreak),
            id: Id.mk(),
          });
        Some(result @ [newline]);
      } else {
        Some(result);
      };
    };
  };
  /* NOTE: Setting indent to anything other than "" has serious
   * perf implications when there are lots of probes on the screen */
  let seg_to_string = Printer.of_segment(~holes="?", ~indent="");
  {
    term_to_seg: (~inline, any) => term_to_seg(inline, any),
    seg_to_term,
    lift_syntax: (~inline) => lift_syntax(inline),
    seg_to_string,
  };
};

let mk_info =
    (
      p: Piece.projector,
      ~sample_focus: Sample.Focus.t,
      ~statics: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      ~elaborated: option(Exp.t),
    )
    : ProjectorBase.info => {
  id: p.id,
  syntax: Piece.unparenthesize(p.syntax),
  statics: Statics.Map.lookup(p.id, statics),
  dynamics:
    switch (Dynamics.Map.lookup(p.id, dynamics)) {
    | Some(samples) =>
      Some({
        samples,
        sample_focus,
      })
    | None => None
    },
  elaborated: {
    let (module P) = ProjectorInit.to_module(p.kind);
    if (P.elaborate_syntax) {
      let seg = Piece.unparenthesize(p.syntax);
      let inner_id =
        try(Some(Segment.root_id(Segment.skel(seg), seg))) {
        | _ => None
        };
      Option.bind(inner_id, id =>
        Option.bind(elaborated, Exp.find_by_id(id))
      );
    } else {
      None;
    };
  },
  utility,
};

module ShapeMapSemantics = {
  let from_semantics =
      (
        sample_focus: Language.Sample.Focus.t,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        ~elaborated: option(Exp.t),
        p: Base.projector,
      )
      : (ProjectorCore.Shape.t, option(ProjectorBase.error)) => {
    let (module P) = ProjectorInit.to_module(p.kind);
    let info = mk_info(p, ~sample_focus, ~statics, ~dynamics, ~elaborated);
    /* A sidebar-docked projector leaves only a chip at the code site, so
     * the base editor reserves the chip's footprint, not the projector's. */
    let shape =
      switch (p.placement) {
      | Sidebar => ProjectorChip.shape(p)
      | Inline => P.placeholder(p.model, info)
      };
    (shape, P.error(p.model, info));
  };

  let mk =
      (
        proj_map: Id.Map.t(Base.projector),
        refractors: ZipperBase.Refractor.t,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        ~elaborated: option(Exp.t),
      )
      : (Id.Map.t(ProjectorCore.Shape.t), Id.Map.t(ProjectorBase.error)) => {
    let both =
      Id.Map.map(
        from_semantics(
          refractors.sample_focus,
          statics,
          dynamics,
          ~elaborated,
        ),
        proj_map,
      );
    let shapes = Id.Map.map(((shape, _)) => shape, both);
    let errors = Id.Map.filter_map((_, (_, err)) => err, both);
    (shapes, errors);
  };
};
