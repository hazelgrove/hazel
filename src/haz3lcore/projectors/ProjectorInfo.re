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
        fold_fn_bodies: `NoFold,
        project_tables: false,
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
      /* Reach refractors only: the (possibly group-merged) path condition for
       * each reach point, precomputed by `resolve_reach` where the whole
       * refractor set is available. Empty elsewhere; solo points fall back to
       * computing their own condition. */
      ~reach_map: Id.Map.t(Reach.t),
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
  reach:
    switch (p.kind) {
    | Reach =>
      switch (Id.Map.find_opt(p.id, reach_map)) {
      | Some(_) as r => r
      | None => Reach.analyze(p.id, statics) /* solo fallback */
      }
    | _ => None
    },
  utility,
};

/* Resolve the reach condition for every Reach refractor, honoring groups:
 * group 0 points use their own path condition; group N≥1 points all share the
 * conjunction of the group's members ("one input reaching all"). */
let resolve_reach =
    (refractors: Refractors.Map.t, statics: Statics.Map.t): Id.Map.t(Reach.t) => {
  let group_of = (entry: Refractors.entry): int =>
    switch (ReachProj.t_of_sexp(Sexplib.Sexp.of_string(entry.model))) {
    | {group, _} => group
    | exception _ => 0
    };
  /* (id, group, path condition) for each analyzable Reach refractor */
  let points =
    Id.Map.bindings(refractors)
    |> List.filter_map(((id, entry: Refractors.entry)) =>
         switch (entry.kind) {
         | Reach =>
           Reach.analyze(id, statics)
           |> Option.map(r => (id, group_of(entry), r))
         | _ => None
         }
       );
  let groups =
    points
    |> List.filter_map(((_, g, _)) => g == 0 ? None : Some(g))
    |> List.sort_uniq(compare);
  let merged_of_group = (g: int): Reach.t =>
    Reach.merge(
      List.filter_map(((_, gg, r)) => gg == g ? Some(r) : None, points),
    );
  let group_reach = List.map(g => (g, merged_of_group(g)), groups);
  List.fold_left(
    (acc, (id, g, r)) =>
      Id.Map.add(id, g == 0 ? r : List.assoc(g, group_reach), acc),
    Id.Map.empty,
    points,
  );
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
    /* The shape path handles syntax-replacing projectors, never Reach
     * refractors, so no reach map is needed here. */
    let info =
      mk_info(
        p,
        ~sample_focus,
        ~statics,
        ~dynamics,
        ~elaborated,
        ~reach_map=Id.Map.empty,
      );
    (P.placeholder(p.model, info), P.error(p.model, info));
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
