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
      (inline, fn: Any.t => Any.t, seg: Base.segment): option(Base.segment) =>
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
  let lift_term = (fn: Any.t => Any.t, seg: Base.segment): option(Any.t) =>
    switch (seg |> seg_to_term) {
    | None => None
    | Some(s) => Some(s |> fn)
    };
  /* NOTE: Setting indent to anything other than "" has serious
   * perf implications when there are lots of probes on the screen */
  let seg_to_string = Printer.of_segment(~holes="?", ~indent="");
  {
    term_to_seg: (~inline, any) => term_to_seg(inline, any),
    seg_to_term,
    lift_syntax: (~inline) => lift_syntax(inline),
    lift_term,
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
  syntax: p.syntax,
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
      let seg = Segment.unparenthesize(p.syntax);
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
        splice_size: ProjectorBase.View.splice_size,
        p: Base.projector,
      )
      : (ProjectorCore.Shape.t, Id.Map.t(int), option(ProjectorBase.error)) => {
    let (module P) = ProjectorInit.to_module(p.kind);
    let info = mk_info(p, ~sample_focus, ~statics, ~dynamics, ~elaborated);
    (
      P.placeholder(p.model, info, splice_size),
      P.splice_rows(p.model, info, splice_size),
      P.error(p.model, info),
    );
  };

  /* All projector ids occurring anywhere within a segment, including
   * inside tile children, splice contents, and nested projector syntax. */
  let rec projector_ids_in = (seg: Base.segment): list(Id.t) =>
    List.concat_map(
      (p: Base.piece) =>
        switch (p) {
        | Projector(pr) => [pr.id, ...projector_ids_in(pr.syntax)]
        | Splice(s) => projector_ids_in(s.content)
        | Tile(t) => List.concat_map(projector_ids_in, t.children)
        | Grout(_)
        | Secondary(_) => []
        },
      seg,
    );

  /* A projector's placeholder shape depends on its splices' sizes,
   * which in turn depend on the shapes of projectors nested inside
   * those splices. Compute shapes innermost-first, measuring each
   * projector's splices against the shapes computed so far. */
  let mk =
      (
        proj_map: Id.Map.t(Base.projector),
        refractors: ZipperBase.Refractor.t,
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        ~elaborated: option(Exp.t),
      )
      : (
          Id.Map.t(ProjectorCore.Shape.t),
          /* splice id → (host projector id, row offset of the splice
           * within the host's placeholder block); see P.splice_rows */
          Id.Map.t((Id.t, int)),
          Id.Map.t(ProjectorBase.error),
        ) => {
    let contained =
      Id.Map.map(
        (p: Base.projector) => projector_ids_in(p.syntax),
        proj_map,
      );
    let process_one = (id, p: Base.projector, (shapes, layouts, errors)) => {
      let splice_size = (sid: Id.t): Util.Point.t =>
        switch (
          List.find_opt(
            (s: Base.splice) => Id.equal(s.id, sid),
            Segment.direct_splices(p.syntax),
          )
        ) {
        | Some(s) => Measured.segment_bbox(~shape_map=shapes, s.content)
        | None => Util.Point.zero
        };
      let (shape, splice_rows, err) =
        from_semantics(
          refractors.sample_focus,
          statics,
          dynamics,
          ~elaborated,
          splice_size,
          p,
        );
      (
        Id.Map.add(id, shape, shapes),
        {
          /* Every direct splice gets a layout entry (defaulting to the
           * top of the block) so the host is always discoverable, with
           * the projector's own splice_rows offsets layered on top. */

          let layouts =
            List.fold_left(
              (acc, s: Base.splice) => Id.Map.add(s.id, (id, 0), acc),
              layouts,
              Segment.direct_splices(p.syntax),
            );
          Id.Map.fold(
            (sid, row, acc) => Id.Map.add(sid, (id, row), acc),
            splice_rows,
            layouts,
          );
        },
        switch (err) {
        | Some(err) => Id.Map.add(id, err, errors)
        | None => errors
        },
      );
    };
    let rec process = (remaining, acc) =>
      if (Id.Map.is_empty(remaining)) {
        acc;
      } else {
        let (ready, blocked) =
          Id.Map.partition(
            (id, _) =>
              List.for_all(
                cid => !Id.Map.mem(cid, remaining) || Id.equal(cid, id),
                Id.Map.find(id, contained),
              ),
            remaining,
          );
        /* Containment is acyclic, so someone is always ready; the
         * fallback just guarantees termination regardless. */
        let (ready, blocked) =
          Id.Map.is_empty(ready)
            ? (blocked, Id.Map.empty) : (ready, blocked);
        process(blocked, Id.Map.fold(process_one, ready, acc));
      };
    process(proj_map, (Id.Map.empty, Id.Map.empty, Id.Map.empty));
  };
};
