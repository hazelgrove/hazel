open Haz3lcore;
open Util.WebUtil;

/* RefractorView handles the display of refractors (probes).
 *
 * Unlike projectors which replace syntax and have their own measurements,
 * refractors overlay on existing syntax. Their position is derived from
 * the underlying term's measurement (specifically the rightmost point).
 */

/* Refractor positioning: place at the right edge of the underlying term */
let measurement_of_term =
    (id: Id.t, term_data: TermData.t, measured: Measured.t)
    : option(Measured.measurement) =>
  switch (TermData.extreme_measures(id, term_data, measured)) {
  | None => None
  | Some((_l, r)) =>
    Some(
      Measured.{
        origin: r,
        last: r,
      },
    )
  };

/* Build refractor data from editor state.
 * This is analogous to ProjectorView.Model.mk but specialized for
 * refractors, with one twist for terms inside splices: the owning
 * splice's sub-editor draws the term-anchored layers (only its local
 * measured map knows the term's position), while the offside sample
 * view never goes into a splice — the root editor draws it beside the
 * host projector, on the document row the splice's contents are laid
 * out on (CachedSyntax.doc_row_of_splice). */
let mk_data =
    (
      ~refractors: Zipper.Refractor.Map.t,
      ~syntax: CachedSyntax.t,
      ~indicated: option(Indicated.piece),
      ~statics: Language.Statics.Map.t,
      ~dynamics: Language.Dynamics.Map.t,
      ~sample_focus: Language.Sample.Focus.t,
      ~editor_active: bool,
      /* The frame being rendered: None for the root editor, Some(sid)
       * for splice sid's sub-editor. */
      ~frame: option(Id.t),
    )
    : list(ProjectorView.Model.projector_data) => {
  open Util.OptUtil.Syntax;
  let {term_data, selection_ids, _}: CachedSyntax.t = syntax;
  let measured = CachedSyntax.measured(syntax);
  let placement = (id: Id.t) =>
    switch (frame, CachedSyntax.splice_containing_id(id, syntax)) {
    | (Some(frame_sid), Some((sid, _))) when Id.equal(sid, frame_sid) =>
      /* This frame's own probe: term-anchored layers at local coords. */
      let+ measurement = measurement_of_term(id, term_data, measured);
      (measurement, 0, ProjectorView.Model.NoOffside);
    | (Some(_), _) =>
      /* Another frame's probe (the root's, another splice's, or a
       * nested splice's — interiors are measured recursively, so the
       * local lookup would "succeed" for nested ids too). */
      None
    | (None, None) =>
      let+ measurement = measurement_of_term(id, term_data, measured);
      (
        measurement,
        ProjectorView.Model.offside_base(
          ~offset=ProjectorView.offside_offset,
          measurement,
          measured,
        ),
        ProjectorView.Model.All,
      );
    | (None, Some((sid, s))) =>
      /* Offside view only, beside the host projector on the splice
       * contents' document row. */
      let* local = measurement_of_term(id, term_data, s.measured);
      let+ splice_row = CachedSyntax.doc_row_of_splice(sid, syntax);
      let point =
        Util.Point.{
          row: splice_row + local.origin.row,
          col: 0,
        };
      let measurement =
        Measured.{
          origin: point,
          last: point,
        };
      (
        measurement,
        ProjectorView.Model.offside_base(
          ~offset=ProjectorView.offside_offset,
          measurement,
          measured,
        ),
        ProjectorView.Model.OffsideOnly,
      );
    };
  List.filter_map(
    ((id, entry)) => {
      /* Construct full Base.projector on demand for rendering,
       * passing the actual syntax so projectors can access the
       * underlying term for syntax rewriting. */
      let syntax_piece =
        Option.value(
          TermData.segment(id, term_data)
          |> Option.map(Segment.unparenthesize)
          |> Option.map(Segment.trim_secondary(Left))
          |> Option.map(Segment.trim_secondary(Right))
          |> Option.map(Segment.parenthesize),
          ~default=
            Base.Secondary({
              id: Id.invalid,
              content: Whitespace(""),
            }),
        );
      let p = Refractors.to_projector(syntax_piece, id, entry);
      let+ (measurement, offside, layers) = placement(id);
      let info =
        ProjectorInfo.mk_info(
          p,
          ~sample_focus,
          ~statics,
          ~dynamics,
          ~elaborated=None,
        );
      ProjectorView.Model.{
        p,
        info,
        measurement,
        offside_base: offside,
        render_layers: layers,
        status:
          ProjectorView.Model.mk_status(
            p,
            ~sort=TermData.sort(id, term_data),
            ~editor_active,
            ~indicated,
            ~selection_ids,
            ~info,
            ~statics,
            ~id,
          ),
        statics_map: statics,
        dynamics_map: dynamics,
        sample_focus,
        elaborated: None,
      };
    },
    Id.Map.bindings(refractors),
  );
};

/* Render all refractors. Refractors skip the inline view (skip_inline=true)
 * because they overlay on existing syntax rather than replacing it.
 */
let all =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      ~visible: option(Globals.VisibleRows.t)=?,
      refractor_data: list(ProjectorView.Model.projector_data),
      refractor_list: list(Id.t),
    ) => {
  let get_row_range = (d: ProjectorView.Model.projector_data) => (
    d.measurement.origin.row,
    d.measurement.last.row,
  );
  let (base_views, overlay_views) =
    refractor_data
    |> ProjectorView.filter_by_visibility(visible, _, get_row_range)
    |> List.sort(ProjectorView.by_measurement)
    |> List.map(data =>
         ProjectorView.split_views(
           inject,
           make_active,
           font_metrics,
           ~core_settings,
           ~skip_inline=true,
           ~render_splice=ProjectorView.default_render_splice(font_metrics),
           data,
           refractor_list,
         )
       )
    |> List.split;
  let overlay_views = List.filter_map(Fun.id, overlay_views);
  [
    div_c(
      "refractors",
      [div_c("base", base_views), div_c("overlays", overlay_views)],
    ),
  ];
};
