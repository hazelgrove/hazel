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
 * This is analogous to ProjectorView.Model.mk but specialized for refractors.
 */
let mk_data =
    (
      ~refractors: Zipper.Refractor.Map.t,
      ~syntax: CachedSyntax.t,
      ~indicated: option(Indicated.piece),
      ~statics: Language.Statics.Map.t,
      ~dynamics: Language.Dynamics.Map.t,
      ~sample_focus: Language.Sample.Focus.t,
      ~editor_active: bool,
    )
    : list(ProjectorView.Model.projector_data) => {
  open Util.OptUtil.Syntax;
  let {measured, term_data, selection_ids, _}: CachedSyntax.t = syntax;
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
      let+ measurement = measurement_of_term(id, term_data, measured);
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
        offside_base:
          ProjectorView.Model.offside_base(
            ~offset=ProjectorView.offside_offset,
            measurement,
            measured,
          ),
        status:
          ProjectorView.Model.mk_status(
            p,
            ~sort=TermData.sort(id, term_data),
            ~editor_active,
            ~indicated,
            ~selection_ids,
            ~info,
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
      ~refractor_shape_map: Id.Map.t(int)=Id.Map.empty,
      refractor_data: list(ProjectorView.Model.projector_data),
      refractor_list: list(Id.t),
    ) => {
  /* A refractor's measurement is collapsed to its anchor point (origin ==
   * last; see `measurement_of_term`), so its vertical extent is invisible
   * to the culling test. In drawer mode the drawer reserves `Tab(n)` rows
   * extending *downward* from the anchor (see ProbeProj.DrawerHeight); that
   * `n` is recorded per-refractor in `refractor_shape_map`. Extend the
   * culled row range's bottom edge by `n` so a multi-line drawer that is
   * still partially on screen isn't disappeared early when its anchor row
   * scrolls past the top of the viewport. */
  let get_row_range = (d: ProjectorView.Model.projector_data) => {
    let drawer_rows =
      Id.Map.find_opt(d.p.id, refractor_shape_map)
      |> Option.value(~default=0);
    (d.measurement.origin.row, d.measurement.last.row + drawer_rows);
  };
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
