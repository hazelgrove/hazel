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
      ~refractors: Id.Map.t(Base.projector),
      ~syntax: CachedSyntax.t,
      ~indicated: option(Indicated.piece),
      ~statics: Language.Statics.Map.t,
      ~dynamics: Language.Dynamics.Map.t,
      ~dyn_cursor: Language.Sample.Cursor.t,
      ~editor_active: bool,
    )
    : list(ProjectorView.Model.projector_data) => {
  open Util.OptUtil.Syntax;
  let {measured, term_data, selection_ids, _}: CachedSyntax.t = syntax;
  List.filter_map(
    ((id, _)) => {
      let* p = Id.Map.find_opt(id, refractors);
      let+ measurement = measurement_of_term(id, term_data, measured);
      let info = ProjectorInfo.mk_info(p, ~dyn_cursor, ~statics, ~dynamics);
      ProjectorView.Model.{
        p,
        info,
        measurement,
        offside_base:
          ProjectorView.Model.offside_base(~offset=4, measurement, measured),
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
      ~visible: option(Globals.VisibleRows.t)=?,
      refractor_data: list(ProjectorView.Model.projector_data),
    ) => {
  let get_row = (d: ProjectorView.Model.projector_data) =>
    d.measurement.origin.row;
  let (base_views, overlay_views) =
    refractor_data
    |> ProjectorView.filter_by_visibility(visible, _, get_row)
    |> List.sort(ProjectorView.by_measurement)
    |> List.map(
         ProjectorView.split_views(
           ~skip_inline=true,
           inject,
           make_active,
           font_metrics,
         ),
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
