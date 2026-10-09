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

/* A proof drawer sits under its theorem's own line (the end of the
   tile's last shard, `in`), where Measured reserves its rows; the
   theorem term itself runs on to the program's end */
let measurement_of_tile =
    (id: Id.t, measured: Measured.t): option(Measured.measurement) =>
  switch (Measured.find_shards_by_id(id, measured)) {
  | Some([_, ..._] as shards) =>
    let last =
      List.fold_left(
        (acc: Util.Point.t, (_, m: Measured.measurement)) =>
          Util.Point.compare(m.last, acc) > 0 ? m.last : acc,
        Util.Point.zero,
        shards,
      );
    Some({
      origin: last,
      last,
    });
  | _ => None
  };

/* the ⇓ probe's drawer: after the program's last line, trailing blank
   lines included */
let measurement_of_end = (measured: Measured.t): Measured.measurement => {
  let last =
    Util.Point.{
      row: max(0, measured.total_rows - 1),
      col: 0,
    };
  {
    origin: last,
    last,
  };
};

/* Build refractor data from editor state.
 * This is analogous to ProjectorView.Model.mk but specialized for refractors.
 */
/* visible rows of a refractor: anchor rows extended down by drawer height
 * (Tab(n) in refractor_rows, keyed by CachedSyntax.rows_key), so a
 * partially-visible drawer isn't culled early. The program's drawer
 * reserves no rows and may run on for screens: never culled once reached */
let row_range =
    (
      ~refractor_rows: Id.Map.t(int),
      ~term_data: TermData.t,
      ~program_value: bool,
      id: Id.t,
      measurement: Measured.measurement,
    )
    : (int, int) =>
  if (program_value) {
    (measurement.origin.row, max_int);
  } else {
    let drawer_rows =
      Id.Map.find_opt(CachedSyntax.rows_key(id, term_data), refractor_rows)
      |> Option.value(~default=0);
    (measurement.origin.row, measurement.last.row + drawer_rows);
  };

let mk_data =
    (
      ~refractors: Zipper.Refractor.Map.t,
      ~syntax: CachedSyntax.t,
      ~indicated: option(Indicated.piece),
      ~statics: Language.Statics.Map.t,
      ~dynamics: Language.Dynamics.Map.t,
      ~sample_focus: Language.Sample.Focus.t,
      ~stepping: option(ProjectorBase.stepping)=None,
      ~editor_active: bool,
      ~visible: option(Globals.VisibleRows.t)=?,
      ~refractor_rows: Id.Map.t(int)=Id.Map.empty,
      ~tail: option(Id.t)=None,
      (),
    )
    : list(ProjectorView.Model.projector_data) => {
  let {measured, term_data, selection_ids, _}: CachedSyntax.t = syntax;
  /* measure + cull BEFORE building per-refractor data: in All mode there are
   * hundreds of refractors but few on screen, so building all then discarding dominated cost */
  let program_value = (id, entry: Refractors.entry) =>
    tail == Some(id) && entry.kind == Probe && ProbeProj.is_bare(entry.model);
  Id.Map.bindings(refractors)
  |> List.filter_map(((id, entry: Refractors.entry)) =>
       (
         if (entry.kind == Proof) {
           measurement_of_tile(id, measured);
         } else if (program_value(id, entry)) {
           Some(measurement_of_end(measured));
         } else {
           measurement_of_term(id, term_data, measured);
         }
       )
       |> Option.map(measurement => (id, entry, measurement))
     )
  |> ProjectorView.filter_by_visibility(
       visible, _, ((id, entry, measurement)) =>
       row_range(
         ~refractor_rows,
         ~term_data,
         ~program_value=program_value(id, entry),
         id,
         measurement,
       )
     )
  |> List.map(((id, entry, measurement)) => {
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
       let info =
         ProjectorInfo.mk_info(
           p,
           ~sample_focus,
           ~statics,
           ~dynamics,
           ~elaborated=None,
           ~stepping,
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
     });
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
      ~refractor_rows: Id.Map.t(int)=Id.Map.empty,
      ~term_data: TermData.t,
      ~tail: option(Id.t)=None,
      refractor_data: list(ProjectorView.Model.projector_data),
      refractor_list: list(Id.t),
    ) => {
  /* usually a no-op (mk_data already culls); kept for callers without visibility info */
  let get_row_range = (d: ProjectorView.Model.projector_data) =>
    row_range(
      ~refractor_rows,
      ~term_data,
      ~program_value=
        tail == Some(d.p.id)
        && d.p.kind == Probe
        && ProbeProj.is_bare(d.p.model),
      d.p.id,
      d.measurement,
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
