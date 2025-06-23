open Haz3lcorep;
open Virtual_dom.Vdom;
open Node;
open ProjectorInterface;
open Util;
open Util.OptUtil.Syntax;
open Util.WebUtil;

module Model = {
  type status = {
    kind: ProjectorKind.t,
    sort: Sort.t,
    indication: option(Direction.t),
    selected: bool,
    error: bool,
  };

  type projector_data('p) = {
    p: Piece.projector('p),
    id: Id.t,
    measurement: Measured.measurement,
    offside_base: int,
    status,
  };

  type t('ed) = list(projector_data('ed));

  /* Is projector indicated and if so what side is the caret on? */
  let indication = (p, id) =>
    switch (p) {
    | Some((id2, d)) when id2 == id => Some(Direction.toggle(d))
    | _ => None
    };

  /* Find the end of row offset position in grid units */
  let offside_base =
      (~offset: int, measurement: Measured.measurement, measured: Measured.t)
      : int =>
    Measured.start_row_width(measurement, measured)
    + offset
    - measurement.origin.col;

  let mk_status =
      (
        type ed,
        type ed_a,
        type ed_f,
        p: Base.projector(Projector.model(ed, ed_a, ed_f)),
        ~common: Common.t,
        ~editor_active: bool,
        ~indicated: option((Id.t, Direction.t)),
        ~selection_ids: list(Id.t),
        ~id: Id.t,
      ) => {
    /* If statics is not available, use the sort that's on the mold,
     * which should be accurate as of projector intialization time,
     * but may not have been updated if the grammatical context of
     * the projector has changed due to remolding or being copied */
    sort:
      Option.map(
        Language.Info.sort_of,
        Language.Statics.Map.lookup(id, common.statics.info_map),
      )
      |> Option.value(~default=p.mold.out),
    error:
      Option.map(
        Language.Info.is_error,
        Language.Statics.Map.lookup(id, common.statics.info_map),
      )
      |> Option.value(~default=false),
    kind: p.model |> Projector.kind_of_model,
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
  };

  let mk =
      (
        type p,
        ~mk_status,
        ~common,
        projectors: Id.Map.t(Base.projector(p)),
        measured: Measured.t,
        selection_ids: list(Id.t),
        indicated: option((Id.t, Direction.t)),
        editor_active: bool,
      )
      : list(projector_data(p)) => {
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        {
          p,
          id,
          measurement,
          offside_base: offside_base(~offset=4, measurement, measured),
          status:
            mk_status(
              p,
              ~common,
              ~editor_active,
              ~indicated,
              ~selection_ids,
              ~id,
            ),
        };
      },
      Id.Map.bindings(projectors),
    );
  };
};

/* Projectors get a default backing decoration similar
 * to token decorations. This can be made transparent
 * in the CSS if no backing is wanted */
let backing_deco =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      p: Base.projector('p),
    ) =>
  ShardDec.relative({
    font_metrics,
    measurement,
    tips: p.mold.nibs |> Nibs.shapes |> ShardDec.tips_of_shapes,
  });

/* Adds attributes to a projector UI to support
 * custom styling when selected or indicated */
let projector_clss =
    ({kind, sort, indication, selected, error}: Model.status) =>
  [
    "projector",
    ProjectorKind.name(kind) |> String.lowercase_ascii,
    Sort.show(sort),
  ]
  @ (selected ? ["selected"] : [])
  @ (error ? ["error"] : [])
  @ (
    switch (indication) {
    | Some(d) => ["indicated", Direction.show(d)]
    | None => []
    }
  );

/* Wraps the view function for a projector, absolutely positioning
 * relative to the syntax, adding a default backing decoration, and
 * adding fallthrough handlers where appropriate*/
let view_wrapper =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~status: Model.status,
      views: list(Node.t),
    ) =>
  div(
    ~attrs=[
      Attr.classes(projector_clss(status)),
      /* Stopping propagation here is stops the base editor's
       * drag-select interaction from being triggered */
      Attr.on_pointerdown(_ => {Effect.Many([Effect.Stop_propagation])}),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );

/* Dispatches projector external actions to editor-level actions */
let handle =
    (
      ~focus: Ui_effect.t(unit),
      ~inject: Action.project('p_k, 'p, 'p_a) => Ui_effect.t(unit),
      id,
      action: external_action,
    ) =>
  switch (action) {
  | Remove => inject(RemoveIndicated)
  | Escape(d) => Ui_effect.Many([focus, inject(Escape(id, d))])
  };

let offside_wrapper =
    (font_metrics: FontMetrics.t, offside_base: int, v: Node.t) =>
  div(
    ~attrs=[
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx;",
          font_metrics.col_width *. float_of_int(offside_base),
        ),
      ),
    ],
    [v],
  );

/* Extract and collate different layers of the resulting view
 * in order to stratify z-levels across all projectors */
let split_views =
    (
      type p_m,
      type p_a,
      type p_f,
      ~view_projector,
      ~common: Common.t,
      ~inject: p_a => Ui_effect.t(unit),
      ~escape: external_action => Ui_effect.t(unit),
      ~take_focus: p_f => Ui_effect.t(unit),
      ~focus: option(p_f),
      ~handoff_map:
         Hashtbl.t(Id.t, (Ui_effect.t(unit), Ui_effect.t(unit))),
      {p, offside_base, measurement, status, _} as projector_data:
        Model.projector_data(p_m),
    )
    : (Node.t, option(Node.t)) => {
  let wrapper =
    view_wrapper(~font_metrics=common.font_metrics, ~measurement, ~status);
  let views: ProjectorInterface.View.t =
    view_projector(
      ~common,
      ~inject,
      ~escape,
      ~take_focus,
      ~focus,
      ~info=
        ProjectorInterface.mk_info(~id=projector_data.id, ~sort=status.sort),
      projector_data.p.model,
    );
  let line_view = {
    let offside_view =
      views.offside
      |> Option.map(offside_wrapper(common.font_metrics, offside_base))
      |> Option.to_list;
    wrapper(
      [views.inline]
      @ [backing_deco(~font_metrics=common.font_metrics, ~measurement, p)]
      @ offside_view,
    );
  };
  let enter_left =
    switch (views.enter_left) {
    | Some(v) => v
    | None => escape(Escape(Right))
    };
  let enter_right =
    switch (views.enter_right) {
    | Some(v) => v
    | None => escape(Escape(Left))
    };
  Hashtbl.add(handoff_map, projector_data.id, (enter_left, enter_right));
  let overlay_view = Option.map(v => wrapper([v]), views.overlay);
  (line_view, overlay_view);
};

/* Is the piece with id indicated? If so, where is it wrt the caret? */
let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

let by_measurement =
    (pd1: Model.projector_data('ed), pd2: Model.projector_data('ed)) =>
  compare(pd1.measurement.origin.row, pd2.measurement.origin.row);

/* Returns a div containing all projector UIs, intended to
 * be absolutely positioned atop a rendered editor UI */
let all =
    (
      type p,
      ~common,
      ~view_projector,
      ~inject: Action.t('p_k, p, 'p_a) => Ui_effect.t(unit),
      ~make_active: (Id.t, 'p_f) => Ui_effect.t(unit),
      ~focus,
      ~focussed: option((Id.t, 'p_f)),
      ~handoff_map:
         Hashtbl.t(Id.t, (Ui_effect.t(unit), Ui_effect.t(unit))),
      projector_data: list(Model.projector_data(p)),
    ) => {
  /* Sorting the projectors by position tends to be a good
   * z-index default; projectors further to the right or
   * further down count as a higher. On its own this could
   * impinge on hover-dropdowns, but the hovered projector
   * has z-index handled separately. But ideally dropdowns
   * should be on the overlay layer so this doesn't come up */
  let (base_views, overlay_views) =
    projector_data
    |> List.sort(by_measurement)
    |> List.map((data: Model.projector_data(p)) =>
         split_views(
           ~view_projector,
           ~common,
           ~inject=a => inject(Project(Perform(data.id, a))),
           ~escape=
             a =>
               handle(data.id, a, ~focus, ~inject=a => inject(Project(a))),
           ~take_focus=f => make_active(data.id, f),
           ~focus=
             switch (focussed) {
             | Some((id2, f)) when id2 == data.id => Some(f)
             | _ => None
             },
           ~handoff_map,
           data,
         )
       )
    |> List.split;
  let overlay_views = List.filter_map(Fun.id, overlay_views);
  [
    div_c(
      "projectors",
      [div_c("base", base_views), div_c("overlays", overlay_views)],
    ),
  ];
};
