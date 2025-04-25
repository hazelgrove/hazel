open Haz3lcorep;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Util;
open Util.OptUtil.Syntax;
open Util.Web;

module Model = {
  type status = {
    kind: ProjectorCore.Kind.t,
    sort: Sort.t,
    indication: option(Direction.t),
    selected: bool,
    error: bool,
  };

  type projector_data('ed) = {
    p: Piece.projector(ProjectorCore.model('ed)),
    info: ProjectorBase.info,
    measurement: Measured.measurement,
    offside_base: int,
    status,
  };

  type t('ed) = list(projector_data('ed));

  /* Is projector indicated and if so what side is the caret on? */
  let indication = (p: option(Indicated.piece('ed)), id) =>
    switch (p) {
    | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
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
        p: Base.projector('ed),
        ~editor_active: bool,
        ~indicated: option(Indicated.piece('ed)),
        ~selection_ids: list(Id.t),
        ~info: ProjectorBase.info,
        ~id: Id.t,
      ) => {
    sort:
      Option.map(Info.sort_of, info.statics)
      |> Option.value(~default=Sort.Exp),
    error:
      Option.map(Info.is_error, info.statics) |> Option.value(~default=false),
    kind:
      p.model
      |> ((ProjectorCore.V(kind, _)) => ProjectorCore.Kind.of_gadt(kind)),
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
  };

  let mk =
      (
        projectors: Id.Map.t(Base.projector('ed)),
        measured: Measured.t,
        selection_ids: list(Id.t),
        indicated: option(Indicated.piece('ed)),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        editor_active: bool,
      ) => {
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        let info =
          ProjectorInfo.mk_info(
            p,
            ~statics,
            ~dynamics,
            //~utility=ProjectorInfo.utility,
          );
        {
          p,
          info,
          measurement,
          offside_base: offside_base(~offset=4, measurement, measured),
          status:
            mk_status(
              p,
              ~editor_active,
              ~indicated,
              ~selection_ids,
              ~info,
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
    (~font_metrics: FontMetrics.t, ~measurement: Measured.measurement, p) =>
  ShardDec.relative({
    font_metrics,
    measurement,
    tips: p |> ProjectorBase.shapes |> ShardDec.tips_of_shapes,
  });

/* Adds attributes to a projector UI to support
 * custom styling when selected or indicated */
let projector_clss =
    ({kind, sort, indication, selected, error}: Model.status) =>
  ["projector", ProjectorCore.Kind.name(kind), Sort.show(sort)]
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
      ~inject: Action.t('p) => Ui_effect.t(unit),
      ~make_active,
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~status: Model.status,
      ~id: Id.t,
      ~kind: ProjectorCore.Kind.t,
      views: list(Node.t),
    ) =>
  div(
    ~attrs=[
      Attr.classes(projector_clss(status)),
      /* Stopping propagation here is stops the base editor's
       * drag-select interaction from being triggered */
      Attr.on_pointerdown(_ => {
        Effect.Many([
          Effect.Stop_propagation,
          make_active,
          inject(Project(Focus(id, kind, None))),
        ])
      }),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );

/* Dispatches projector external actions to editor-level actions */
let handle = (id, action: external_action): Action.project('p) =>
  switch (action) {
  | Remove => RemoveIndicated
  | Escape(d) => Escape(id, d)
  //| SetSyntax(f) => SetSyntax(id, f)
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

// let simple_code = (~background=false, font_metrics, sort, segment): Node.t => {
//   let shape_map = ProjectorShape.Map.empty; /* Assume this doesn't contain projectors */
//   let map = Measured.of_segment(segment, shape_map);
//   module Text =
//     Code.Text({
//       // TODO(Matt): text should be abtracted away from projectors
//       type p = Projectors.model;
//       let map = map;
//       let secondary_icons = Settings.Model.init.secondary_icons;
//       let shape_map = shape_map;
//       let font_metrics = font_metrics;
//     });
//   let backing =
//     if (background) {
//       switch (Deco.quick_select_deco(segment)) {
//       | exception _ => []
//       | view => [view]
//       };
//     } else {
//       [];
//     };
//   div(
//     ~attrs=[Attr.class_("code")],
//     [span_c("code-text", Text.of_segment([], false, sort, segment))]
//     @ backing,
//   );
// };

/* Route top-level metadata to the projector view function. */
let mk_view =
    (
      inject: Action.t('p) => Ui_effect.t(unit),
      ~ed_str,
      ~view_any,
      _font_metrics: FontMetrics.t,
      {p, info, _}: Model.projector_data('ed),
    )
    : View.t => {
  let ProjectorCore.V(kind_gadt, model) = p.model;
  let methods = ProjectorInit.to_module(kind_gadt);
  let parent = a => inject(Project(handle(p.id, a)));
  let local = a =>
    inject(
      Project(
        SetModel(
          p.id,
          ProjectorCore.V(kind_gadt, methods.update(model, info, a)),
        ),
      ),
    );
  methods.view(~ed_str, ~view_any, model, info, ~local, ~parent);
};

/* Extract and collate different layers of the resulting view
 * in order to stratify z-levels across all projectors */
let split_views =
    (
      inject: Action.t('p) => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      ~ed_str,
      ~view_any,
      {p, offside_base, measurement, status, _} as projector_data:
        Model.projector_data('ed),
    )
    : (Node.t, option(Node.t)) => {
  let wrapper =
    view_wrapper(
      ~inject,
      ~make_active,
      ~font_metrics,
      ~measurement,
      ~status,
      ~id=p.id,
      ~kind=p.model |> ProjectorCore.kind_of_model,
    );
  let views =
    mk_view(~ed_str, ~view_any, inject, font_metrics, projector_data);
  let line_view = {
    let offside_view =
      views.offside
      |> Option.map(offside_wrapper(font_metrics, offside_base))
      |> Option.to_list;
    wrapper(
      [views.inline]
      @ [backing_deco(~font_metrics, ~measurement, p)]
      @ offside_view,
    );
  };
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
      inject: Action.t(ProjectorCore.model('ed)) => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      projector_data: list(Model.projector_data('ed)),
      ~ed_str,
      ~view_any,
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
    |> List.map(
         split_views(~ed_str, ~view_any, inject, make_active, font_metrics),
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
