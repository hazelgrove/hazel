open Haz3lcorep;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Util;
open Util.OptUtil.Syntax;
open Util.Web;
open Js_of_ocaml;

module Model = {
  type status = {
    kind: ProjectorCore.Kind.t,
    sort: Sort.t,
    indication: option(Direction.t),
    selected: bool,
    error: bool,
  };

  type projector_data('p) = {
    p: Piece.projector('p),
    info: ProjectorBase.info,
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
        p: Base.projector(ProjectorCore.model(ed, ed_a, ed_f)),
        ~editor_active: bool,
        ~indicated: option((Id.t, Direction.t)),
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
      |> ((ProjectorCore.V(kind, _, _)) => ProjectorCore.Kind.of_gadt(kind)),
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
  };

  let mk =
      (
        type p,
        ~mk_status,
        projectors: Id.Map.t(Base.projector(p)),
        measured: Measured.t,
        selection_ids: list(Id.t),
        indicated: option((Id.t, Direction.t)),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        editor_active: bool,
      )
      : list(projector_data(p)) => {
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        let info =
          ProjectorCore.mk_info(
            ~id,
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
    tips: p |> ProjectorNibs.nibs |> ShardDec.tips_of_shapes,
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
    // ~focus: Ui_effect.t(unit),
    (
      ~inject: Action.project('p_k, 'p, 'p_a) => Ui_effect.t(unit),
      id,
      action: external_action,
    ) =>
  switch (action) {
  | Remove => inject(RemoveIndicated)
  | Escape(d) =>
    Ui_effect.Many([
      // TODO(Matt): We need to focus this editor somehow
      // JsUtil.focus_current_target(Js.Unsafe.coerce(evt)),
      // focus,
      inject(Escape(id, d)),
    ])
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
      type ed_m,
      type ed_a,
      type ed_f,
      ~common,
      ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
      ~inject: ProjectorCore.Update.t(ed_a) => Ui_effect.t(unit),
      ~focus: ProjectorCore.Focus.t(ed_f) => Ui_effect.t(unit),
      ~focussed: option(ProjectorCore.Focus.t(ed_f)),
      ~ed_str,
      ~view_ed,
      ~view_editable,
      ~mk_ed,
      {p, info, _}:
        Model.projector_data(ProjectorCore.model(ed_m, ed_a, ed_f)),
    )
    : View.t => {
  let ProjectorCore.V(kind_gadt, model, _) = p.model;
  let methods = ProjectorCore.to_module(kind_gadt);
  let local = a => inject(ProjectorCore.Update.A(kind_gadt, a));
  methods.view(
    ~common,
    ~ed_str,
    ~view_ed,
    ~view_editable,
    ~mk_ed,
    ~local,
    ~parent,
    ~focus=f => focus(F(kind_gadt, f)),
    ~focussed=
      switch (focussed) {
      | Some(F(k, f)) when ProjectorCore.Kind.gadt_eq(k, kind_gadt) =>
        Some(Obj.magic(f)) // Note(Matt): Using Obj.magic here because we know the types are the same if gadt_eq(k, kind_gadt) is true
      | _ => None
      },
    model,
    info,
  );
};

/* Extract and collate different layers of the resulting view
 * in order to stratify z-levels across all projectors */
let split_views =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~common: ProjectorInterface.common,
      ~ed_str,
      ~view_ed,
      ~view_editable,
      ~mk_ed,
      ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
      ~inject: ProjectorCore.Update.t(ed_a) => Ui_effect.t(unit),
      ~focus: ProjectorCore.Focus.t(ed_f) => Ui_effect.t(unit),
      ~focussed: option(ProjectorCore.Focus.t(ed_f)),
      {p, offside_base, measurement, status, _} as projector_data:
        Model.projector_data(ProjectorCore.model(ed, ed_a, ed_f)),
    )
    : (Node.t, option(Node.t)) => {
  let wrapper =
    view_wrapper(~font_metrics=common.font_metrics, ~measurement, ~status);
  let views =
    mk_view(
      ~common,
      ~parent,
      ~inject,
      ~focus,
      ~focussed,
      ~ed_str,
      ~view_ed,
      ~view_editable,
      ~mk_ed,
      projector_data,
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
      ~split_views:
         (
           ~sort: Sort.t,
           ~parent: external_action => Ui_effect.t(unit),
           ~inject: 'p_a => Ui_effect.t(unit),
           ~focus: 'p_f => Ui_effect.t(unit),
           ~focussed: option('p_f),
           Model.projector_data(p)
         ) =>
         (Node.t, option(Node.t)),
      ~inject: Action.t('p_k, p, 'p_a) => Ui_effect.t(unit),
      ~make_active: (Id.t, 'p_f) => Ui_effect.t(unit),
      ~focussed: option((Id.t, 'p_f)),
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
           ~sort=data.status.sort,
           ~parent=
             a => handle(data.info.id, a, ~inject=a => inject(Project(a))),
           ~inject=a => inject(Project(Perform(data.info.id, a))),
           ~focus=
             f =>
               Ui_effect.Many([
                 make_active(data.info.id, f),
                 inject(
                   Project(Focus(data.info.id, data.status.kind, None)),
                 ),
               ]),
           ~focussed=
             switch (focussed) {
             | Some((id2, f)) when id2 == data.info.id => Some(f)
             | _ => None
             },
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
