open Haz3lcore;
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

  type projector_data = {
    p: Piece.projector,
    info: ProjectorBase.info,
    measurement: Measured.measurement,
    offside_base: int,
    status,
  };

  type t = list(projector_data);

  /* Is projector indicated and if so what side is the caret on? */
  let indication = (p: option(Indicated.piece), id) =>
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
        p: Base.projector,
        ~editor_active: bool,
        ~indicated: option(Indicated.piece),
        ~selection_ids: list(Id.t),
        ~info: ProjectorBase.info,
        ~id: Id.t,
      ) => {
    sort:
      Option.map(Info.sort_of, info.statics)
      |> Option.value(~default=Sort.Exp),
    error:
      Option.map(Info.is_error, info.statics) |> Option.value(~default=false),
    kind: p.kind,
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
  };

  let mk =
      (
        projectors: Id.Map.t(Base.projector),
        measured: Measured.t,
        selection_ids: list(Id.t),
        indicated: option(Indicated.piece),
        statics: Statics.Map.t,
        dynamics: Dynamics.Map.t,
        editor_active: bool,
      ) => {
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        let info = ProjectorInfo.mk_info(p.id, p.syntax, ~statics, ~dynamics);
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

let simple_code = (~background=false, font_metrics, sort, segment): Node.t => {
  let shape_map = ProjectorCore.Shape.Map.empty; /* Assume this doesn't contain projectors */
  let map = Measured.of_segment(segment, shape_map);
  module Text =
    Code.Text({
      let map = map;
      let settings = Settings.Model.init;
      let shape_map = shape_map;
      let font_metrics = font_metrics;
    });
  let backing =
    if (background) {
      switch (Deco.quick_select_deco(segment)) {
      | exception _ => []
      | view => [view]
      };
    } else {
      [];
    };
  div(
    ~attrs=[Attr.class_("code")],
    [span_c("code-text", Text.of_segment([], false, sort, segment))]
    @ backing,
  );
};

/* Is the piece with id indicated? If so, where is it wrt the caret? */
let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

let by_measurement = (pd1: Model.projector_data, pd2: Model.projector_data) =>
  compare(pd1.measurement.origin.row, pd2.measurement.origin.row);

let view_wrapper =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~status: Model.status,
      ~focus,
      views: list(Node.t),
    )
    : Node.t =>
  Node.div(
    ~attrs=[
      Attr.classes(projector_clss(status)),
      /* Stopping propagation here is stops the base editor's
       * drag-select interaction from being triggered */
      Attr.on_pointerdown(_ =>
        Effect.Many([
          Effect.Stop_propagation,
          //make_active,
          focus,
          //inject(Project(Focus(info.id, None))),
        ])
      ),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );
let split_views =
    (
      font_metrics: FontMetrics.t,
      f: Id.t => ProjectorBase.View.t,
      ~focus,
      {p, offside_base, measurement, status, _}: Model.projector_data,
    )
    : (Node.t, Node.t, option(Node.t)) => {
  let wrapper = view_wrapper(~focus, ~font_metrics, ~measurement, ~status);
  let views = f(p.id);
  switch (views) {
  | Tylr(_) => failwith("Tylrlmao")
  | Pro(views) =>
    let line_view = {
      let inline_view = Option.to_list(views.inline);
      let offside_view =
        views.offside
        |> Option.map(offside_wrapper(font_metrics, offside_base))
        |> Option.to_list;
      wrapper(inline_view @ offside_view);
    };
    let overlay_view = Option.map(v => wrapper([v]), views.overlay);
    let underlay_view =
      switch (views.underlay) {
      | Some(v) => wrapper([v])
      | None => wrapper([backing_deco(~font_metrics, ~measurement, p)])
      };

    (underlay_view, line_view, overlay_view);
  };
};
let all =
    (
      ~focus: Ui_effect.t(unit),
      f,
      font_metrics: FontMetrics.t,
      projector_data: list(Model.projector_data),
    ) => {
  /* Sorting the projectors by position tends to be a good
   * z-index default; projectors further to the right or
   * further down count as a higher. On its own this could
   * impinge on hover-dropdowns, but the hovered projector
   * has z-index handled separately. But ideally dropdowns
   * should be on the overlay layer so this doesn't come up */
  let (underlay_views, base_views, overlay_views) =
    projector_data
    |> List.sort(by_measurement)
    |> List.map(split_views(font_metrics, f, ~focus))
    |> ListUtil.split3;
  let overlay_views = List.filter_map(Fun.id, overlay_views);
  [
    div_c(
      "projectors",
      [
        div_c("base", base_views),
        div_c("underlays", underlay_views),
        div_c("overlays", overlay_views),
      ],
    ),
  ];
};

let move_dir = (key: Key.t): option(Direction.t) =>
  switch (key) {
  | {key: D("ArrowLeft"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
    Some(Left)
  | {key: D("ArrowRight"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
    Some(Right)
  | _ => None
  };

/* When the caret is directly adjacent to a projector, keyboard commands
 * can be overidden here. Right now, trying to move into the projector,
 * that is, pressing left when it's to the right or vice-versa, without
 * holding down a modifier, will give the projector focus (if its can_focus)
 * flag is set. Be conservative about these kind of overloads; you need
 * to consider how they interact with all the editor keyboard commands.
 * For example, without the modifiers check, this would break selection
 * around a projector. */
let key_handoff = (editor: Editor.t, key: Key.t): option(Action.project) => {
  let z = editor.state.zipper;
  switch (
    move_dir(key),
    Siblings.neighbors(editor.state.zipper.relatives.siblings),
  ) {
  | _ when z.caret != Outer => None
  | (Some(Left), (Some(Projector({id, kind, _})), _)) =>
    let (module P) = ProjectorInit.to_module(kind);
    P.can_focus ? Some(Focus(id, Some(Right))) : None;
  | (Some(Right), (_, Some(Projector({id, kind, _})))) =>
    let (module P) = ProjectorInit.to_module(kind);
    P.can_focus ? Some(Focus(id, Some(Left))) : None;
  | _ => None
  };
};
