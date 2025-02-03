open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Util;
open Util.OptUtil.Syntax;
open Util.Web;

type status = {
  kind: ProjectorCore.kind,
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

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: ProjectorCore.kind): string =>
  switch (p) {
  | Fold => "fold"
  | Info => "type"
  | Probe => "probe"
  | Checkbox => "check"
  | Slider => "slider"
  | SliderF => "sliderf"
  | TextArea => "text"
  };

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): ProjectorCore.kind =>
  switch (p) {
  | "fold" => Fold
  | "type" => Info
  | "probe" => Probe
  | "check" => Checkbox
  | "slider" => Slider
  | "sliderf" => SliderF
  | "text" => TextArea
  | _ => failwith("Unknown projector kind")
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
let projector_clss = ({kind, sort, indication, selected, error}: status) =>
  ["projector", name(kind), Sort.show(sort)]
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
      ~inject: Action.t => Ui_effect.t(unit),
      ~make_active,
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~info: info,
      ~status: status,
      views: list(Node.t),
    ) =>
  div(
    ~attrs=[
      Attr.classes(projector_clss(status)),
      /* Stopping propagation here is stops the base editor's
       * drag-select interaction from being triggered */
      Attr.on_pointerdown(_ =>
        Effect.Many([
          Effect.Stop_propagation,
          make_active,
          inject(Project(Focus(info.id, None))),
        ])
      ),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );

/* Dispatches projector external actions to editor-level actions */
let handle = (id, action: external_action): Action.project =>
  switch (action) {
  | Remove => RemoveIndicated
  | Escape(d) => Escape(id, d)
  | SetSyntax(f) => SetSyntax(id, f)
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

let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

/* Find end of row offset position in grid units */
let offside_base =
    (~offset: int, measurement: Measured.measurement, measured: Measured.t)
    : int =>
  Measured.start_row_width(measurement, measured)
  + offset
  - measurement.origin.col;

let collect_data =
    (
      cached_syntax: Editor.CachedSyntax.t,
      zipper,
      cached_statics: CachedStatics.t,
      dynamics,
      editor_active: bool,
    ) => {
  let projector_ids = cached_syntax.projectors |> Id.Map.bindings |> List.rev;
  List.filter_map(
    ((id, _)) => {
      let* p = Id.Map.find_opt(id, cached_syntax.projectors);
      let+ measurement = Measured.find_pr_opt(p, cached_syntax.measured);
      let info =
        ProjectorInfo.mk_info(p, ~statics=cached_statics.info_map, ~dynamics);
      let sort =
        Option.map(Info.sort_of, info.statics)
        |> Option.value(~default=Sort.Exp);
      let error =
        Option.value(
          ~default=false,
          Option.map(Info.is_error, info.statics),
        );
      {
        p,
        info,
        measurement,
        offside_base:
          offside_base(~offset=4, measurement, cached_syntax.measured),
        status: {
          sort,
          error,
          kind: p.kind,
          indication: editor_active ? indication(zipper, id) : None,
          selected:
            editor_active ? List.mem(id, cached_syntax.selection_ids) : false,
        },
      };
    },
    projector_ids,
  );
};

let simple_code_view =
    (~background=false, font_metrics, sort, segment): Node.t => {
  /* Assume this doesn't contain projectors */
  let shape_of_proj = ProjectorInfo.Shape.of_map_default;
  let map = Measured.of_segment(segment, shape_of_proj);
  module Text =
    Code.Text({
      let map = map;
      let settings = Settings.Model.init;
      let shape_of_proj = shape_of_proj;
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

/* Extracts projector-instance-specific metadata necessary to
 * render the view, instantiates appropriate action handlers,
 * renders the view, and then wraps it so as to position it
 * correctly with respect to the underyling editor */
let setup_view =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      {p, info, offside_base, measurement, status}: projector_data,
    )
    : (Node.t, Node.t, option(Node.t)) => {
  let (module P) = ProjectorInit.to_module(p.kind);
  let parent = a => inject(Project(handle(p.id, a)));
  let local = a =>
    inject(Project(SetModel(p.id, P.update(p.model, info, a))));
  let view_seg = (~background=false, sort, seg) =>
    simple_code_view(~background, font_metrics, sort, seg);
  let wrapper =
    view_wrapper(
      ~inject,
      ~make_active,
      ~font_metrics,
      ~measurement,
      ~status,
      ~info,
    );
  let inline_view = P.view(p.model, info, ~local, ~parent, ~view_seg);
  let offside_view =
    Option.map(
      v =>
        offside_wrapper(
          font_metrics,
          offside_base,
          v(p.model, info, ~local, ~parent, ~view_seg),
        ),
      P.offside_view,
    );
  let overlay_view =
    Option.map(
      v => wrapper([v(p.model, info, ~local, ~parent, ~view_seg)]),
      P.overlay_view,
    );
  let underlay_view =
    switch (P.underlay_view) {
    | Some(v) => wrapper([v(p.model, info, ~view_seg)])
    | None => wrapper([backing_deco(~font_metrics, ~measurement, p)])
    };
  let combined_view = wrapper([inline_view] @ Option.to_list(offside_view));
  (underlay_view, combined_view, overlay_view);
};

/* Is the piece with id indicated? If so, where is it wrt the caret? */
let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

/* Returns a div containing all projector UIs, intended to
 * be absolutely positioned atop a rendered editor UI */
let all =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      projector_data: list(projector_data),
    ) => {
  let (underlay_views, base_views, overlay_views) =
    projector_data
    |> List.map(setup_view(inject, make_active, font_metrics))
    |> ListUtil.split3;
  let overlay_views = overlay_views |> List.filter_map(Fun.id);
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
