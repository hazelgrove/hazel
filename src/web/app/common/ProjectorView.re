open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Util;
open Util.OptUtil.Syntax;
open Util.WebUtil;

/* Re-export visible_rows type from Globals for convenience */
type visible_rows = Globals.VisibleRows.t;

let offside_offset = 4; /* Num characters offset to the right of the end of the line */

/* Filter projector data to only include items in visible row range */
let filter_by_visibility =
    (visible: option(visible_rows), data: list('a), get_row: 'a => int)
    : list('a) =>
  switch (visible) {
  | None => data
  | Some({first, last}) =>
    List.filter(
      item => {
        let row = get_row(item);
        row >= first && row <= last;
      },
      data,
    )
  };

module Model = {
  type status = ProjectorBase.View.status;

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
        ~sort: Sort.t,
      )
      : status => {
    sort,
    error:
      Option.map(Language.Info.is_error, info.statics)
      |> Option.value(~default=false),
    kind: p.kind,
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
  };

  let mk =
      (
        ~syntax: CachedSyntax.t,
        ~indicated: option(Indicated.piece),
        ~statics: Language.Statics.Map.t,
        ~dynamics: Language.Dynamics.Map.t,
        ~sample_cursor: Language.Sample.Cursor.t,
        ~editor_active: bool,
      ) => {
    let {projectors, measured, term_data, selection_ids, _}: CachedSyntax.t = syntax;
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        let info =
          ProjectorInfo.mk_info(p, ~sample_cursor, ~statics, ~dynamics);
        {
          p,
          info,
          measurement,
          offside_base:
            offside_base(~offset=offside_offset, measurement, measured),
          status:
            mk_status(
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
    tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
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
      ~inject: Action.t => Ui_effect.t(unit),
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
      /* Stopping propagation here stops the base editor's
       * drag-select interaction from being triggered.
       * However, we let right-clicks bubble through so the
       * context menu can be shown. */
      Attr.on_pointerdown(evt =>
        switch (Pointer.Event.mk(evt)) {
        | {button: Right, _} => Effect.Ignore /* Let right-clicks bubble for context menu */
        | _ =>
          Effect.Many([
            Effect.Stop_propagation,
            make_active,
            inject(Project(Focus(id, kind, None))),
          ])
        }
      ),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );

/* Dispatches projector external actions to editor-level actions */
let handle = (id, action: external_action): Action.t =>
  switch (action) {
  | Remove => Project(RemoveIndicated)
  | Escape(d) => Project(Escape(id, d))
  | SetSyntax(f) => Project(SetSyntax(id, f))
  | SampleCursor(sc) => Project(SampleCursor(sc))
  | Probe(p) => Probe(p)
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

let simple_code =
    (~background=false, ~is_single_line=false, font_metrics, _sort, segment)
    : Node.t => {
  let shape_map = ProjectorCore.Shape.Map.empty; /* Assume this doesn't contain projectors */
  let refractor_shape_map = Id.Map.empty; /* Assume this doesn't contain refractors (probes) */
  let measured =
    Measured.of_segment(~is_single_line, segment, shape_map, Id.Map.empty);
  let code =
    Code.view(
      ~measured,
      ~settings=Settings.Model.init,
      ~shape_map,
      ~refractor_shape_map,
      ~font_metrics,
      ~term_data=Id.Map.empty,
      ~buffer_ids=[],
      segment,
    );
  let backing =
    if (background) {
      switch (
        Highlight.of_segment(
          ~measured,
          ~shape_map,
          ~font_metrics,
          ~shape_init=Some(Convex),
          ~clss=[],
          segment,
        )
      ) {
      | exception _ => []
      | view => view
      };
    } else {
      [];
    };
  div(
    ~attrs=[Attr.class_("code")],
    [span_c("code-text", code)] @ [div_c("quick-select-deco", backing)],
  );
};

let text_code = (segment): Node.t =>
  div(
    ~attrs=[Attr.class_("code")],
    [
      span_c(
        "code-text",
        [
          div(
            ~attrs=[Attr.classes(["token", "Exp"])],
            [
              Node.text(
                Printer.of_segment(
                  ~holes="?",
                  ~indent="",
                  ~is_single_line=true,
                  segment,
                ),
              ),
            ],
          ),
        ],
      ),
    ],
  );

let flex_code =
    (
      ~font_metrics,
      ~single_line=false, /* Perf optimization if you promise it's single-line */
      ~background=?,
      ~text_only=false,
      sort,
      segment,
    ) =>
  text_only
    ? text_code(segment)
    : simple_code(
        ~background?,
        ~is_single_line=single_line,
        font_metrics,
        sort,
        segment,
      );

/* Route top-level metadata to the projector view function. */
let mk_view =
    (
      inject: Action.t => Ui_effect.t(unit),
      font_metrics: FontMetrics.t,
      {p, info, status, _}: Model.projector_data,
    )
    : View.t => {
  let (module P) = ProjectorInit.to_module(p.kind);
  P.view({
    model: p.model,
    info,
    local: a =>
      inject(Project(SetModel(p.id, P.update(p.model, info, a)))),
    parent: a => inject(handle(p.id, a)),
    view_seg: (~single_line=?, ~background=?, ~text_only=?, sort, segment) =>
      flex_code(
        ~font_metrics,
        ~single_line?,
        ~background?,
        ~text_only?,
        sort,
        segment,
      ),
    status,
  });
};

/* Extract and collate different layers of the resulting view
 * in order to stratify z-levels across all projectors */
let split_views =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      ~skip_inline: bool,
      {p, offside_base, measurement, status, _} as projector_data: Model.projector_data,
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
      ~kind=p.kind,
    );
  let views = mk_view(inject, font_metrics, projector_data);
  let line_view = {
    let offside_view =
      views.offside
      |> Option.map(offside_wrapper(font_metrics, offside_base))
      |> Option.to_list;
    wrapper(
      (skip_inline ? [] : [views.inline])
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

let by_measurement = (pd1: Model.projector_data, pd2: Model.projector_data) =>
  compare(pd1.measurement.origin.row, pd2.measurement.origin.row);

/* Returns a div containing all projector UIs, intended to
 * be absolutely positioned atop a rendered editor UI */
let all =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      ~visible: option(visible_rows)=?,
      projector_data: list(Model.projector_data),
    ) => {
  /* Sorting the projectors by position tends to be a good
   * z-index default; projectors further to the right or
   * further down count as a higher. On its own this could
   * impinge on hover-dropdowns, but the hovered projector
   * has z-index handled separately. But ideally dropdowns
   * should be on the overlay layer so this doesn't come up */
  let get_row = (d: Model.projector_data) => d.measurement.origin.row;
  let (base_views, overlay_views) =
    projector_data
    |> filter_by_visibility(visible, _, get_row)
    |> List.sort(by_measurement)
    |> List.map(
         split_views(~skip_inline=false, inject, make_active, font_metrics),
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
    P.focusable.keyboard != None
      ? Some(Focus(id, kind, Some(Right))) : None;
  | (Some(Right), (_, Some(Projector({id, kind, _})))) =>
    let (module P) = ProjectorInit.to_module(kind);
    P.focusable.keyboard != None ? Some(Focus(id, kind, Some(Left))) : None;
  | _ => None
  };
};
