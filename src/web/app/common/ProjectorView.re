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

/* Cache projector view results to avoid expensive P.view() calls
 * when statics/dynamics haven't changed (e.g. during debounced typing).
 * Per-projector cache keyed on map identity + status + model. Handles
 * multiple editors calling into the same cache without thrashing. */
module ViewCache = {
  type entry = {
    statics_map: Language.Statics.Map.t,
    dynamics_map: Language.Dynamics.Map.t,
    sample_focus: Language.Sample.Focus.t,
    elaborated: option(Language.Exp.t),
    core_settings: Language.CoreSettings.t,
    settings_version: int,
    /* The MVU app store lives in the web model, out of reach here; app
     * projectors read it through AppBridge, so its counter is the cache's
     * only handle on "the app state changed". Zero for kinds that don't
     * read the store, so a ticking app doesn't invalidate every projector. */
    app_version: int,
    /* Views close over the cell size (view_seg renders code with it, and
     * projectors that resize themselves convert pixels with it), so a zoom
     * or font-size change has to expire the entry. */
    font_metrics: FontMetrics.t,
    status: View.status,
    model: string,
    view: View.t,
  };
  let cache: Hashtbl.t(Id.t, entry) = Hashtbl.create(64);

  let lookup =
      (
        id,
        ~statics_map,
        ~dynamics_map,
        ~sample_focus,
        ~elaborated,
        ~core_settings,
        ~status,
        ~model,
        ~app_version,
        ~font_metrics,
      )
      : option(View.t) =>
    switch (Hashtbl.find_opt(cache, id)) {
    | Some(e)
        when
          e.statics_map === statics_map
          && e.dynamics_map === dynamics_map
          && Language.Sample.Focus.equal(e.sample_focus, sample_focus)
          && CachedSyntax.elaborated_phys_eq(e.elaborated, elaborated)
          && e.core_settings == core_settings
          && e.settings_version == ProbeProj.Settings.version^
          && e.app_version == app_version
          && e.font_metrics == font_metrics
          && e.status == status
          && e.model == model =>
      Some(e.view)
    | _ => None
    };

  let store =
      (
        id,
        ~statics_map,
        ~dynamics_map,
        ~sample_focus,
        ~elaborated,
        ~core_settings,
        ~status,
        ~model,
        ~app_version,
        ~font_metrics,
        ~view,
      ) =>
    Hashtbl.replace(
      cache,
      id,
      {
        statics_map,
        dynamics_map,
        sample_focus,
        elaborated,
        core_settings,
        settings_version: ProbeProj.Settings.version^,
        app_version,
        font_metrics,
        status,
        model,
        view,
      },
    );

  let hits = ref(0);
  let misses = ref(0);
  let log_frame = () => {
    hits := 0;
    misses := 0;
  };
};

/* Filter projector data to only include items in visible row range.
 * For multi-line projectors (like large text areas), we check if ANY part
 * of the projector overlaps with the visible range, not just the origin. */
let filter_by_visibility =
    (
      visible: option(visible_rows),
      data: list('a),
      get_row_range: 'a => (int, int),
    )
    : list('a) =>
  switch (visible) {
  | None => data
  | Some({first, last}) =>
    List.filter(
      item => {
        let (origin_row, last_row) = get_row_range(item);
        /* Projector is visible if it overlaps with visible range:
         * - Starts before visible area ends: origin_row <= last
         * - Ends after visible area starts: last_row >= first */
        origin_row <= last && last_row >= first;
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
    /* Map refs for view cache identity comparison. `elaborated` is the whole-
     * editor elaborated Exp.t that P.view() may consume via info.elaborated;
     * it must participate in the cache key since info.elaborated is derived
     * from it. Refractors pass None. */
    statics_map: Language.Statics.Map.t,
    dynamics_map: Language.Dynamics.Map.t,
    sample_focus: Language.Sample.Focus.t,
    elaborated: option(Language.Exp.t),
  };

  type t = list(projector_data);

  /* Is projector indicated and if so what side is the caret on? */
  let indication = (p: option(Indicated.piece), id) =>
    switch (p) {
    | Some({piece: p, side: d, _}) when Piece.id(p) == id =>
      Some(Direction.toggle(d))
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
    warning:
      Option.map(Language.Info.is_warning, info.statics)
      |> Option.value(~default=false),
    kind: p.kind,
    indication: editor_active ? indication(indicated, id) : None,
    selected: editor_active ? List.mem(id, selection_ids) : false,
    placement: p.placement,
  };

  let mk =
      (
        ~syntax: CachedSyntax.t,
        ~indicated: option(Indicated.piece),
        ~statics: Language.Statics.Map.t,
        ~dynamics: Language.Dynamics.Map.t,
        ~sample_focus: Language.Sample.Focus.t,
        ~editor_active: bool,
        ~elaborated: option(Language.Exp.t),
      ) => {
    let {projectors, measured, term_data, selection_ids, _}: CachedSyntax.t = syntax;
    List.filter_map(
      ((id, _)) => {
        let* p = Id.Map.find_opt(id, projectors);
        let+ measurement = Measured.find_pr_opt(p, measured);
        let info =
          ProjectorInfo.mk_info(
            p,
            ~sample_focus,
            ~statics,
            ~dynamics,
            ~elaborated,
          );
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
          statics_map: statics,
          dynamics_map: dynamics,
          sample_focus,
          elaborated,
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
    (
      ~view_error: bool=false,
      /* Docked: the div holds a chip, not the projector's own UI, so CSS
       * can neutralize this kind's styling and make every chip look alike */
      ~chipped: bool=false,
      {kind, sort, indication, selected, error, warning, _}: Model.status,
    ) =>
  ["projector", ProjectorCore.Kind.name(kind), Sort.show(sort)]
  @ (chipped ? ["chipped"] : [])
  @ (selected ? ["selected"] : [])
  @ (error || view_error ? ["error"] : [])
  @ (warning ? ["warning"] : [])
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
      ~view_error: bool=false,
      ~chipped: bool=false,
      ~idx: int,
      ~kind: ProjectorCore.Kind.t,
      views: list(Node.t),
    ) =>
  div(
    ~attrs=[
      Attr.classes(projector_clss(~view_error, ~chipped, status)),
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
            inject(Project(Focus(idx, kind, None))),
          ])
        }
      ),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );

/* Dispatches projector external actions to editor-level actions */
let handle = (idx, kind, action: external_action): Action.t =>
  switch (action) {
  | Remove => Project(RemoveIndicated)
  | Escape(d) => Project(Escape(idx, d))
  | EscapeToLineEnd(kind) => Project(EscapeToLineEnd(idx, kind))
  | SetSyntax(f) => Project(SetSyntax(idx, kind, f))
  | SampleFocus(sc) => Project(SampleFocus(sc))
  | Probe(p) => Probe(p)
  | FocusById(_) => failwith("FocusById: intercepted in parent closure")
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

/* Abbreviated read-only rendering of a projector's underlying syntax,
 * shown in the sidebar card header. */
let chip_syntax = (~font_metrics: FontMetrics.t, p: Base.projector): Node.t =>
  flex_code(
    ~font_metrics,
    ~single_line=true,
    ~text_only=true,
    Language.Sort.Exp,
    ProjectorChip.segment(p),
  );

/* What a sidebar-docked projector leaves behind at the code site: one
 * fixed glyph, identical for every kind, drawn in the space reserved by
 * ProjectorChip.shape. Clicking it reveals the projector's card. */
let chip = (~open_panel: Ui_effect.t(unit)): Node.t =>
  div(
    ~attrs=[
      Attr.classes(["proj-chip"]),
      Attr.title("Show in the Projectors panel"),
      Attr.on_click(_ => open_panel),
    ],
    [text(ProjectorChip.glyph)],
  );

/* Route top-level metadata to the projector view function. */
let mk_view =
    (
      inject: Action.t => Ui_effect.t(unit),
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      {
        p,
        info,
        status,
        statics_map,
        dynamics_map,
        sample_focus,
        elaborated,
        _,
      }: Model.projector_data,
      projector_list: list(Id.t),
    )
    : View.t => {
  /* Only surfaces that read the AppStore have to expire when it changes:
     the HTML projector, and a probe whose rich probe is running an app. */
  let app_version =
    switch (p.kind) {
    | ProjectorCore.Kind.HTML
    | ProjectorCore.Kind.Probe => AppBridge.version^
    | _ => 0
    };
  switch (
    ViewCache.lookup(
      p.id,
      ~statics_map,
      ~dynamics_map,
      ~sample_focus,
      ~elaborated,
      ~core_settings,
      ~status,
      ~model=p.model,
      ~app_version,
      ~font_metrics,
    )
  ) {
  | Some(view) =>
    ViewCache.hits := ViewCache.hits^ + 1;
    view;
  | None =>
    ViewCache.misses := ViewCache.misses^ + 1;
    let (module P) = ProjectorInit.to_module(p.kind);
    let idx = List.find_index(x => x == p.id, projector_list) |> Option.get;
    let view =
      P.view({
        model: p.model,
        info,
        local: a => {
          let new_model = P.update(p.model, info, a);
          inject(Project(SetModel(idx, p.kind, new_model)));
        },
        local_quiet: a => {
          let new_model = P.update(p.model, info, a);
          inject(Project(SetModelQuiet(idx, p.kind, new_model)));
        },
        parent: a =>
          switch (a) {
          | FocusById(id) =>
            let target_idx = List.find_index(x => x == id, projector_list);
            switch (target_idx) {
            | Some(target_idx) =>
              inject(Project(Focus(target_idx, Probe, None)))
            | None => Effect.Ignore
            };
          | a => inject(handle(idx, p.kind, a))
          },
        view_seg:
          (~single_line=?, ~background=?, ~text_only=?, sort, segment) =>
          flex_code(
            ~font_metrics,
            ~single_line?,
            ~background?,
            ~text_only?,
            sort,
            segment,
          ),
        status,
        core_settings,
        col_width: font_metrics.col_width,
        row_height: font_metrics.row_height,
      });
    ViewCache.store(
      p.id,
      ~statics_map,
      ~dynamics_map,
      ~sample_focus,
      ~elaborated,
      ~core_settings,
      ~status,
      ~model=p.model,
      ~app_version,
      ~font_metrics,
      ~view,
    );
    view;
  };
};

/* Extract and collate different layers of the resulting view
 * in order to stratify z-levels across all projectors */
let split_views =
    (
      inject: Action.t => Ui_effect.t(unit),
      make_active,
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      ~skip_inline: bool,
      /* What clicking a chip does; supplied by the code editor, which is
       * the only caller that can render one */
      ~open_panel: Ui_effect.t(unit)=Effect.Ignore,
      {p, offside_base, measurement, status, _} as projector_data: Model.projector_data,
      projector_list: list(Id.t),
    )
    : (Node.t, option(Node.t)) => {
  let idx = List.find_index(x => x == p.id, projector_list) |> Option.get;
  let chipped =
    !skip_inline && ProjectorCore.Placement.is_sidebar(p.placement);
  let views =
    mk_view(
      inject,
      font_metrics,
      ~core_settings,
      projector_data,
      projector_list,
    );
  let wrapper =
    view_wrapper(
      ~inject,
      ~make_active,
      ~font_metrics,
      ~measurement,
      ~status,
      ~view_error=views.error,
      ~chipped,
      ~idx,
      ~kind=p.kind,
    );
  let line_view = {
    let offside_view =
      views.offside
      |> Option.map(offside_wrapper(font_metrics, offside_base))
      |> Option.to_list;
    /* A docked projector shows a chip here; its primary UI is rendered by
     * ProjectorPanel instead. Overlay and offside layers are unaffected by
     * placement. Refractors (skip_inline) never occupy the inline slot. */
    let inline_view =
      skip_inline ? [] : chipped ? [chip(~open_panel)] : [views.inline];
    wrapper(
      inline_view
      @ [backing_deco(~font_metrics, ~measurement, p)]
      @ offside_view,
    );
  };
  let overlay_view = Option.map(v => wrapper([v]), views.overlay);
  (line_view, overlay_view);
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
      ~core_settings: Language.CoreSettings.t,
      ~visible: option(visible_rows)=?,
      /* Reveals the Projectors panel; what a chip click does */
      ~open_panel: Ui_effect.t(unit)=Effect.Ignore,
      projector_data: list(Model.projector_data),
      projector_list: list(Id.t),
    ) => {
  /* Sorting the projectors by position tends to be a good
   * z-index default; projectors further to the right or
   * further down count as a higher. On its own this could
   * impinge on hover-dropdowns, but the hovered projector
   * has z-index handled separately. But ideally dropdowns
   * should be on the overlay layer so this doesn't come up */
  let get_row_range = (d: Model.projector_data) => (
    d.measurement.origin.row,
    d.measurement.last.row,
  );
  let (base_views, overlay_views) =
    projector_data
    |> filter_by_visibility(visible, _, get_row_range)
    |> List.sort(by_measurement)
    |> List.map(
         split_views(
           ~skip_inline=false,
           ~core_settings,
           ~open_panel,
           inject,
           make_active,
           font_metrics,
           _,
           projector_list,
         ),
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

/* Primary views of the projectors docked to the sidebar, ordered as they
 * read on screen. The chip at the code site and the view
 * returned here are the two halves of a docked projector; `mk_view` is
 * placement-agnostic, so this is the same node the inline placement would
 * have shown. Viewport culling is deliberately not applied: the panel
 * shows docked projectors whether or not they're scrolled into view. */
let sidebar_views =
    (
      inject: Action.t => Ui_effect.t(unit),
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      projector_data: list(Model.projector_data),
      projector_list: list(Id.t),
    )
    : list((Base.projector, Node.t)) => {
  /* Order cards the way they read on screen. Deliberately NOT by position
   * in projector_list: MakeTerm logs projectors during a skel-driven
   * descent, so that list's order follows the traversal, not the source.
   * The measured origin is unambiguous. */
  let syntax_order = (d: Model.projector_data) => (
    d.measurement.origin.row,
    d.measurement.origin.col,
  );
  /* In the panel there is no absolutely-positioned box from the code
   * editor, so projectors that size themselves against it (height: 100%,
   * e.g. TextArea) would collapse to nothing. Give them the same number
   * of rows the inline placement would have reserved.
   *
   * HTML is the exception: an app in the panel sizes to its own content
   * (see proj-html.css), so imposing a height here would either clip it or
   * leave dead space. That also keeps a docked app's height out of the
   * projector model, which matters because model state has no textual
   * form and so cannot survive a backup_text round-trip. */
  let docked_style = (d: Model.projector_data): string =>
    switch (d.p.kind) {
    | HTML => ""
    | _ =>
      let (module P) = ProjectorInit.to_module(d.p.kind);
      let rows =
        switch (P.placeholder(d.p.model, d.info)) {
        | {vertical: Inline, _} => 1
        | {vertical: Tab(n) | Block(n), _} => n + 1
        };
      Printf.sprintf(
        "height: %fpx;",
        float_of_int(rows) *. font_metrics.row_height,
      );
    };
  projector_data
  |> List.filter((d: Model.projector_data) =>
       ProjectorCore.Placement.is_sidebar(d.p.placement)
     )
  |> List.sort((d1, d2) => compare(syntax_order(d1), syntax_order(d2)))
  |> List.map((d: Model.projector_data) => {
       let views =
         mk_view(inject, font_metrics, ~core_settings, d, projector_list);
       /* Same class list the code-site wrapper applies, so per-kind CSS
        * still matches; the panel overrides the absolute positioning. */
       (
         d.p,
         div(
           ~attrs=[
             Attr.classes(projector_clss(~view_error=views.error, d.status)),
             Attr.create("style", docked_style(d)),
           ],
           [views.inline],
         ),
       );
     });
};

/* Secondary surfaces projectors want in the panel without moving themselves
 * there — see ProjectorBase.View.docked. A probe uses this to dock its rich
 * probe while the probe itself stays on the code, so unlike sidebar_views
 * these contribute no chip and their source projector keeps its inline view.
 *
 * Every candidate has to be rendered to find out whether it wants a card, so
 * this is deliberately given refractor data rather than every projector;
 * mk_view is cached, and the code editor has already populated those entries
 * this frame. */
let docked_views =
    (
      inject: Action.t => Ui_effect.t(unit),
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      projector_data: list(Model.projector_data),
      projector_list: list(Id.t),
    )
    : list((Base.projector, Node.t, unit => Ui_effect.t(unit))) =>
  projector_data
  |> List.sort(by_measurement)
  |> List.filter_map((d: Model.projector_data) => {
       let views =
         mk_view(inject, font_metrics, ~core_settings, d, projector_list);
       views.docked
       |> Option.map((docked: ProjectorBase.View.docked) =>
            (
              d.p,
              div(
                ~attrs=[
                  Attr.classes(
                    projector_clss(~view_error=views.error, d.status),
                  ),
                ],
                [docked.content],
              ),
              docked.undock,
            )
          );
     });

let move_dir = (key: Key.t): option(Direction.t) =>
  switch (key) {
  | {key: D("ArrowLeft"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up, _} =>
    Some(Left)
  | {key: D("ArrowRight"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up, _} =>
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
let key_handoff =
    (editor: Editor.t, key: Key.t, projector_list: list(Id.t))
    : option(Action.project) => {
  let z = editor.state.zipper;
  switch (
    move_dir(key),
    Siblings.neighbors(editor.state.zipper.relatives.siblings),
  ) {
  | _ when z.caret != Outer => None
  /* A docked projector has no UI at the code site to hand off to */
  | (Some(Left), (Some(Projector({placement: Sidebar, _})), _))
  | (Some(Right), (_, Some(Projector({placement: Sidebar, _})))) => None
  | (Some(Left), (Some(Projector({id, kind, _})), _)) =>
    let (module P) = ProjectorInit.to_module(kind);
    let idx = List.find_index(x => x == id, projector_list) |> Option.get;
    P.focusable.keyboard != None
      ? Some(Focus(idx, kind, Some(Right))) : None;
  | (Some(Right), (_, Some(Projector({id, kind, _})))) =>
    let (module P) = ProjectorInit.to_module(kind);
    let idx = List.find_index(x => x == id, projector_list) |> Option.get;
    P.focusable.keyboard != None
      ? Some(Focus(idx, kind, Some(Left))) : None;
  | _ => None
  };
};
