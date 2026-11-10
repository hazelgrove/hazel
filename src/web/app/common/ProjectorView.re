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

/* Cache projector view results to avoid expensive Util.BenchMarks.time("view: a projector drawn", () =>
          P.view()) calls
   * when statics/dynamics haven't changed (e.g. during debounced typing).
   * Per-projector cache keyed on map identity + status + model. Handles
   * multiple editors calling into the same cache without thrashing. */
module ViewCache = {
  /* (^report(51)) what a livelit's drawing read of the run: the samples at
     its own id and at each id it looked up (info.dynamics_at), and its own
     pinned interval. Every answer of the run is a new map, so keyed on the
     map's identity all 52 unchanged livelits on the Witch were drawn again
     after each answer (25 to 87 ms a step on run 42713728065); keyed on
     what each read, compared by value, only those whose samples changed */
  type reads = {
    pinned: option((int, int)),
    at: list((Id.t, option(list(Language.Sample.t)))),
  };
  let by_seq = (xs: list(Language.Sample.t)): list(Language.Sample.t) =>
    List.sort(
      (a: Language.Sample.t, b: Language.Sample.t) =>
        Int.compare(a.seq, b.seq),
      xs,
    );
  let samples_same =
      (
        a: option(list(Language.Sample.t)),
        b: option(list(Language.Sample.t)),
      )
      : bool =>
    switch (a, b) {
    | (Stdlib.Option.None, Stdlib.Option.None) => true
    | (Some(xs), Some(ys)) =>
      xs === ys
      || List.length(xs) == List.length(ys)
      && List.for_all2(
           (x: Language.Sample.t, y: Language.Sample.t) =>
             x.value === y.value || Language.Exp.fast_equal(x.value, y.value),
           by_seq(xs),
           by_seq(ys),
         )
    | _ => false
    };
  let reads_hold =
      (
        reads: option(reads),
        ~pinned: option((int, int)),
        dynamics_map: Language.Dynamics.Map.t,
      )
      : bool =>
    switch (reads) {
    | Some(r) =>
      r.pinned == pinned
      && List.for_all(
           ((id, old)) =>
             samples_same(
               old,
               Language.Dynamics.Map.lookup(id, dynamics_map),
             ),
           r.at,
         )
    | Stdlib.Option.None => false
    };
  /* (^report(51)) each cached projector's info at its latest draw or hit,
     for its handlers: a view kept across a new map acts on the newest */
  let latest: Hashtbl.t(Id.t, ProjectorBase.info) = Hashtbl.create(64);
  let latest_info = (id: Id.t, info: ProjectorBase.info): ProjectorBase.info =>
    Option.value(~default=info, Hashtbl.find_opt(latest, id));
  type entry = {
    /* (^report(51)) a livelit's reads of the run when it was drawn */
    reads: option(reads),
    /* (^report(51)) a livelit's own statics record and elaborated subterm,
       which stand for statics_map and elaborated in its key (program_same) */
    own: option((Obj.t, Obj.t)),
    statics_map: Language.Statics.Map.t,
    dynamics_map: Language.Dynamics.Map.t,
    sample_focus: Language.Sample.Focus.t,
    stepping: option(ProjectorBase.stepping),
    program_value: bool,
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
    last_used: int /* tick of the last hit or store */
  };
  let cache: Hashtbl.t(Id.t, entry) = Hashtbl.create(64);
  /* (^report(51)) why the last lookup missed: the parts of the key that
     changed, for the check's "view: a projector drawn" marks */
  let last_miss = ref("");
  /* (^report(51)) each projector's own statics, dynamics and elaborated
     term when it was last drawn, to say which of them are the very same
     objects at its next draw ("S", "D", "E"): whether a key on them, not
     on the whole maps, would hold. Kept only while the check's marks are on */
  let own_parts: Hashtbl.t(Id.t, array(Obj.t)) = Hashtbl.create(64);
  let own_same = (id, parts: array(Obj.t)): string =>
    switch (Hashtbl.find_opt(own_parts, id)) {
    | None => "new"
    | Some(old) =>
      String.concat(
        "",
        List.mapi(
          (i, o) => o === parts[i] ? List.nth(["S", "D", "E"], i) : "",
          Array.to_list(old),
        ),
      )
    };

  /* jsoo has no weak refs: an id that stops rendering (autoprobe mints
     fresh probe ids per edit) would pin its entry, and with it a whole
     generation of statics, dynamics, elaboration and vdom closures.
     log_frame ticks once per editor per frame and sweeps stale entries */
  let tick: ref(int) = ref(0);

  /* (^report(51)) whether the program's statics and elaboration are as
     they were for this entry: for a livelit, by its own statics record (its
     type, its context, and so the definition it looks up in that context)
     and its own elaborated subterm, the very objects they were; else by the
     whole maps. After an edit of one livelit's model on the Witch, the other
     51 had their own records unchanged but were all drawn again, as the
     maps were new (7 to 13 ms a view). */
  let program_same = (e, ~own, ~statics_map, ~elaborated) =>
    switch (own, e.own) {
    | (Some((s, x)), Some((s', x'))) => s === s' && x === x'
    | _ =>
      e.statics_map === statics_map
      && CachedSyntax.elaborated_phys_eq(e.elaborated, elaborated)
    };

  let lookup =
      (
        id,
        ~own,
        ~pinned,
        ~statics_map,
        ~dynamics_map,
        ~sample_focus,
        ~stepping,
        ~program_value,
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
          program_same(e, ~own, ~statics_map, ~elaborated)
          && (
            e.dynamics_map === dynamics_map
            || reads_hold(e.reads, ~pinned, dynamics_map)
          )
          && Language.Sample.Focus.equal(e.sample_focus, sample_focus)
          && e.stepping == stepping
          && e.program_value == program_value
          && e.core_settings == core_settings
          && e.settings_version == ProbeProj.Settings.version^
          && e.app_version == app_version
          && e.font_metrics == font_metrics
          && e.status == status
          && e.model == model =>
      if (e.last_used != tick^) {
        Hashtbl.replace(
          cache,
          id,
          {
            ...e,
            last_used: tick^,
          },
        );
      };
      Some(e.view);
    | Some(e) =>
      if (Util.BenchMarks.on()) {
        last_miss :=
          String.concat(
            ",",
            List.filter_map(
              ((same, what)) => same ? None : Some(what),
              [
                (
                  program_same(e, ~own, ~statics_map, ~elaborated),
                  Option.is_none(own)
                    ? "statics or elaborated" : "own statics or elaborated",
                ),
                (
                  e.dynamics_map === dynamics_map
                  || reads_hold(e.reads, ~pinned, dynamics_map),
                  "dynamics",
                ),
                (
                  Language.Sample.Focus.equal(e.sample_focus, sample_focus),
                  "sample focus",
                ),
                (e.stepping == stepping, "stepping"),
                (e.program_value == program_value, "program value"),
                (e.core_settings == core_settings, "settings"),
                (
                  e.settings_version == ProbeProj.Settings.version^,
                  "probe settings",
                ),
                (e.app_version == app_version, "version"),
                (e.font_metrics == font_metrics, "font"),
                (e.status == status, "status"),
                (e.model == model, "model"),
              ],
            ),
          );
      };
      None;
    | None =>
      last_miss := "new";
      None;
    };

  let store =
      (
        id,
        ~own,
        ~reads,
        ~statics_map,
        ~dynamics_map,
        ~sample_focus,
        ~stepping,
        ~program_value,
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
        reads,
        own,
        statics_map,
        dynamics_map,
        sample_focus,
        stepping,
        program_value,
        elaborated,
        core_settings,
        settings_version: ProbeProj.Settings.version^,
        app_version,
        font_metrics,
        status,
        model,
        view,
        last_used: tick^,
      },
    );

  let hits = ref(0);
  let misses = ref(0);
  let log_frame = () => {
    hits := 0;
    misses := 0;
    incr(tick);
    if (tick^ mod 8 == 0) {
      let stale =
        Hashtbl.fold(
          (id, e, acc) => tick^ - e.last_used > 24 ? [id, ...acc] : acc,
          cache,
          [],
        );
      List.iter(Hashtbl.remove(cache), stale);
      List.iter(Hashtbl.remove(latest), stale);
    };
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

  /* Which layers of a view to render in the current frame. Projectors
   * render all layers in their own frame. Probes on terms inside a
   * splice render in two frames: the owning splice's sub-editor draws
   * the term-anchored layers (highlight, indicator) but never the
   * offside sample view — chips don't go into splices — while the root
   * editor draws only the offside view, repositioned beside the host
   * projector (see RefractorView.mk_data). */
  type layers =
    | All
    | OffsideOnly
    | NoOffside;

  type projector_data = {
    p: Piece.projector,
    info: ProjectorBase.info,
    measurement: Measured.measurement,
    offside_base: int,
    render_layers: layers,
    status,
    /* Map refs for view cache identity comparison. `elaborated` is the whole-
     * editor elaborated Exp.t that Util.BenchMarks.time("view: a projector drawn", () =>
        P.view()) may consume via info.elaborated;
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

  /* Find the end of row offset position in grid units. Starts after
     any quiver offside boxes on the row (RowOffsets registry; quiver
     renders first) — the extra gap leaves room for probe controls
     drawn left of the sample chips. */
  let stack_gap = 2;
  let offside_base =
      (~offset: int, measurement: Measured.measurement, measured: Measured.t)
      : int => {
    let standard = Measured.start_row_width(measurement, measured) + offset;
    let after_quiver = {
      let c = RowOffsets.claimed(~row=measurement.origin.row);
      c == 0 ? 0 : c + stack_gap;
    };
    max(standard, after_quiver) - measurement.origin.col;
  };

  /* Whether a statics error is anchored in the projector's own hidden
   * syntax — pieces between its splices, like a table row's comma —
   * which is rendered as projector chrome rather than laid-out text.
   * Such errors get no arms anywhere (see Arms.tiles_data), so the
   * projector chrome reports them. Splice contents report errors in
   * their own sub-editors, and nested projectors on their own chrome. */
  let has_hidden_error =
      (p: Base.projector, statics: Language.Statics.Map.t): bool => {
    let rec hidden_tile_ids = (seg: Base.segment): list(Id.t) =>
      List.concat_map(
        (piece: Base.piece) =>
          switch (piece) {
          | Tile(t) => [
              t.id,
              ...List.concat_map(hidden_tile_ids, t.children),
            ]
          | Splice(_)
          | Projector(_)
          | Grout(_)
          | Secondary(_) => []
          },
        seg,
      );
    hidden_tile_ids(p.syntax)
    |> List.exists(id =>
         switch (Language.Statics.Map.lookup(id, statics)) {
         | Some(info) => Language.Info.is_error(info)
         | None => false
         }
       );
  };

  let mk_status =
      (
        p: Base.projector,
        ~editor_active: bool,
        ~indicated: option(Indicated.piece),
        ~selection_ids: list(Id.t),
        ~info: ProjectorBase.info,
        ~statics: Language.Statics.Map.t,
        ~id: Id.t,
        ~sort: Sort.t,
      )
      : status => {
    sort,
    error:
      Option.map(Language.Info.is_error, info.statics)
      |> Option.value(~default=false)
      || has_hidden_error(p, statics),
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
        /* The projectors to render in this frame: top-level projectors
         * for the main editor, a splice's own projectors for a
         * sub-editor. Nested projectors are rendered by the sub-editor
         * of the splice that hosts them, not by the outer frame. */
        ~projector_list: list(Id.t),
        ~indicated: option(Indicated.piece),
        ~statics: Language.Statics.Map.t,
        ~dynamics: Language.Dynamics.Map.t,
        ~sample_focus: Language.Sample.Focus.t,
        ~editor_active: bool,
        ~elaborated: option(Language.Exp.t),
      ) => {
    let {projectors, term_data, selection_ids, _}: CachedSyntax.t = syntax;
    let measured = CachedSyntax.measured(syntax);
    List.filter_map(
      id => {
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
          render_layers: All,
          status:
            mk_status(
              p,
              ~sort=TermData.sort(id, term_data),
              ~editor_active,
              ~indicated,
              ~selection_ids,
              ~info,
              ~statics,
              ~id,
            ),
          statics_map: statics,
          dynamics_map: dynamics,
          sample_focus,
          elaborated,
        };
      },
      projector_list,
    );
  };

  /* (^ticket(722), the drag fast path, cut C) mk's answer again, while every input it reads is the same object as
     before: a drag's frames change no editor state (Page.Update.LivelitRepaint), and making every projector's data
     again was most of the projector layer's 14 ms a frame (Firefox run 42563736665). The zipper stands for
     `indicated`, which is made from it anew each frame. A few editors draw each frame (a stack's cells), so a few
     entries are kept. */
  type mk_key = {
    k_syntax: CachedSyntax.t,
    k_zipper: Zipper.t,
    k_statics: Language.Statics.Map.t,
    k_dynamics: Language.Dynamics.Map.t,
    k_active: bool,
    /* the elaboration itself: an option made anew each frame is never the same object */
    k_elaborated: Language.Exp.t,
  };
  let mk_memo: ref(list((mk_key, list(projector_data)))) = ref([]);
  let mk_cached = (~key: mk_key, make: unit => list(projector_data)) => {
    let same = (k: mk_key) =>
      k.k_syntax === key.k_syntax
      && k.k_zipper === key.k_zipper
      && k.k_statics === key.k_statics
      && k.k_dynamics === key.k_dynamics
      && k.k_active == key.k_active
      && k.k_elaborated === key.k_elaborated;
    switch (List.find_opt(((k, _)) => same(k), mk_memo^)) {
    | Some((_, data)) => data
    | None =>
      /* which input differed from the latest entry's, for the timeline */
      let why =
        switch (mk_memo^) {
        | [] => "first"
        | [(k, _), ..._] =>
          k.k_syntax !== key.k_syntax
            ? "syntax"
            : k.k_zipper !== key.k_zipper
                ? "zipper"
                : k.k_statics !== key.k_statics
                    ? "statics"
                    : k.k_dynamics !== key.k_dynamics
                        ? "dynamics"
                        : k.k_active != key.k_active ? "active" : "elaborated"
        };
      let data =
        Util.BenchMarks.time(
          "view: projector data made (" ++ why ++ ")",
          make,
        );
      mk_memo := [(key, data), ...List.filteri((i, _) => i < 7, mk_memo^)];
      data;
    };
  };
};

/* Projectors get a default backing decoration similar
 * to token decorations. This can be made transparent
 * in the CSS if no backing is wanted */
let backing_deco =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~shape: ProjectorCore.Shape.t,
      p,
    ) => {
  let dims: ShardDec.shard_dims = {
    font_metrics,
    measurement,
    tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
  };
  switch (shape.vertical) {
  | Tab(rows) when rows > 0 => ShardDec.relative_tab(dims, ~rows)
  | _ => ShardDec.relative(dims)
  };
};

/* The backing needs the projector shape (a Tab draws differently);
   mirrors ShapeMapSemantics placement switch. */
let shape_of = (p: Base.projector, info: ProjectorBase.info) => {
  let (module P) = ProjectorInit.to_module(p.kind);
  /* Splice sizes are intrinsic to the projector's own syntax -- they do
     not depend on where the parent lays it out -- so this measures them
     here rather than threading the view's map down. */
  let splice_size_map = Measured.splice_sizes(p.syntax);
  let splice_size = (id: Id.t): Util.Point.t =>
    Measured.splice_size_of(splice_size_map, id);
  switch (p.placement) {
  | Sidebar => ProjectorChip.shape(p)
  | Inline => P.placeholder(p.model, info, splice_size)
  };
};

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

/* Keyed by projector id: under culling a scroll step adds/removes probes at
 * the edges; unkeyed, the vdom differ matches positionally and repatches onto
 * a different probe, jumping keyboard focus off the user's probe. */
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
      ~id: Id.t,
      views: list(Node.t),
    ) =>
  div(
    ~key="proj-" ++ Id.to_string(id),
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
  | ToggleSyntax => Project(ToggleSyntax(idx))
  | SetSyntax(f) => Project(SetSyntax(idx, kind, f))
  | SampleFocus(sc) => Project(SampleFocus(sc))
  | SetTerm(term, preserve_splices) =>
    Project(SetTerm(idx, term, preserve_splices))
  | Unproject(seg) => Project(Unproject(idx, seg))
  | Probe(p) => Probe(p)
  | FocusById(_) => failwith("FocusById: intercepted in parent closure")
  };

let offside_wrapper =
    (
      font_metrics: FontMetrics.t,
      offside_base: int,
      ~row: int,
      ~origin_col: int,
      v: Node.t,
    ) =>
  div(
    ~attrs=[
      /* class + row/col identity for the after-display stagger pass
         (ProbeStagger): displays spanning several rows get pushed past
         longer lines below and past displays from rows above */
      Attr.classes(["offside-wrapper"]),
      Attr.create("data-row", string_of_int(row)),
      Attr.create("data-ocol", string_of_int(origin_col)),
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

/* below view at pane col 0: shift back by origin.col (wrapper sits at the projector's col) */
let below_wrapper = (font_metrics: FontMetrics.t, origin_col: int, v: Node.t) =>
  div(
    ~attrs=[
      Attr.classes(["below-wrapper"]),
      /* Gates the scroll-affordance fade (proj-probe.css .at-bottom). */
      Attr.on_scroll(evt => {
        JsUtil.sync_at_bottom_class(evt);
        Effect.Ignore;
      }),
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; top: %fpx; left: %fpx;",
          font_metrics.row_height,
          -. (font_metrics.col_width *. float_of_int(origin_col)),
        ),
      ),
    ],
    [v],
  );

let simple_code =
    (
      ~background=false,
      ~classes=?,
      ~is_single_line=false,
      font_metrics,
      _sort,
      segment,
    )
    : Node.t => {
  let shape_map = ProjectorCore.Shape.Map.empty; /* Assume this doesn't contain projectors */
  let refractor_rows = Id.Map.empty; /* Assume this doesn't contain refractors (probes) */
  let measured =
    Measured.of_segment(~is_single_line, segment, shape_map, Id.Map.empty);
  let code =
    Code.view(
      ~classes?,
      ~measured,
      ~settings=Settings.Model.init,
      ~shape_map,
      ~refractor_rows,
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
      ~classes=?,
      sort,
      segment,
    ) => {
  text_only
    ? text_code(segment)
    : simple_code(
        ~background?,
        ~classes?,
        ~is_single_line=single_line,
        font_metrics,
        sort,
        segment,
      );
};

/* Default fallback splice renderer: non-interactive simple_code. Used
 * when a ProjectorView caller does not provide a richer ~render_splice
 * callback (e.g. read-only code viewers). */
let default_render_splice =
    (
      font_metrics: FontMetrics.t,
      ~projector_idx: int,
      ~splice_idx: int,
      splice: Base.splice,
    )
    : Node.t => {
  ignore(projector_idx);
  ignore(splice_idx);
  div(
    ~attrs=[Attr.classes(["splice", "inner-editor"])],
    [simple_code(~background=true, font_metrics, Sort.Any, splice.content)],
  );
};

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
/* Kinds with out-of-band state get a version counter in their views' keys: apps read the AppStore; livelits their
   optimistic entries and the slide's compiled run (LivelitProj.version_of) */
let app_version_of = (p: Base.projector): int =>
  switch (p.kind) {
  | ProjectorCore.Kind.HTML => AppBridge.version^
  | ProjectorCore.Kind.Livelit => LivelitProj.version_of(p.id)
  | _ => 0
  };

let rec seg_has_splices = (seg: Base.segment): bool =>
  List.exists(
    (piece: Base.piece) =>
      switch (piece) {
      | Splice(_) => true
      | Tile(t) => List.exists(seg_has_splices, t.children)
      | Projector(_)
      | Grout(_)
      | Secondary(_) => false
      },
    seg,
  );

let mk_view =
    (
      inject: Action.t => Ui_effect.t(unit),
      font_metrics: FontMetrics.t,
      ~core_settings: Language.CoreSettings.t,
      ~render_splice:
         (~projector_idx: int, ~splice_idx: int, Base.splice) => Node.t,
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
  /* Kinds with out-of-band state get a version counter in the key: apps
     read the AppStore; livelits read the optimistic table, whose updates
     must repaint before the debounced statics refresh. */
  let app_version = app_version_of(p);
  /* (^ticket(722)) the program this livelit is drawn from, for its run to
     be compiled with livelits' models as parameters (a physical-equality
     check when it has not changed) */
  switch (p.kind, elaborated) {
  | (ProjectorCore.Kind.Livelit, Some(e)) =>
    Language.HazelJitRun.note_program(e)
  | _ => ()
  };
  /* Splice-bearing projectors render live sub-editors whose caret and
   * selection decorations depend on the zipper — which is not part of
   * the cache key — so their views must be rebuilt every frame. */
  let rec has_splices = (seg: Base.segment): bool =>
    List.exists(
      (piece: Base.piece) =>
        switch (piece) {
        | Splice(_) => true
        | Tile(t) => List.exists(has_splices, t.children)
        | Projector(_)
        | Grout(_)
        | Secondary(_) => false
        },
      seg,
    );
  let cacheable = !has_splices(p.syntax);
  /* (^report(51)) a livelit's key on its own statics record, and its own
     elaborated subterm if it has one (ViewCache.program_same). A livelit
     elaborates none of its syntax (LivelitProj.elaborate_syntax), so its
     info.elaborated is None and its view reads the program through its
     statics record alone */
  let own =
    switch (p.kind, info.statics) {
    | (ProjectorCore.Kind.Livelit, Some(st)) =>
      Some((
        Obj.repr(st),
        switch (info.elaborated) {
        | Some(el) => Obj.repr(el)
        | None => Obj.repr(0)
        },
      ))
    | _ => Stdlib.Option.None
    };
  /* (^report(51)) a livelit is kept across a run's new answer while what it
     read of the run is the same (ViewCache.reads) */
  let is_livelit = p.kind == ProjectorCore.Kind.Livelit;
  let pinned =
    Option.bind(info.dynamics, (d: Language.Dynamics.Info.t) =>
      d.pinned_interval
    );
  if (cacheable) {
    Hashtbl.replace(ViewCache.latest, p.id, info);
  };
  switch (
    cacheable
      ? ViewCache.lookup(
          p.id,
          ~own,
          ~pinned,
          ~statics_map,
          ~dynamics_map,
          ~sample_focus,
          ~stepping=info.stepping,
          ~program_value=info.program_value,
          ~elaborated,
          ~core_settings,
          ~status,
          ~model=p.model,
          ~app_version,
          ~font_metrics,
        )
      : None
  ) {
  | Some(view) =>
    ViewCache.hits := ViewCache.hits^ + 1;
    view;
  | None =>
    ViewCache.misses := ViewCache.misses^ + 1;
    let (module P) = ProjectorInit.to_module(p.kind);
    /* (^report(51)) a livelit's lookups of the run, while it is drawn */
    let read_at = ref([]);
    let info =
      is_livelit
        ? {
          ...info,
          dynamics_at: id => {
            let r = info.dynamics_at(id);
            read_at := [(id, r), ...read_at^];
            r;
          },
        }
        : info;
    let idx = List.find_index(x => x == p.id, projector_list) |> Option.get;
    /* Walk the projector's syntax (including tile children) collecting every
     * splice regardless of nesting. Splices inside nested projectors are not
     * collected — those belong to the inner projector. */
    let rec collect_splices = (seg: Base.segment): list(Base.splice) =>
      List.concat_map(
        (p: Base.piece) =>
          switch (p) {
          | Splice(s) => [s, ...collect_splices(s.content)]
          | Tile(t) => List.concat_map(collect_splices, t.children)
          | Projector(_)
          | Grout(_)
          | Secondary(_) => []
          },
        seg,
      );
    let splices = collect_splices(p.syntax);
    let splice_view = (id: Id.t) =>
      switch (List.find_index((s: Base.splice) => s.id == id, splices)) {
      | None => span_c("splice-missing", [Node.text("?")])
      | Some(splice_idx) =>
        let splice = List.nth(splices, splice_idx);
        render_splice(~projector_idx=idx, ~splice_idx, splice);
      };
    let splice_size_map = Measured.splice_sizes(p.syntax);
    let splice_size = (id: Id.t): Util.Point.t =>
      Measured.splice_size_of(splice_size_map, id);
    let own_parts =
      Util.BenchMarks.on()
        ? [|
          /* (None as a fresh object: an absent part is never "the same") */
          switch (info.statics) {
          | Some(x) => Obj.repr(x)
          | None => Obj.repr(ref(0))
          },
          switch (info.dynamics) {
          | Some(x) => Obj.repr(x)
          | None => Obj.repr(ref(0))
          },
          switch (info.elaborated) {
          | Some(x) => Obj.repr(x)
          | None => Obj.repr(ref(0))
          },
        |]
        : [||];
    let view =
      Util.BenchMarks.time(
        ~extra=
          () => {
            let same = ViewCache.own_same(p.id, own_parts);
            Hashtbl.replace(ViewCache.own_parts, p.id, own_parts);
            Util.BenchMarks.[
              ("kind", str(ProjectorCore.Kind.name(p.kind))),
              ("id", str(String.sub(Id.to_string(p.id), 0, 8))),
              ("why", str(cacheable ? ViewCache.last_miss^ : "splices")),
              ("own_same", str(same)),
            ];
          },
        "view: a projector drawn",
        () =>
          P.view({
            model: p.model,
            info,
            local: a => {
              let new_model =
                P.update(p.model, ViewCache.latest_info(p.id, info), a);
              inject(Project(SetModel(idx, p.kind, new_model)));
            },
            local_quiet: a => {
              let new_model =
                P.update(p.model, ViewCache.latest_info(p.id, info), a);
              inject(Project(SetModelQuiet(idx, p.kind, new_model)));
            },
            parent: a =>
              switch (a) {
              | FocusById(id) =>
                let target_idx =
                  List.find_index(x => x == id, projector_list);
                switch (target_idx) {
                | Some(target_idx) =>
                  inject(Project(Focus(target_idx, Probe, None)))
                | None => Effect.Ignore
                };
              | a => inject(handle(idx, p.kind, a))
              },
            view_seg:
              (
                ~single_line=?,
                ~background=?,
                ~classes=?,
                ~text_only=?,
                sort,
                segment,
              ) =>
              flex_code(
                ~font_metrics,
                ~single_line?,
                ~background?,
                ~classes?,
                ~text_only?,
                sort,
                segment,
              ),
            splice_view,
            splice_size,
            splices,
            status,
            core_settings,
            col_width: font_metrics.col_width,
            row_height: font_metrics.row_height,
          }),
      );
    if (cacheable) {
      ViewCache.store(
        p.id,
        ~own,
        ~reads=
          is_livelit
            ? Some(
                {
                  pinned,
                  at: [
                    (p.id, Language.Dynamics.Map.lookup(p.id, dynamics_map)),
                    ...read_at^,
                  ],
                }: ViewCache.reads,
              )
            : Stdlib.Option.None,
        ~statics_map,
        ~dynamics_map,
        ~sample_focus,
        ~stepping=info.stepping,
        ~program_value=info.program_value,
        ~elaborated,
        ~core_settings,
        ~status,
        ~model=p.model,
        ~app_version,
        ~font_metrics,
        ~view,
      );
    };
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
      ~render_splice:
         (~projector_idx: int, ~splice_idx: int, Base.splice) => Node.t,
      {p, offside_base, measurement, status, render_layers, _} as projector_data: Model.projector_data,
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
      ~render_splice,
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
      ~id=p.id,
    );
  let shape = shape_of(p, projector_data.info);
  let line_view = {
    let offside_view =
      views.offside
      |> Option.map(
           offside_wrapper(
             font_metrics,
             offside_base,
             ~row=measurement.origin.row,
             ~origin_col=measurement.origin.col,
           ),
         )
      |> Option.to_list;
    let below_view =
      views.below
      |> Option.map(below_wrapper(font_metrics, measurement.origin.col))
      |> Option.to_list;
    /* A Tab projector's wrapper div spans only its top row (that is all
       the measurement covers), so mount the GUI in an extent layer sized
       to the full hang-below region; per-kind CSS positions within it. */
    let tab_extent = (view: Node.t): Node.t =>
      switch (shape.vertical) {
      | Tab(rows) when rows > 0 =>
        div(
          ~attrs=[
            Attr.class_("tab-extent"),
            Attr.create(
              "style",
              Printf.sprintf(
                "top: 1px; bottom: auto; height: %fpx;",
                float_of_int(rows + 1) *. font_metrics.row_height -. 2.,
              ),
            ),
          ],
          [view],
        )
      | _ => view
      };
    /* A docked projector shows a chip here; its primary UI is rendered by
     * ProjectorPanel instead. Overlay and offside layers are unaffected by
     * placement. Refractors (skip_inline) never occupy the inline slot. */
    let inline_view =
      skip_inline
        ? [] : chipped ? [chip(~open_panel)] : [tab_extent(views.inline)];
    wrapper(
      inline_view
      @ [backing_deco(~font_metrics, ~measurement, ~shape, p)]
      @ offside_view
      @ below_view,
    );
  };
  let overlay_view =
    Option.map(
      v => wrapper([v]),
      render_layers == Model.OffsideOnly ? Option.none : views.overlay,
    );
  (line_view, overlay_view);
};

let by_measurement = (pd1: Model.projector_data, pd2: Model.projector_data) =>
  compare(pd1.measurement.origin.row, pd2.measurement.origin.row);

/* (^ticket(722), cut C) each cacheable projector's finished views, by id, from the frame that last drew it; several
   editors draw in one frame (a stack's cells), so entries are kept across editors, and the whole table is let go
   past 512 entries, so it holds no closures of projectors long gone */
type split_entry = {
  s_pd: Model.projector_data,
  s_version: int,
  s_list: list(Id.t),
  s_font: FontMetrics.t,
  s_core: Language.CoreSettings.t,
  s_probe: int,
  s_out: (Node.t, option(Node.t)),
};
let split_memo: Hashtbl.t(Id.t, split_entry) = Hashtbl.create(64);

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
      ~render_splice:
         option(
           (~projector_idx: int, ~splice_idx: int, Base.splice) => Node.t,
         )=?,
      projector_data: list(Model.projector_data),
      projector_list: list(Id.t),
    ) => {
  let render_splice =
    Option.value(
      render_splice,
      ~default=default_render_splice(font_metrics),
    );
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
  /* (^ticket(722), the drag fast path, cut C) a projector's finished views again, while its data is the same object
     (Model.mk_cached), its version has not moved, and the frame's settings are the same; a projector holding
     splices is made every frame, as its view's cache does (its caret decorations read the zipper) */
  let split = (pd: Model.projector_data) => {
    let make = () =>
      split_views(
        ~skip_inline=false,
        ~core_settings,
        ~open_panel,
        ~render_splice,
        inject,
        make_active,
        font_metrics,
        pd,
        projector_list,
      );
    let version = app_version_of(pd.p);
    switch (Hashtbl.find_opt(split_memo, pd.p.id)) {
    | Some(e)
        when
          e.s_pd === pd
          && e.s_version == version
          && e.s_list === projector_list
          && e.s_font == font_metrics
          && e.s_core == core_settings
          && e.s_probe == ProbeProj.Settings.version^ =>
      e.s_out
    | _ =>
      let out = make();
      if (!seg_has_splices(pd.p.syntax)) {
        if (Hashtbl.length(split_memo) > 512) {
          Hashtbl.reset(split_memo);
        };
        Hashtbl.replace(
          split_memo,
          pd.p.id,
          {
            s_pd: pd,
            s_version: version,
            s_list: projector_list,
            s_font: font_metrics,
            s_core: core_settings,
            s_probe: ProbeProj.Settings.version^,
            s_out: out,
          },
        );
      };
      out;
    };
  };
  let (base_views, overlay_views) =
    projector_data
    |> filter_by_visibility(visible, _, get_row_range)
    |> List.sort(by_measurement)
    |> List.map(split)
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
      let splice_size_map = Measured.splice_sizes(d.p.syntax);
      let splice_size = (id: Id.t): Util.Point.t =>
        Measured.splice_size_of(splice_size_map, id);
      let rows =
        switch (P.placeholder(d.p.model, d.info, splice_size)) {
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
       /* The sidebar card is a preview, not an editing surface, so its
          splices render read-only rather than as live sub-editors. */
       let views =
         mk_view(
           inject,
           font_metrics,
           ~core_settings,
           ~render_splice=default_render_splice(font_metrics),
           d,
           projector_list,
         );
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
