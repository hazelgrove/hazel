open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

open Js_of_ocaml;
open Language;
open RichProbe;
open RichProbeRegistry;

/* Global probe display state. See ZipperBase.re for full probe state documentation.
 * - Settings.s: Global display settings (window mode, cutoffs)
 * - Settings.offset: Per-probe window scroll offsets
 * - SampleLength.lengths: Per-sample display lengths
 * These use mutable refs for simplicity since they're UI-only state. */

[@deriving (show({with_path: false}), sexp, yojson)]
type probe_model = {
  active_renderer: option(packed_model),
  drawer_mode: bool,
};

let init_probe_model: probe_model = {
  active_renderer: None,
  drawer_mode: false,
};

/* Any deserialization failure resets to defaults — the record is
 * pure transient UI state. Known failure modes are logged for
 * debuggability; unknown ones still degrade gracefully.
 * Tolerates older serialized models (without drawer_mode) via fallback. */
let probe_model_of_sexp = sexp =>
  switch (probe_model_of_sexp(sexp)) {
  | model => model
  | exception (RichProbeRegistry.Unknown_renderer(rid)) =>
    print_endline("probe_model_of_sexp: unknown renderer " ++ rid);
    init_probe_model;
  | exception (Failure(msg)) =>
    print_endline("probe_model_of_sexp: malformed payload: " ++ msg);
    init_probe_model;
  | exception _ => init_probe_model
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleModal(option(packed_model))
  | RendererAction(packed_action)
  | ToggleWindowMode
  | ToggleDrawerMode
  | SetDrawerMode(bool)
  | ToggleDropdown(string)
  | SetDropdown(option(string))
  | ResetSettings;

/* How a sample's value should be rendered inside the offside view.
 * Inline is the existing single-line, Abbreviate-budgeted display
 * (one row in the editor, horizontal budget set by SampleLength).
 * Block is the multi-line drawer display, pretty-printed to wrap at
 * a per-sample width pulled from SampleLength in `value_view`, with
 * `settings.drawer.width` as the default when none is set. */
type sample_display =
  | Inline
  | Block;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
    | Hybrid
    | StepRange;

  /* Per-drawer display configuration. Currently only the line-wrap
   * target width; will grow over time (height cap, indent-respect,
   * etc.) so we keep it as a nested record from the start. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type drawer_settings = {width: int};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type settings = {
    window: Sample.Window.mode,
    sample_base,
    before_cutoff: option(int),
    after_cutoff: option(int),
    caller_cutoff: option(int),
    callee_cutoff: option(int),
    drawer: drawer_settings,
  };

  type set_action =
    | ToggleWindow
    | SetSampleBase(sample_base)
    | ToggleBeforeCutoff
    | ToggleAfterCutoff
    | ToggleCallerCutoff
    | ToggleCalleeCutoff;

  let init_drawer: drawer_settings = {width: 80};

  let init: settings = {
    window: Single,
    sample_base: Hybrid,
    before_cutoff: None,
    after_cutoff: None,
    caller_cutoff: None,
    callee_cutoff: None,
    drawer: init_drawer,
  };

  /* When true, ArrowUp/Down skip probes that have no samples
   * aligned with the current cursor. */
  let skip_unaligned_nav = true;

  let update = (settings: settings, action: set_action): settings =>
    switch (action) {
    | ToggleWindow => {
        ...settings,
        window: settings.window == Sample.Window.Single ? Many : Single,
      }
    | SetSampleBase(base) => {
        ...settings,
        sample_base: base,
      }
    | ToggleBeforeCutoff => {
        ...settings,
        before_cutoff: settings.before_cutoff == None ? Some(1) : None,
      }
    | ToggleAfterCutoff => {
        ...settings,
        after_cutoff: settings.after_cutoff == None ? Some(1) : None,
      }
    | ToggleCallerCutoff => {
        ...settings,
        caller_cutoff: settings.caller_cutoff == None ? Some(1) : None,
      }
    | ToggleCalleeCutoff => {
        ...settings,
        callee_cutoff: settings.callee_cutoff == None ? Some(1) : None,
      }
    };

  let offset = Hashtbl.create(100);

  let s = ref(init);
  let version = ref(0);

  /* Where the sample context drawer (actions/args/env) is shown.
   * Three mutually exclusive states:
   *   HoverOnly:     dropdown appears on hover, hides off-hover (default).
   *   StickyInPlace: dropdown is pinned open for the indicated sample
   *                  without hover. Transient (resets on app reload).
   *                  Toggled by '/' key.
   *   DockedSidebar: content rendered in the probe sidebar drawer
   *                  instead of as a per-sample dropdown. Persistent
   *                  (mirror of web/Settings.sample_drawer_in_sidebar).
   *                  Toggled by Cmd/Ctrl+; or the dock-arrow icon.
   * The '/' key can also exit DockedSidebar into StickyInPlace (the
   * asymmetric "undock to in-place" behavior); the icon is the only
   * thing that enters DockedSidebar. See ProjectorView.ViewCache —
   * mutations must go through set_display_mode so version bumps and
   * the projector view cache invalidates. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type display_mode =
    | HoverOnly
    | StickyInPlace
    | DockedSidebar;

  let display_mode = ref(HoverOnly);

  /* Callbacks invoked by UI affordances inside the projector view
   * (which can only dispatch external_actions through the framework
   * API, not global Settings updates). Both are set by Page.view on
   * each render so they capture the current `inject`. */
  let on_drawer_toggle: ref(unit => Virtual_dom.Vdom.Effect.t(unit)) =
    ref(_ => Virtual_dom.Vdom.Effect.Ignore);
  let on_sticky_toggle: ref(unit => Virtual_dom.Vdom.Effect.t(unit)) =
    ref(_ => Virtual_dom.Vdom.Effect.Ignore);

  /* Write `display_mode` AND bump version so the projector view cache
   * (keyed on Settings.version) invalidates on the next render. */
  let set_display_mode = (m: display_mode) => {
    display_mode := m;
    version := version^ + 1;
  };

  /* DOM id of the single sample dropdown opened by alt-click (None = closed).
   * Independent of display_mode: works in the default (non-docked) view to
   * give the sample dropdown context-menu semantics (alt-click to open,
   * click-outside/Escape to close via SampleMenuListener). Transient; bumps
   * version so the projector view cache invalidates. */
  let open_dropdown: ref(option(string)) = ref(None);
  let set_open_dropdown = (o: option(string)) => {
    open_dropdown := o;
    version := version^ + 1;
  };

  let reset_mode = () => {
    Hashtbl.clear(offset);
    s := init;
    open_dropdown := None;
    /* Only clear the transient StickyInPlace bit; preserve the user's
     * persisted DockedSidebar preference (matches the old reset behavior
     * which only cleared show_env). */
    if (display_mode^ == StickyInPlace) {
      display_mode := HoverOnly;
    };
    version := version^ + 1;
  };

  let go = (a: set_action): unit => {
    s := update(s^, a);
    version := version^ + 1;
  };
};

open Settings;
open Node;

/* Shared context for probe view functions. Constructed once in offside_view
 * after unwrapping dynamics and statics, then threaded to all child views. */
type probe_ctx = {
  ap_id: option(Id.t),
  statics: Language.Statics.Info.t,
  settings,
  dynamics: Dynamics.Info.t,
  utility: ProjectorBase.utility,
  parent: external_action => Ui_effect.t(unit),
  local: action => Ui_effect.t(unit),
  sort: Sort.t,
  /* Id of the currently-open rich-probe renderer, if any. Drives the
   * "View as <id>" / "Hide <id>" toggle label in the sample menu. */
  active_renderer_id: option(string),
};

/* Stateful window offset management (GUI-specific) */
module WindowState = {
  let get_offset = (k: Id.t): int =>
    switch (Hashtbl.find_opt(offset, k)) {
    | Some(v) => v
    | None => 0
    };

  let set_offset = (k: Id.t, v: int) => Hashtbl.replace(offset, k, v);

  /* Update offset and return (new_offset, max_samples) */
  let reform =
      (
        ~window: Sample.Window.mode,
        id: Id.t,
        all_samples: int,
        cursor_idx: int,
      )
      : (int, int) => {
    let max = Sample.Window.max_samples(window);
    let new_offset =
      Sample.Window.adjusted_offset(
        ~cursor_idx,
        ~current_offset=get_offset(id),
        ~max_samples=max,
        ~total=all_samples,
      );
    set_offset(id, new_offset);
    (new_offset, max);
  };
};

module SampleLength = {
  let lengths: Hashtbl.t(int, int) = Hashtbl.create(100);

  let reset = () => {
    Hashtbl.clear(lengths);
  };

  let is_explicit = (sample: Sample.t): bool =>
    Hashtbl.mem(lengths, sample.id);

  let get = (window: Sample.Window.mode, sample: Sample.t): int =>
    Hashtbl.find_opt(lengths, sample.id)
    |> Option.value(~default=window == Single ? 150 : 12);

  let set = (id: int, length: int): unit =>
    Hashtbl.replace(lengths, id, length);
};

/* Select samples to display, using stateful window offset.
 * This wraps Sample.Selection with WindowState for offset persistence.
 * Optionally takes pre-filtered samples to avoid redundant filtering. */
let select_samples =
    (
      ~settings: settings,
      ~id: Id.t,
      ~ap_id: option(Id.t),
      ~filtered: option(list(Sample.t))=?,
      dynamics: Dynamics.Info.t,
    )
    : list(Sample.t) => {
  let samples =
    switch (filtered) {
    | Some(s) => s
    | None =>
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=dynamics.sample_focus.pinned_stack,
        dynamics.samples,
      )
    };
  let first_idx =
    Sample.Selection.most_aligned_index(
      ~ap_id,
      dynamics.sample_focus,
      samples,
    );
  if (first_idx == None && settings.window == Single) {
    [];
  } else {
    let cursor_idx = first_idx |> Option.value(~default=0);
    let all_samples = List.length(samples);
    let (new_offset, max) =
      WindowState.reform(
        ~window=settings.window,
        id,
        all_samples,
        cursor_idx,
      );
    ListUtil.slice(new_offset, max, samples) |> List.rev;
  };
};

let seg_of_exp = ProbeUtil.seg_of_exp;
let abbreviated_seg_of = ProbeUtil.abbreviated_seg_of;

/* Pretty-print a value to a multi-line segment at the given width.
 * Skips Abbreviate since drawer mode is the use case where samples
 * are allowed to span as many lines as they need. */
let pretty_seg_of_value =
    (utility: utility, ~width: int, exp: Exp.t): Segment.t => {
  /* Pretty-print to a multi-line drawer: ask term_to_seg for the
   * non-inline (block) form so PrettySegment has real linebreaks
   * to fold rather than a single squashed line. */
  let seg =
    utility.term_to_seg(~inline=false, Exp(exp |> DHExp.strip_ascriptions));
  PrettySegment.prettify(~width, seg);
};

/* Rows reserved for a probe's drawer-mode Tab(n) placeholder.
 * Pretty-prints each visible sample at the drawer's wrap width,
 * counts rows via Measured, returns the max. With no samples
 * available (no focus-aligned samples, no samples yet) falls back
 * to 1 row — just enough for the empty-status icon. Refractor
 * drawer height transitions are smoothed out by the caret-shift
 * compensation in `CachedSyntax.calculate` rather than a cache
 * here. */
module DrawerHeight = {
  /* Max rows the drawer can reserve via Tab(n); content taller than
   * this scrolls inside `.below-wrapper` (see overflow-y: auto in
   * proj-probe.css). Prevents a single deeply-nested sample from
   * pushing code arbitrarily far down. */
  let max_rows = 15;

  /* Row count via Measured. PrettySegment.format_segment ends with
   * Segment.reassemble, which folds the formatter's flat output back
   * into nested tile structure, so naively scanning top-level pieces
   * for Secondary linebreaks undercounts. Measured.of_segment runs the
   * canonical layout walk (handling indentation, deferred linebreaks,
   * etc.) and `total_rows` reads off its row tally directly. */
  let row_count = (seg: Segment.t): int =>
    Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty)
    |> Measured.total_rows;

  let sample_rows = (utility: utility, ~width: int, sample: Sample.t): int =>
    row_count(pretty_seg_of_value(utility, ~width, sample.value));

  let compute = (info: info): int =>
    switch (info.dynamics, info.statics) {
    | (Some(dynamics), Some(statics)) =>
      let settings = Settings.s^;
      let ap_id = Sample.Focus.cur_var_ap(statics);
      let samples = select_samples(~settings, ~id=info.id, ~ap_id, dynamics);
      switch (samples) {
      | [] => 1
      | _ =>
        /* Per-sample width: matches value_view's Block branch so the
         * reserved row count (Tab(n)) aligns with the rendered drawer
         * heights. Falls back to settings.drawer.width if no explicit
         * width set. */
        let heights =
          List.map(
            (sample: Sample.t) =>
              sample_rows(
                info.utility,
                ~width=
                  Hashtbl.find_opt(SampleLength.lengths, sample.id)
                  |> Option.value(~default=settings.drawer.width),
                sample,
              ),
            samples,
          );
        min(max_rows, List.fold_left(max, 1, heights));
      };
    | _ => 1
    };
};

let pos_rel_to_target = (e: Js.t(Dom_html.mouseEvent)): Point.t => {
  open Float;
  let (col_width, row_height) = JsUtil.font_metrics_from_specimen();
  let text_box =
    e##.currentTarget
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code")
    |> Option.get;
  let x_rel = of_int(e##.clientX) -. text_box##getBoundingClientRect##.left;
  let y_rel = of_int(e##.clientY) -. text_box##getBoundingClientRect##.top;
  let row = to_int(y_rel /. row_height);
  let col = to_int(round(x_rel /. col_width));
  {
    row,
    col,
  };
};

let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 4) {
    "s" ++ string_of_int(length - 4);
  } else {
    "s0";
  };

/* Depth classes from call stack relation (structural effects: displacement, stacking) */
let depth_clss =
    (~settings, ~ap_id, dynamics: Dynamics.Info.t, sample: Sample.t)
    : list(string) => {
  let relation =
    Sample.Focus.relation(
      ~trimmed=true,
      ~ap_id,
      dynamics.sample_focus,
      sample,
    );
  switch (relation.relative_level_to_cursor) {
  | Same => ["depth-same"]
  | Below(n)
      when settings.before_cutoff == None || Some(n) <= settings.before_cutoff => [
      "depth-below",
      "depth-" ++ string_of_int(n),
    ]
  | Above(n)
      when settings.after_cutoff == None || Some(n) <= settings.after_cutoff => [
      "depth-above",
      "depth-" ++ string_of_int(n),
    ]
  | _ => []
  };
};

/* Color classes from active scheme (background, text color) */
let color_clss =
    (~settings, ~ap_id, dynamics: Dynamics.Info.t, sample: Sample.t)
    : list(string) => {
  let step_range_clss = () =>
    switch (
      Sample.Focus.step_containment(
        ~focus_range=dynamics.sample_focus.step_range,
        sample,
      )
    ) {
    | StepEqual => ["focus"]
    | StepContains => ["related-before"]
    | StepContainedWithin => ["related-after"]
    | StepDisjointBefore => ["tangent-before"]
    | StepDisjointAfter => ["tangent-after"]
    | StepNoFocus => ["unrelated"]
    };
  switch (settings.sample_base) {
  | Calls =>
    let relation =
      Sample.Focus.relation(
        ~trimmed=true,
        ~ap_id,
        dynamics.sample_focus,
        sample,
      );
    switch (
      relation.is_call_cursor,
      relation.is_call_above_call_cursor,
      relation.is_below_indicated_call,
    ) {
    | (true, _, _) => ["focus"]
    | (_, Some(0), _) => ["related-before"]
    | (_, Some(_), _) when settings.caller_cutoff == None => [
        "related-before",
      ]
    | (_, _, Some(0)) => ["related-after"]
    | (_, _, Some(_)) when settings.callee_cutoff == None => [
        "related-after",
      ]
    | (_, _, _) =>
      /* Unrelated samples with a depth direction get faded directional coloring,
         respecting cutoffs (matching old behavior where level_class provided above/below) */
      switch (relation.relative_level_to_cursor) {
      | Above(n)
          when
            settings.after_cutoff == None || Some(n) <= settings.after_cutoff => [
          "tangent-before",
        ]
      | Below(n)
          when
            settings.before_cutoff == None
            || Some(n) <= settings.before_cutoff => [
          "tangent-after",
        ]
      | _ => ["unrelated"]
      }
    };
  | StepRange => step_range_clss()
  | Hybrid =>
    /* Top-level samples (empty call stack): use step range only */
    if (sample.call_stack == []) {
      step_range_clss();
    } else {
      let relation =
        Sample.Focus.relation(
          ~trimmed=true,
          ~ap_id,
          dynamics.sample_focus,
          sample,
        );
      if (relation.is_call_cursor) {
        [
          "focus" /* Green — same call stack as cursor */
        ];
      } else {
        step_range_clss(); /* Everything else: step range coloring */
      };
    }
  };
};

let cursor_clss =
    (~settings, ~ap_id, dynamics: Dynamics.Info.t, sample: Sample.t)
    : list(string) => {
  color_clss(~settings, ~ap_id, dynamics, sample)
  @ depth_clss(~settings, ~ap_id, dynamics, sample);
};

module Debug = {
  let stack = (stack: Sample.call_stack): string =>
    stack
    |> List.map((f: Sample.stack_frame) => Id.str3(f.id))
    |> String.concat("\n");

  let str = (~ap_id: option(Id.t), sample: Sample.t): string =>
    "sample id: "
    ++ string_of_int(sample.id)
    ++ "\n"
    ++ "ap:"
    ++ (
      switch (Sample.Focus.cur_call(ap_id, sample)) {
      | Some([{id: ap_id, _}, ..._]) => Id.str3(ap_id)
      | _ => "None"
      }
    )
    ++ "\nstack:\n"
    ++ stack(sample.call_stack)
    ++ "\nstep-range:\n"
    ++ Printf.sprintf("[%d, %d]", sample.step_start, sample.step_end)
    ++ "\ntime: "
    ++ Printf.sprintf("%.0f", sample.time);
};

/* Find first compatible renderer for an expression */
let find_compatible_renderer =
    (sort: Sort.t, exp: Exp.t): option(RichProbe.packed_renderer) =>
  List.find_opt(r => r.can_handle(sort, exp), renderers);

let pin_call = (ctx: probe_ctx) =>
  switch (ctx.ap_id, Dynamics.Info.is_in(ctx.dynamics)) {
  | (Some(ap_id), Some(sample)) =>
    let call_stack = [
      {
        Sample.id: ap_id,
        name: None,
        fn_def_id: None,
      },
      ...sample.call_stack,
    ];
    ctx.parent(Probe(Pin(call_stack, ap_id)));
  | _ => Effect.Ignore
  };

let focus_call = (ctx: probe_ctx) =>
  switch (Dynamics.Info.is_in(ctx.dynamics)) {
  | Some(sample) when sample.call_stack != [] =>
    ctx.parent(SampleFocus(TogglePin(sample.call_stack)))
  | _ => Effect.Ignore
  };

/* Find the largest budget whose rendered width fits within target_width.
 * width_at(b) returns the rendered width for budget b. */
let find_best_budget = (width_at: int => int, target_width: int): int => {
  let rec find_upper = (b: int): int =>
    if (b > 500 || width_at(b) > target_width) {
      b;
    } else {
      find_upper(b * 2 + 1);
    };
  let upper = find_upper(max(1, target_width));
  let rec bisect = (lo: int, hi: int): int =>
    if (lo >= hi) {
      lo;
    } else {
      let mid = (lo + hi + 1) / 2;
      if (width_at(mid) <= target_width) {
        bisect(mid, hi);
      } else {
        bisect(lo, mid - 1);
      };
    };
  bisect(target_width, upper);
};

module ValueState = {
  let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);
};

let value_view =
    (
      ~display: sample_display,
      ~alt_toggle: Ui_effect.t(unit),
      ctx: probe_ctx,
      ~num_total,
      view_seg,
      local,
      sample: Sample.t,
    ) => {
  let {settings, ap_id, utility, _} = ctx;
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    if (Js.to_bool(e##.shiftKey)) {
      let target =
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      JsUtil.setPointerCapture(target, e##.pointerId);
      ValueState.mousedown := Some(target);
    };
    ctx.parent(
      SampleFocus(Capture(Sample.capture_of_sample(sample), ap_id)),
    );
  };

  let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
    let target =
      e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
    if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
      JsUtil.releasePointerCapture(target, e##.pointerId);
    };
    ValueState.mousedown := None;
    Effect.Ignore;
  };

  let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) => {
    switch (ValueState.mousedown^) {
    | Some(_) when Js.to_bool(e##.shiftKey) =>
      let goal = pos_rel_to_target(e);
      let target_width = max(1, goal.col);
      /* Inline: rendered width isn't a linear function of budget
       * (Abbreviate makes discrete decisions), so bisect for the budget
       * whose output fits target_width. Block: pretty-print wrap width
       * IS target_width directly. Both update SampleLength.lengths via
       * ChangeLength; value_view's Block branch reads the same map. */
      let budget =
        switch (display) {
        | Inline =>
          let width_at = (b: int): int =>
            abbreviated_seg_of(utility, b, sample.value) |> snd;
          find_best_budget(width_at, target_width);
        | Block => target_width
        };
      local(ChangeLength(sample.id, budget));
    | _ => Effect.Ignore
    };
  };

  let (seg, length_class) =
    switch (display) {
    | Inline =>
      let length =
        if (!SampleLength.is_explicit(sample) && num_total == 1) {
          150;
        } else {
          SampleLength.get(settings.window, sample);
        };
      let (seg, length) = abbreviated_seg_of(utility, length, sample.value);
      (seg, [length_cls(length)]);
    | Block =>
      let width =
        Hashtbl.find_opt(SampleLength.lengths, sample.id)
        |> Option.value(~default=settings.drawer.width);
      (pretty_seg_of_value(utility, ~width, sample.value), []);
    };

  div(
    ~attrs=[
      Attr.classes(
        ["value", ...length_class]
        @ cursor_clss(
            ~settings=ctx.settings,
            ~ap_id=ctx.ap_id,
            ctx.dynamics,
            sample,
          )
        @ (Option.is_some(ap_id) ? ["ap"] : [])
        @ (!ValueChecker.is_value(sample.value) ? ["indet"] : []),
      ),
      Attr.on_double_click(_ => local(ToggleWindowMode)),
      Attr.on_pointerdown(evt =>
        Js.to_bool(Js.Unsafe.coerce(evt)##.altKey)
          ? Effect.Many([
              alt_toggle,
              Effect.Stop_propagation,
              Effect.Prevent_default,
            ])
          : Key.meta_held(evt)
              ? Option.is_some(ctx.ap_id) ? pin_call(ctx) : focus_call(ctx)
              : val_pointerdown(evt)
      ),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [view_seg(~text_only=false, seg)],
  );
};

let env_val = (ctx: probe_ctx, view_seg, sample, en: Sample.Env.entry): Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ " ≡ "),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        let (seg, _) =
          abbreviated_seg_of(
            ctx.utility,
            SampleLength.get(ctx.settings.window, sample),
            d,
          );
        view_seg(~text_only=false, seg);
      },
    ],
  );
};

let show_pin = (ctx: probe_ctx, sample: Sample.t) => {
  switch (ctx.ap_id, ctx.dynamics.sample_focus.pinned_stack) {
  | (Some(ap_id), Some(pinned_stack)) =>
    /* Compare by ID only - function names may differ */
    Sample.ids_of_stack(pinned_stack)
    == [ap_id, ...Sample.ids_of_stack(sample.call_stack)]
  | _ => false
  };
};

let show_focus = (ctx: probe_ctx, sample: Sample.t) =>
  switch (ctx.dynamics.sample_focus.pinned_stack) {
  | Some(pinned_stack) =>
    Sample.ids_of_stack(pinned_stack)
    == Sample.ids_of_stack(sample.call_stack)
  | _ => false
  };

let pin_view = (ctx: probe_ctx, sample: Sample.t) =>
  if (show_pin(ctx, sample)) {
    [div(~attrs=[Attr.classes(["pin"])], [])];
  } else if (show_focus(ctx, sample)) {
    [div(~attrs=[Attr.classes(["pin-enclosing"])], [])];
  } else {
    [];
  };

/* Generate a DOM id that's unique per sample-instance. sample.id is
 * Hashtbl.hash((stack, syntax_id)) and is intentionally coarse — recursive
 * invocations frequently collide on it. Combining with step_start/step_end
 * disambiguates. If Sample.id is ever made truly unique, simplify this back
 * to just sample.id. See issue #2288. */
let dropdown_id = (sample: Sample.t): string =>
  Printf.sprintf(
    "sample-dropdown-%d-%d-%d",
    sample.id,
    sample.step_start,
    sample.step_end,
  );

/* Step into handler for sample context menu */
let step_into_sample =
    (~parent, ~sample: Sample.t, ~ap_id: Id.t): Ui_effect.t(unit) =>
  parent(Probe(StepInto(sample.call_stack, ap_id)));

/* Check if step-into is possible for this probe's function call.
 * Requires: Ap of a named variable that isn't a built-in. */
let can_step_into = (statics: Language.Statics.Info.t): bool =>
  switch (statics) {
  | InfoExp({user_term: {term: Ap(_, fn_exp, _), _}, _}) =>
    switch (fn_exp.term) {
    | Var(name) => Environment.lookup(Builtins.env_init, name) == None
    | _ => false
    }
  | _ => false
  };

let pin_action = (ctx: probe_ctx, sample: Sample.t) => {
  let is_pinned = show_pin(ctx, sample);
  div(
    ~attrs=[
      Attr.classes(
        ["action-item", "pin-action"] @ (is_pinned ? ["pinned"] : []),
      ),
      Attr.on_pointerdown(_ => pin_call(ctx)),
    ],
    [
      // div(~attrs=[Attr.classes(["pin-icon"])], []),
      text(is_pinned ? "Unpin this call" : "Pin this call"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("P")]),
    ],
  );
};

let focus_action = (ctx: probe_ctx, sample: Sample.t) => {
  let is_focused = show_focus(ctx, sample);
  div(
    ~attrs=[
      Attr.classes(
        ["action-item", "pin-action"] @ (is_focused ? ["pinned"] : []),
      ),
      Attr.on_pointerdown(_ => focus_call(ctx)),
    ],
    [
      // div(~attrs=[Attr.classes(["pin-icon"])], []),
      text(is_focused ? "Unpin enclosing call" : "Pin enclosing call"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("P")]),
    ],
  );
};

/* Step Into action */
let step_into_action = (ctx: probe_ctx, sample: Sample.t, ap_id: Id.t) =>
  div(
    ~attrs=[
      Attr.classes(["action-item", "step-into-action"]),
      Attr.on_pointerdown(_
        /* Stop propagation to prevent parent wrapper's Focus action
           from moving cursor back to the probe after we jump */
        =>
          Effect.Many([
            Effect.Stop_propagation,
            step_into_sample(~parent=ctx.parent, ~sample, ~ap_id),
          ])
        ),
    ],
    [
      // div(~attrs=[Attr.classes(["step-into-icon"])], []),
      text("Step into"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("Enter")]),
    ],
  );

/* Dock toggle: swaps the sample drawer between hover dropdown
 * (in the editor offside view) and the probe sidebar. The bar-arrow
 * glyph points toward the destination: → bar (sidebar) when currently
 * a hover dropdown; bar ← when currently docked in the sidebar. */
let dock_toggle = (): Node.t => {
  let docked = Settings.display_mode^ == Settings.DockedSidebar;
  let icon = docked ? {js|⇤|js} : {js|⇥|js};
  let tooltip =
    docked
      ? "Undock sample drawer (Cmd+;)"
      : "Dock sample drawer in sidebar (Cmd+;)";
  div(
    ~attrs=[
      Attr.classes(["action-item", "dock-toggle"]),
      Attr.title(tooltip),
      Attr.on_pointerdown(_ =>
        Effect.Many([Effect.Stop_propagation, Settings.on_drawer_toggle^()])
      ),
    ],
    [text(icon)],
  );
};

/* Rich probe action: open a domain-specific visualization via ToggleModal.
   One menu item per compatible renderer; r.badge supplies the icon.
   Label flips to "Hide <id>" when this renderer's modal is already open,
   since dispatching ToggleModal again closes it. */
let rich_probe_action =
    (ctx: probe_ctx, sample: Sample.t, r: packed_renderer): Node.t => {
  let is_active = ctx.active_renderer_id == Some(r.id);
  let label = (is_active ? "Hide " : "View as ") ++ r.id;
  div(
    ~attrs=[
      Attr.classes(["action-item", "rich-probe-action"]),
      Attr.on_pointerdown(_ =>
        ctx.local(ToggleModal(r.init_model(ctx.sort, sample.value)))
      ),
    ],
    [r.badge, text(label)],
  );
};

let rich_probe_items = (ctx: probe_ctx, _sample: Sample.t): list(Node.t) =>
  switch (Dynamics.Info.most_aligned_sample(ctx.ap_id, ctx.dynamics)) {
  | None => []
  | Some(indicated) =>
    renderers
    |> List.filter_map(r =>
         r.can_handle(ctx.sort, indicated.value)
           ? Some(rich_probe_action(ctx, indicated, r)) : None
       )
  };

/* Drawer-mode toggle: flips this probe between inline-offside display
 * and below-line full-width pretty-printed drawer. Per-probe state
 * (ProbeProj.M.model.drawer_mode), so dispatched via local. Now drawn
 * as the chevron portion of the consolidated SVG nav-bar (see
 * `nav_bar_view`), not a standalone div. */

/* Context actions for a sample (Pin/Unpin, Step Into, rich-probe views, etc.).
 * The dock toggle is always appended at the row's far right, so the user
 * can switch between hover/sidebar views from either context. */
let sample_context_actions =
    (ctx: probe_ctx, ~can_step_into: bool, sample: Sample.t): list(Node.t) => {
  let rich_items = rich_probe_items(ctx, sample);
  let primary =
    switch (ctx.ap_id) {
    | Some(ap_id) =>
      [pin_action(ctx, sample)]
      @ (can_step_into ? [step_into_action(ctx, sample, ap_id)] : [])
      @ rich_items
    | None when sample.call_stack != [] =>
      [focus_action(ctx, sample)] @ rich_items
    | None => rich_items
    };
  [
    div(
      ~attrs=[Attr.classes(["context-actions"])],
      primary @ [dock_toggle()],
    ),
  ];
};

/* Get function name from statics info if this is an Ap expression */
let get_fn_name_from_statics =
    (statics: Language.Statics.Info.t): option(string) =>
  switch (statics) {
  | InfoExp({user_term: {term: Ap(_, fn_exp, _), _}, _}) =>
    switch (fn_exp.term) {
    | Var(name) => Some(name)
    | Constructor(name, _) => Some(name)
    | Fun(_) => Some({js|λ|js})
    | BuiltinFun(name) => Some(name)
    | _ => Some("fn")
    }
  | _ => None
  };

/* Extract per-position variable info from arguments.
 * Returns list(option(string)) where Some(name) means that argument
 * position is a bare variable reference. Used to render "name = value"
 * labels in the call display for variable arguments. */
let get_arg_var_info =
    (statics: Language.Statics.Info.t): list(option(string)) => {
  let rec extract_var = (e: Exp.t): option(string) =>
    switch (e.term) {
    | Var(name) => Some(name)
    | Parens(inner) => extract_var(inner)
    | _ => None
    };
  switch (statics) {
  | InfoExp({user_term: {term: Ap(_, _, arg), _}, _}) =>
    switch (arg.term) {
    | Var(name) => [Some(name)]
    | Parens(inner) => [extract_var(inner)]
    | Tuple(elements) => List.map(extract_var, elements)
    | _ => [None]
    }
  | _ => []
  };
};

/* fn_name span + opening paren, used in call display */
let fn_header = (fn_name: string): list(Node.t) => [
  Node.span(~attrs=[Attr.classes(["fn-name"])], [Node.text(fn_name)]),
  Node.span(~attrs=[Attr.classes(["paren"])], [Node.text("(")]),
];

/* A single argument row with optional var label, value, and comma/close-paren */
let arg_row =
    (~var_info: option(string), ~is_last: bool, rendered: Node.t): Node.t =>
  div(
    ~attrs=[Attr.classes(["call-arg-row"])],
    (
      switch (var_info) {
      | Some(name) => [
          Node.span(
            ~attrs=[Attr.classes(["arg-name"])],
            [Node.text(name)],
          ),
          Node.text(" = "),
        ]
      | None => []
      }
    )
    @ [rendered]
    @ (is_last ? [] : [Node.text(",")])
    @ (
      is_last
        ? [
          Node.span(~attrs=[Attr.classes(["paren"])], [Node.text(")")]),
        ]
        : []
    ),
  );

/* Call display section showing function call with argument values */
let sample_call_display =
    (ctx: probe_ctx, view_seg, sample: Sample.t): list(Node.t) =>
  switch (sample.args, get_fn_name_from_statics(ctx.statics)) {
  | (Some(arg_val), Some(fn_name)) =>
    let length = SampleLength.get(ctx.settings.window, sample);
    let arg_var_info = get_arg_var_info(ctx.statics);
    let render_exp = (exp: Exp.t) => {
      let (seg, _) = abbreviated_seg_of(ctx.utility, length, exp);
      view_seg(~text_only=false, seg);
    };
    switch (arg_val) {
    | Opaque => [
        div(
          ~attrs=[Attr.classes(["call-display"])],
          fn_header(fn_name)
          @ [
            Node.text({js|⟨fn⟩|js}),
            Node.span(~attrs=[Attr.classes(["paren"])], [Node.text(")")]),
          ],
        ),
      ]
    | Val(arg_exp) =>
      switch (arg_exp.term) {
      | Tuple(elements) when List.length(elements) > 1 =>
        let num_elems = List.length(elements);
        let arg_rows =
          List.mapi(
            (i, elem) =>
              arg_row(
                ~var_info=
                  switch (List.nth_opt(arg_var_info, i)) {
                  | Some(v) => v
                  | None => None
                  },
                ~is_last=i == num_elems - 1,
                render_exp(elem),
              ),
            elements,
          );
        [
          div(
            ~attrs=[Attr.classes(["call-display", "multiline"])],
            [
              div(
                ~attrs=[Attr.classes(["call-header"])],
                fn_header(fn_name),
              ),
            ]
            @ arg_rows,
          ),
        ];
      | _ =>
        let var_label =
          switch (arg_var_info) {
          | [Some(name)] => [
              Node.span(
                ~attrs=[Attr.classes(["arg-name"])],
                [Node.text(name)],
              ),
              Node.text(" = "),
            ]
          | _ => []
          };
        [
          div(
            ~attrs=[Attr.classes(["call-display"])],
            fn_header(fn_name)
            @ var_label
            @ [
              render_exp(arg_exp),
              Node.span(
                ~attrs=[Attr.classes(["paren"])],
                [Node.text(")")],
              ),
            ],
          ),
        ];
      }
    };
  | _ => []
  };

/* Filter environment entries: dedup, remove opaques, exclude filter_vars */
let filtered_env_entries =
    (~filter_vars: list(string), sample: Sample.t): list(Sample.Env.entry) =>
  sample.env
  |> ListUtil.dedup
  |> Sample.Env.remove_opaques
  |> List.filter((en: Sample.Env.entry) =>
       !List.mem(en.binding.name, filter_vars)
     );

/* Environment section showing variable bindings.
 * filter_vars: variable names to exclude (already shown in call display) */
let sample_environment =
    (
      ctx: probe_ctx,
      ~filter_vars: list(string)=[],
      view_seg,
      sample: Sample.t,
    )
    : list(Node.t) => {
  let elems = filtered_env_entries(~filter_vars, sample);
  elems == []
    ? []
    : [
      div(
        ~attrs=[Attr.classes(["environment-section"])],
        [
          div(
            ~attrs=[Attr.classes(["live-env"])],
            List.map(env_val(ctx, view_seg, sample), elems),
          ),
        ],
      ),
    ];
};

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (statics: Language.Statics.Info.t): bool =>
  switch (statics) {
  | InfoExp({user_term: {term: Var(_), _}, _}) => true
  | InfoPat(_) => true
  | _ => false
  };

/* Inner sections of the sample context drawer: actions, call display, env.
 * Used by both the hover dropdown (sample_context_menu) and the sidebar
 * drawer (sample_context_drawer). Also returns has_env/has_call flags so
 * the dropdown can adjust its chrome. */
let sample_context_sections =
    (ctx: probe_ctx, view_seg, sample: Sample.t): (bool, bool, list(Node.t)) => {
  /* Get variable names shown in call display to filter from environment */
  let filter_vars = List.filter_map(Fun.id, get_arg_var_info(ctx.statics));
  let env_elems = filtered_env_entries(~filter_vars, sample);
  let has_env = env_elems != [];
  let has_call = Option.is_some(sample.args);
  let nodes =
    sample_context_actions(
      ctx,
      ~can_step_into=can_step_into(ctx.statics),
      sample,
    )
    @ sample_call_display(ctx, view_seg, sample)
    @ sample_environment(ctx, ~filter_vars, view_seg, sample);
  (has_env, has_call, nodes);
};

/* Sample context menu (dropdown) combining actions and environment */
let sample_context_menu =
    (~show_env, ctx: probe_ctx, view_seg, sample: Sample.t): Node.t => {
  let (has_env, has_call, nodes) =
    sample_context_sections(ctx, view_seg, sample);
  div(
    ~attrs=
      [
        Attr.classes(
          ["sample-context-menu"]
          @ (has_env || has_call ? [] : ["no-env"])
          @ (show_env ? ["dropdown-active"] : []),
        ),
        /* id only — visibility is driven by the dropdown-active class
         * (set via show_env); no SafeTriangle hover handlers. */
        Attr.id(dropdown_id(sample)),
      ],
    nodes,
  );
};

/* Sidebar drawer rendering: same content as the dropdown, without
 * SafeTriangle hover chrome. Returns None when there's nothing to show. */
let sample_context_drawer =
    (ctx: probe_ctx, view_seg, sample: Sample.t): option(Node.t) => {
  let (has_env, has_call, nodes) =
    sample_context_sections(ctx, view_seg, sample);
  /* Mirror sample_view's has_dropdown gate: a non-Ap term that's a Var or
   * Pat with no enclosing call has nothing useful to show. */
  let hide_env = hide_env(ctx.statics);
  let has_dropdown =
    !(hide_env && ctx.ap_id == None) || sample.call_stack != [];
  if (!has_dropdown && !has_env && !has_call) {
    None;
  } else {
    /* Inner content rules (.context-actions, .live-env, .call-display, ...)
     * are unscoped in proj-probe.css, so the drawer styles itself via
     * those inner classes. Sidebar-specific tweaks live under
     * `#probe-sidebar .sample-context-drawer` in probesystem.css.
     * `.no-env` toggles the vertical action layout when there's
     * nothing else to show. */
    let classes =
      ["sample-context-drawer"] @ (has_env || has_call ? [] : ["no-env"]);
    Some(div(~attrs=[Attr.classes(classes)], nodes));
  };
};

let sample_view =
    (
      ~display: sample_display,
      ctx: probe_ctx,
      ~indicated_sample_id,
      ~num_total,
      view_seg,
      local,
      sample: Sample.t,
    ) => {
  let hide_env = hide_env(ctx.statics);
  let has_rich =
    switch (Dynamics.Info.most_aligned_sample(ctx.ap_id, ctx.dynamics)) {
    | Some(indicated) =>
      List.exists(r => r.can_handle(ctx.sort, indicated.value), renderers)
    | None => false
    };
  let has_dropdown =
    !(hide_env && ctx.ap_id == None) || sample.call_stack != [] || has_rich;
  /* In DockedSidebar the per-sample hover dropdown is hidden; the same
   * content is rendered by the probe sidebar instead. */
  let render_dropdown =
    has_dropdown && Settings.display_mode^ != Settings.DockedSidebar;
  /* StickyInPlace pins the dropdown open for the indicated sample
   * (the in-place equivalent of DockedSidebar). */
  let is_indicated = indicated_sample_id == Some(sample.id);
  let did = dropdown_id(sample);
  /* Shown when pinned in place (StickyInPlace, indicated) OR explicitly
   * opened by alt-click (open_dropdown) — the latter gives the dropdown
   * context-menu semantics in the default view. */
  let show_env =
    Settings.display_mode^ == Settings.StickyInPlace
    && is_indicated
    || Settings.open_dropdown^ == Some(did);
  /* The `indicated-sample` class marks this probe's most-aligned sample.
   * Combined with `.projector.probe.indicated` (set on the unique probe
   * adjacent to the caret), this gives a single DOM anchor element used
   * by `SampleAnchor` to compensate scroll on Left/Right SetIndex.
   * `menu-trigger` exempts the sample from SampleMenuListener's
   * click-outside dismissal so the opening alt-click isn't undone. */
  let sample_classes =
    ["sample", "menu-trigger"]
    @ (is_indicated ? ["indicated-sample"] : []);
  div(
    ~attrs=[Attr.classes(sample_classes)],
    [
      value_view(
        ~display,
        ~alt_toggle=local(ToggleDropdown(did)),
        ctx,
        ~num_total,
        view_seg,
        local,
        sample,
      ),
    ]
    @ pin_view(ctx, sample)
    @ (
      render_dropdown
        ? [sample_context_menu(~show_env, ctx, view_seg, sample)] : []
    ),
  );
};

/* Select a default sample by preferring the closest match to the current
 * sample focus. */
let mv_least_distant_sample = (ctx: probe_ctx, _evt): Effect.t(unit) => {
  let {ap_id, dynamics, parent, _} = ctx;
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=dynamics.sample_focus.pinned_stack,
      dynamics.samples,
    );
  switch (
    Sample.Selection.most_aligned_sample(
      ~ap_id,
      ~cursor=dynamics.sample_focus,
      samples,
    )
  ) {
  | Some(selected) =>
    parent(SampleFocus(Capture(Sample.capture_of_sample(selected), ap_id)))
  | None => Effect.Ignore
  };
};

let ellipsis_view = (local): Node.t =>
  div(
    ~attrs=[
      Attr.classes(["ellipsis"]),
      Attr.on_double_click(_ => local(ToggleWindowMode)),
    ],
    [text("⋯")],
  );

/* Unified view for explaining why no samples are shown */
let empty_status_view =
    (ctx: probe_ctx, ~status: Sample.Selection.empty_status, local): Node.t =>
  switch (status) {
  | NoSamplesExist =>
    div(
      ~attrs=[
        Attr.classes(["empty-status", "no-samples"]),
        Attr.title("This expression was never evaluated"),
      ],
      [text("∅")],
    )
  | HiddenByPin =>
    div(
      ~attrs=[
        Attr.classes(["empty-status", "hidden-by-pin"]),
        Attr.title("Samples hidden by pin — click to unpin"),
        Attr.on_pointerdown(_ => ctx.parent(SampleFocus(Reset))),
      ],
      [text("⍟")] //📌◌🔒
    )
  | NotAligned =>
    /* Reuse existing ellipsis behavior for not-aligned case */
    div(
      ~attrs=[
        Attr.classes(["empty-status", "not-aligned"]),
        Attr.title("Samples not aligned with focus — click to align"),
        Attr.on_pointerdown(mv_least_distant_sample(ctx)),
        Attr.on_double_click(_ => local(ToggleWindowMode)),
      ],
      [text("⊖")],
    )
  | Evaluating =>
    /* Animated spinner while waiting for evaluation after step-into */
    div(
      ~attrs=[
        Attr.classes(["empty-status", "evaluating"]),
        Attr.title("Evaluating..."),
      ],
      [text("⟳")],
    )
  };

let move_cursor = (ctx: probe_ctx, offset: int) => {
  let {ap_id, dynamics, parent, _} = ctx;
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=dynamics.sample_focus.pinned_stack,
      dynamics.samples,
    );
  let cursor_idx =
    Sample.Selection.most_aligned_index(
      ~ap_id,
      dynamics.sample_focus,
      samples,
    );
  switch (cursor_idx) {
  /* Cursor would be outside window, reset to next visible sample */
  | Some(idx) =>
    let next_idx_maybe = idx - offset;
    if (next_idx_maybe >= 0 && next_idx_maybe < List.length(samples)) {
      let sample = List.nth(samples, next_idx_maybe);
      parent(
        SampleFocus(Capture(Sample.capture_of_sample(sample), ap_id)),
      );
    } else {
      Effect.Ignore;
    };
  | _ => Effect.Ignore
  };
};

/* Row-level controls for a probe sample row, drawn as a single SVG
 * containing three independently-clickable regions:
 *   - left arrow (previous sample)   — left half of rotated square
 *   - right arrow (next sample)      — right half of rotated square
 *   - drawer-mode toggle             — chevron below the arrows
 * Each region is a <g> with a visual <polygon> plus an invisible
 * <rect> hit-target; CSS `:hover` on the group flips the polygon
 * fill to red. The chevron's hit-rect is drawn LAST so it's on top
 * in z-order, meaning the diamond's bottom corners (which fall inside
 * the chevron's hit area) dispatch to the toggle rather than to an
 * arrow.
 *
 * Geometry (in viewBox units, free parameters s/√2 = 7 and gap g = 2):
 *   Diamond center at (0, -2), chevron outer at (0, +2). Whole shape
 *   is centered at (0, 0) so CSS `scaleY(-1)` flips it in drawer mode
 *   around the actual bounding-box center.
 *
 * Arrows only render when show_arrows is true (overflow exists). The
 * drawer toggle always renders so it's reachable on any indicated
 * probe. */
let nav_bar_view = (ctx: probe_ctx, ~num_total, ~show_arrows: bool) => {
  let disable_left =
    num_total < Sample.Window.max_samples(ctx.settings.window);
  let disable_right =
    num_total < Sample.Window.max_samples(ctx.settings.window);
  let svg_attr = (k, v) => Attr.create(k, v);
  let polygon = points =>
    Node.create_svg(
      "polygon",
      ~attrs=[Attr.classes(["visual"]), svg_attr("points", points)],
      [],
    );
  let hit_rect = (~x, ~y, ~w, ~h) =>
    Node.create_svg(
      "rect",
      ~attrs=[
        Attr.classes(["hit"]),
        svg_attr("x", x),
        svg_attr("y", y),
        svg_attr("width", w),
        svg_attr("height", h),
      ],
      [],
    );
  let title = (label: string) =>
    Node.create_svg("title", [Node.text(label)]);
  /* Mac shows the Cmd glyph as `⌘X`; PC/Linux shows `Ctrl+X`. Matches
   * the convention used in ProbeSidebar's quick reference. */
  let meta = Util.Os.is_mac^ ? {js|⌘|js} : "Ctrl+";
  let arrow_group = (~side, ~disabled: bool, ~offset: int, ~hit_x: string) =>
    Node.create_svg(
      "g",
      ~attrs=[
        Attr.classes(["nav-" ++ side] @ (disabled ? ["disabled"] : [])),
        Attr.on_click(_ => move_cursor(ctx, offset)),
      ],
      [
        title(side == "left" ? "Previous sample (←)" : "Next sample (→)"),
        polygon(side == "left" ? "-7,-2 -1,-8 -1,4" : "1,-8 7,-2 1,4"),
        hit_rect(~x=hit_x, ~y="-10", ~w="9", ~h="11"),
      ],
    );
  let toggle_group =
    Node.create_svg(
      "g",
      ~attrs=[
        Attr.classes(["drawer-mode-toggle"]),
        Attr.on_pointerdown(_ =>
          Effect.Many([Effect.Stop_propagation, ctx.local(ToggleDrawerMode)])
        ),
      ],
      [
        title(
          "Toggle drawer ("
          ++ meta
          ++ {js|↓|js}
          ++ " / "
          ++ meta
          ++ {js|↑|js}
          ++ ")",
        ),
        polygon("-6,1 -7,2 0,9 7,2 6,1 0,7"),
        hit_rect(~x="-9", ~y="1", ~w="18", ~h="9"),
      ],
    );
  let arrow_groups =
    show_arrows
      ? [
        arrow_group(
          ~side="left",
          ~disabled=disable_left,
          ~offset=1,
          ~hit_x="-9",
        ),
        arrow_group(
          ~side="right",
          ~disabled=disable_right,
          ~offset=-1,
          ~hit_x="0",
        ),
      ]
      : [];
  Node.create_svg(
    "svg",
    ~attrs=[Attr.classes(["nav-bar"]), svg_attr("viewBox", "-9 -10 18 20")],
    arrow_groups @ [toggle_group],
  );
};

let num_samples_view = (~ap_id: option(Id.t), dynamics: Dynamics.Info.t) => {
  let num_samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=dynamics.sample_focus.pinned_stack,
      dynamics.samples,
    )
    |> List.length;
  let description = num_samples < 1000 ? string_of_int(num_samples) : "1k+";
  div(
    ~attrs=[
      Attr.title(string_of_int(num_samples)),
      Attr.classes(["num-samples"]),
    ],
    [text(description)],
  );
};

let round_up = (ctx: probe_ctx, sample): int => {
  let (_, cur) =
    abbreviated_seg_of(
      ctx.utility,
      SampleLength.get(ctx.settings.window, sample),
      sample.value,
    );
  let goal = cur + 1;
  let (_, max_len) =
    seg_of_exp(ctx.utility, DHExp.strip_ascriptions(sample.value));
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(ctx.utility, target, sample.value) |> snd;
    if (attempt_len < goal && target <= max_len) {
      find_target(target + 1);
    } else {
      target;
    };
  };
  find_target(goal);
};

let round_down = (ctx: probe_ctx, sample: Sample.t): int => {
  let (_, cur) =
    abbreviated_seg_of(
      ctx.utility,
      SampleLength.get(ctx.settings.window, sample),
      sample.value,
    );
  let goal = max(1, cur - 1);
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(ctx.utility, target, sample.value) |> snd;
    if (attempt_len > goal && target > 0) {
      find_target(target - 1);
    } else {
      target;
    };
  };
  find_target(goal);
};

let indicated_sample = (ctx: probe_ctx): option(Sample.t) =>
  Dynamics.Info.most_aligned_sample(ctx.ap_id, ctx.dynamics);

let key_handler =
    (ctx: probe_ctx, ~id: Id.t, ~drawer_mode_active: bool, local, evt) => {
  let {ap_id, parent, _} = ctx;
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("E" | "e") when key.meta == Down || key.ctrl == Down => parent(Remove)
  | D("Escape") when Settings.open_dropdown^ != None =>
    /* First Escape closes an alt-click-opened sample dropdown (stays
     * focused); subsequent Escapes fall through to the cases below. */
    Many([local(SetDropdown(None)), Stop_propagation, Prevent_default])
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([local(ResetSettings), parent(SampleFocus(Reset))]);
  | D("Escape") when drawer_mode_active =>
    /* Two-stage Esc: first Esc collapses the drawer (stays focused),
     * second Esc blurs (handled by the rule below). */
    Many([local(SetDrawerMode(false)), Stop_propagation, Prevent_default])
  | D("Escape") =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([Stop_propagation, Prevent_default]);
  | D("Enter") when key.meta == Down || key.ctrl == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([
      parent(EscapeToLineEnd(Probe)),
      Stop_propagation,
      Prevent_default,
    ]);
  /* Cmd+Left (Mac) / Home (PC): bounce back to editor */
  | D("ArrowLeft") when key.meta == Down || key.ctrl == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([
      parent(EscapeToLineEnd(Probe)),
      Stop_propagation,
      Prevent_default,
    ]);
  | D("Home") =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([Stop_propagation, Prevent_default]);
  | D("ArrowRight") when key.shift == Down =>
    let effect =
      switch (indicated_sample(ctx)) {
      | Some(sample) =>
        local(ChangeLength(sample.id, round_up(ctx, sample)))
      | None => Ignore
      };
    Many([effect, Stop_propagation, Prevent_default]);
  | D("ArrowLeft") when key.shift == Down =>
    let effect =
      switch (indicated_sample(ctx)) {
      | Some(sample) =>
        local(ChangeLength(sample.id, round_down(ctx, sample)))
      | None => Ignore
      };
    Many([effect, Stop_propagation, Prevent_default]);
  | D("ArrowRight") =>
    /* Stash indicated sample's screen-y BEFORE dispatch so
     * Main.after_display can compensate any reflow above it. */
    SampleAnchor.capture();
    // Prevent_default below stops aggressive horizontal scroll
    Many([move_cursor(ctx, -1), Stop_propagation, Prevent_default]);
  | D("ArrowLeft") =>
    SampleAnchor.capture();
    Many([move_cursor(ctx, 1), Stop_propagation, Prevent_default]);
  | D("ArrowDown") when key.meta == Down || key.ctrl == Down =>
    /* Enter drawer mode for this probe. Idempotent if already in
     * drawer mode. Paired with Cmd/Ctrl+ArrowUp below to exit. */
    Many([local(SetDrawerMode(true)), Stop_propagation, Prevent_default])
  | D("ArrowUp") when key.meta == Down || key.ctrl == Down =>
    /* Exit drawer mode. Idempotent if not in drawer mode. */
    Many([local(SetDrawerMode(false)), Stop_propagation, Prevent_default])
  | D("ArrowDown") =>
    let skip = Settings.skip_unaligned_nav;
    let effect =
      switch (
        JsUtil.navigate_probes(~skip_unaligned=skip, Id.cls(id), `Down)
      ) {
      | Some(target_id) => parent(FocusById(target_id))
      | None => Ignore
      };
    Many([effect, Stop_propagation, Prevent_default]);
  | D("ArrowUp") =>
    let skip = Settings.skip_unaligned_nav;
    let effect =
      switch (JsUtil.navigate_probes(~skip_unaligned=skip, Id.cls(id), `Up)) {
      | Some(target_id) => parent(FocusById(target_id))
      | None => Ignore
      };
    Many([effect, Stop_propagation, Prevent_default]);
  | D(" ") =>
    Many([local(ToggleWindowMode), Stop_propagation, Prevent_default])
  | D("p" | "P") when key.meta == Down || key.ctrl == Down => Ignore /* Defer to page-level handler for auto-probe toggle */
  | D("p") =>
    /* Pin/Unpin the indicated sample, or Focus/Unfocus for non-ap probes */
    switch (indicated_sample(ctx), ap_id) {
    | (Some(_), Some(_)) =>
      Many([pin_call(ctx), Stop_propagation, Prevent_default])
    | (Some(_), None) =>
      Many([focus_call(ctx), Stop_propagation, Prevent_default])
    | _ => Many([Stop_propagation, Prevent_default])
    }
  | D("Enter") =>
    /* Step into the indicated sample */
    switch (indicated_sample(ctx), ap_id) {
    | (Some(sample), Some(ap_id)) =>
      Many([
        Stop_propagation,
        Prevent_default,
        step_into_sample(~parent, ~sample, ~ap_id),
      ])
    | _ => Many([Stop_propagation, Prevent_default])
    }
  | D(";") when key.meta == Down || key.ctrl == Down => Ignore /* Defer to page-level handler: toggle sidebar drawer mode */
  | D("/") =>
    /* Sticky-in-place toggle. Handler in web/Settings owns the
     * transition logic (including the asymmetric DockedSidebar →
     * StickyInPlace move which also clears the persisted dock). */
    Many([Settings.on_sticky_toggle^(), Stop_propagation, Prevent_default])
  | D("\\") =>
    /* Per-probe drawer-mode toggle: pretty-printed multi-line samples
     * displayed below the line, full editor-pane width. */
    Many([local(ToggleDrawerMode), Stop_propagation, Prevent_default])
  | D("c" | "C") when Key.meta_held(evt) || Key.ctrl_held(evt) =>
    switch (indicated_sample(ctx)) {
    | Some(sample) =>
      let seg = ctx.utility.term_to_seg(~inline=true, Exp(sample.value));
      let str = ctx.utility.seg_to_string(seg);
      let _ =
        Js.Unsafe.global##.navigator##.clipboard##writeText(Js.string(str));
      Many([Stop_propagation, Prevent_default]);
    | None => Many([Stop_propagation, Prevent_default])
    }
  | D("z" | "Z") when Key.ctrl_held(evt) || Key.meta_held(evt) => Ignore // Defer to parent editor undo for now
  | _ => Many([Stop_propagation])
  };
};

let empty_view = (~id: Id.t, ~settings: settings) =>
  Node.div(
    ~attrs=[
      Attr.id(Id.cls(id)),
      Attr.create("data-cursor-aligned", "false"),
      Attr.classes([
        "live-offside",
        settings.window |> Sample.Window.show_mode,
      ]),
    ],
    [
      div(
        ~attrs=[
          Attr.classes(["empty-status", "no-samples"]),
          Attr.title("This expression was never evaluated"),
        ],
        [text("∅")],
      ),
    ],
  );

/* Pre-computed data shared by every offside-view renderer (full
 * live-offside, nav-only wrapper, drawer below). Built once per
 * render so we don't redo selection/filter work for both slots. */
type offside_data = {
  ctx: probe_ctx,
  id: Id.t,
  num_total: int,
  num_shown: int,
  groups: list(list(Sample.t)),
  is_cursor_aligned: bool,
  empty_status: option(Sample.Selection.empty_status),
};

let prepare_offside =
    (
      info: info,
      local,
      parent,
      ~settings: settings,
      ~sort: Sort.t,
      ~model: probe_model,
    )
    : option(offside_data) =>
  switch (info.dynamics, info.statics) {
  | (Some(dynamics), Some(statics)) =>
    let id = info.id;
    let ap_id = Sample.Focus.cur_var_ap(statics);
    let active_renderer_id =
      Option.map(RichProbe.renderer_id_of_model, model.active_renderer);
    let ctx = {
      ap_id,
      statics,
      settings,
      dynamics,
      utility: info.utility,
      parent,
      local,
      sort,
      active_renderer_id,
    };
    let filtered_samples =
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=dynamics.sample_focus.pinned_stack,
        dynamics.samples,
      );
    let num_total = List.length(filtered_samples);
    let is_cursor_aligned =
      Sample.Selection.most_aligned_index(
        ~ap_id,
        dynamics.sample_focus,
        filtered_samples,
      )
      != None;
    let samples =
      select_samples(
        ~settings,
        ~id,
        ~ap_id,
        ~filtered=filtered_samples,
        dynamics,
      );
    let (num_shown, groups) = Sample.Selection.collate(samples);
    let is_evaluating =
      switch (dynamics.sample_focus.pending_focus) {
      | Some({probe_id, _}) => probe_id == id
      | None => false
      };
    let empty_status =
      Sample.Selection.get_empty_status(
        ~num_total,
        ~num_shown,
        ~is_evaluating,
        (),
      );
    Some({
      ctx,
      id,
      num_total,
      num_shown,
      groups,
      is_cursor_aligned,
      empty_status,
    });
  | _ => None
  };

/* Minimal wrapper used in the inline (offside) slot when drawer mode
 * is active: just the nav-bar, sitting where it would in inline mode,
 * so the controls don't visually jump when toggling drawer-mode.
 *
 * NO id / focus / key-handlers — the focusable .live-offside lives in
 * the `below` slot with the samples (see `live_offside_view`).
 * `.drawer-mode` class drives the up-arrow rotation on the toggle. */
let nav_bar_wrapper_view = (data: offside_data, ~settings: settings): Node.t => {
  let has_overflow = data.num_shown > 0 && data.num_shown < data.num_total;
  Node.div(
    ~attrs=[
      Attr.classes([
        "live-offside",
        "nav-only",
        "drawer-mode",
        settings.window |> Sample.Window.show_mode,
      ]),
    ],
    [
      nav_bar_view(
        data.ctx,
        ~num_total=data.num_total,
        ~show_arrows=has_overflow,
      ),
    ],
  );
};

/* Focusable .live-offside with the canonical id, key-handlers, and
 * cursor-alignment data attribute. Optionally includes the nav-bar
 * inline (true in inline mode; false in drawer mode, where the nav-
 * bar lives in the offside slot's `nav_bar_wrapper_view` instead). */
let live_offside_view =
    (
      ~display: sample_display,
      ~include_nav_bar: bool,
      ~drawer_mode_active: bool,
      data: offside_data,
      local,
      view_seg: View.seg,
      ~settings: settings,
    )
    : Node.t => {
  let {ctx, id, num_total, num_shown, groups, is_cursor_aligned, empty_status} = data;
  let base_classes = [
    "live-offside",
    settings.window |> Sample.Window.show_mode,
  ];
  /* Context-menu-style dismissal for the alt-click sample dropdown:
   * click-outside / window-blur closes it. Re-synced every render so the
   * close effect captures the current `local`. Idempotent across probes. */
  SampleMenuListener.sync(
    ~menu_open=Settings.open_dropdown^ != None,
    ~on_close=local(SetDropdown(None)),
    (),
  );
  Node.div(
    ~attrs=[
      Attr.id(Id.cls(id)),
      Attr.create("data-probe-id", Id.to_string(id)),
      Attr.create(
        "data-cursor-aligned",
        is_cursor_aligned ? "true" : "false",
      ),
      Attr.tabindex(0),
      Attr.on_keydown(key_handler(ctx, ~id, ~drawer_mode_active, local)),
      Attr.classes(
        base_classes @ (drawer_mode_active ? ["drawer-mode"] : []),
      ),
    ],
    switch (empty_status) {
    | Some(status) => [empty_status_view(ctx, ~status, local)]
    | None =>
      /* Block-display segments carry real linebreaks, so the layout
       * hint `single_line` must follow the display mode. */
      let single_line =
        switch (display) {
        | Inline => true
        | Block => false
        };
      let view_seg_line = (~text_only, segment) =>
        view_seg(
          ~single_line,
          ~background=false,
          ~text_only,
          Sort.Exp,
          segment,
        );
      let indicated_sample_id =
        indicated_sample(ctx) |> Option.map((s: Sample.t) => s.id);
      let sample_view =
        sample_view(
          ~display,
          ctx,
          ~indicated_sample_id,
          ~num_total,
          view_seg_line,
          local,
        );
      let group_views =
        List.map(
          samples =>
            Node.div(
              ~attrs=[Attr.classes(["sample-group"])],
              List.map(sample_view, samples),
            ),
          groups,
        );
      let samples_part =
        group_views == []
          ? []
          : [div(~attrs=[Attr.classes(["sample-groups"])], group_views)];
      let has_overflow = num_shown > 0 && num_shown < num_total;
      let nav_bar_part =
        include_nav_bar
          ? [nav_bar_view(ctx, ~num_total, ~show_arrows=has_overflow)] : [];
      let overflow_indicator = has_overflow ? [ellipsis_view(local)] : [];
      nav_bar_part @ samples_part @ overflow_indicator;
    },
  );
};

let get_current = (~settings, info: info) => {
  switch (info.dynamics, info.statics) {
  | (Some(di), Some(statics)) =>
    let ap_id = Sample.Focus.cur_var_ap(statics);
    /* First try to get the indicated closure */
    switch (Dynamics.Info.most_aligned_sample(ap_id, di)) {
    | Some(closure) => Some(closure.value)
    | None =>
      /* Fallback: get the first sample */
      let samples = select_samples(~settings, ~id=info.id, ~ap_id, di);
      ListUtil.hd_opt(samples) |> Option.map((s: Sample.t) => s.value);
    };
  | _ => None
  };
};

let overlay_view = (~settings, ~sort, info: info): Node.t =>
  switch (info.dynamics, info.statics) {
  | (Some(dynamics), Some(statics)) =>
    let ap_id = Sample.Focus.cur_var_ap(statics);
    let has_renderer =
      switch (get_current(~settings, info)) {
      | Some(exp) => Option.is_some(find_compatible_renderer(sort, exp))
      | None => false
      };
    div(
      ~attrs=[
        Attr.classes(
          ["overlay"]
          @ (Option.is_some(ap_id) ? ["ap"] : [])
          @ (has_renderer ? ["has-renderer"] : []),
        ),
      ],
      [num_samples_view(~ap_id, dynamics)],
    );
  | _ => Node.div([])
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = probe_model;
  let init_model: model = init_probe_model;
  /* Tolerate old serialized models that don't match the current shape
   * by falling back to defaults whenever sexp parsing fails. */
  let model_of_sexp = sexp =>
    try(model_of_sexp(sexp)) {
    | _ => init_model
    };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let init = (any: Any.t) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(init_model)
    | Any(_) => Some(init_model) /* Grout don't have sorts */
    | _ => None
    };
  };

  let dynamics = true;
  let elaborate_syntax = false;

  let focusable =
    Focusable.{
      pointer: Some(id => {JsUtil.get_elem_by_id(Id.cls(id))##focus}),
      keyboard: None,
    };

  let placeholder = (model: model, info) =>
    if (model.drawer_mode) {
      ProjectorCore.Shape.{
        horizontal: 0,
        vertical: Tab(DrawerHeight.compute(info)),
      };
    } else {
      ProjectorCore.Shape.default;
    };

  let update = (model: model, info: info, a: action): model => {
    switch (a) {
    | ChangeLength(id, len) =>
      SampleLength.set(id, len);
      Settings.version := Settings.version^ + 1;
      model;
    | ToggleWindowMode =>
      Settings.go(ToggleWindow);
      model;
    | ToggleDrawerMode =>
      Settings.version := Settings.version^ + 1;
      /* Toggling drawer mode moves the focusable .live-offside DOM node
       * between the offside slot (inline mode) and the below slot
       * (drawer mode). Browsers drop focus on the unmounted element, so
       * schedule a focus restoration via the after_display hook — the
       * new .live-offside is in the DOM by then and `elem.focus()`
       * sticks. Keeps the probe keyboard-active across toggles. */
      FocusEffect.schedule(info.id);
      {
        ...model,
        drawer_mode: !model.drawer_mode,
      };
    | SetDrawerMode(b) =>
      Settings.version := Settings.version^ + 1;
      FocusEffect.schedule(info.id);
      {
        ...model,
        drawer_mode: b,
      };
    | ToggleDropdown(did) =>
      Settings.set_open_dropdown(
        Settings.open_dropdown^ == Some(did) ? None : Some(did),
      );
      model;
    | SetDropdown(o) =>
      Settings.set_open_dropdown(o);
      model;
    | ResetSettings =>
      Settings.reset_mode();
      SampleLength.reset();
      model;
    | ToggleModal(pm) =>
      switch (model.active_renderer) {
      | None => {
          ...model,
          active_renderer: pm,
        }
      | Some(_) => {
          ...model,
          active_renderer: None,
        }
      }
    | RendererAction(pa) =>
      /* Dispatch through the action's renderer. update_model's internal
       * Type.Id casts no-op on a model/action mismatch, so an explicit
       * id check here is redundant. */
      switch (
        model.active_renderer,
        find(RichProbe.renderer_id_of_action(pa)),
      ) {
      | (Some(pm), Some(r)) => {
          ...model,
          active_renderer: Some(r.update_model(pm, pa)),
        }
      | _ => model
      }
    };
  };

  /* Modal overlay for dynamic renderer display */
  let modal_overlay =
      (
        ~settings,
        model,
        info,
        ~local: action => Ui_effect.t(unit),
        ~parent,
        ~view_seg,
        ~sort,
      )
      : list(Node.t) => {
    switch (model.active_renderer, get_current(~settings, info)) {
    | (Some(pm), Some(exp)) =>
      let rid = RichProbe.renderer_id_of_model(pm);
      /* Find the renderer and check if it can still handle the expression */
      switch (find(rid)) {
      | Some(renderer) when renderer.can_handle(sort, exp) =>
        let rendered =
          renderer.render_model(
            pm,
            ~info,
            ~exp,
            ~view_seg,
            ~local=pa => local(RendererAction(pa)),
            ~parent,
            ~sort,
            (),
          );
        switch (rendered) {
        | None => []
        | Some(content) => [
            div(
              ~attrs=[Attr.classes(["modal-backdrop", "live-offside"])],
              [
                div(
                  ~attrs=[
                    Attr.classes(["modal"]),
                    Attr.on_click(_ => Effect.Stop_propagation),
                  ],
                  [
                    div(
                      ~attrs=[
                        Attr.classes(["modal-close-btn"]),
                        Attr.title("Close"),
                        Attr.on_click(_ => local(ToggleModal(None))),
                      ],
                      [text("×")],
                    ),
                    content,
                  ],
                ),
              ],
            ),
          ]
        };
      | _ => []
      };
    | _ => []
    };
  };

  let error = (_, _): option(ProjectorBase.error) => None;

  let view =
      (
        {info, local, parent, view_seg, model, status, _}:
          View.args(model, action),
      ) => {
    let settings = Settings.s^;
    let sort = status.sort;
    /* Two compositional pieces (nav-bar component + samples-bearing
     * .live-offside) get routed to the projector's slots differently
     * per mode:
     *   Inline mode  — offside = .live-offside w/ nav-bar + samples
     *                  below   = none
     *   Drawer mode  — offside = nav-bar wrapper (controls stay put at
     *                            end-of-line, no samples)
     *                  below   = .live-offside w/ samples only
     * The focusable .live-offside (id, key-handlers, tabindex) always
     * goes wherever the samples live, so probe-navigation/cursor logic
     * stays attached to the same DOM element regardless of mode. */
    let data_opt =
      prepare_offside(info, local, parent, ~settings, ~sort, ~model);
    let drawer = model.drawer_mode;
    let offside_main =
      switch (data_opt, drawer) {
      | (None, _) => empty_view(~id=info.id, ~settings)
      | (Some(data), false) =>
        live_offside_view(
          ~display=Inline,
          ~include_nav_bar=true,
          ~drawer_mode_active=false,
          data,
          local,
          view_seg,
          ~settings,
        )
      | (Some(data), true) => nav_bar_wrapper_view(data, ~settings)
      };
    let modal_nodes =
      modal_overlay(
        ~settings,
        model,
        info,
        ~local,
        ~parent,
        ~view_seg,
        ~sort,
      );
    /* Wrap offside content + modal nodes in a containerless div only when
     * the modal is open; otherwise return offside_main directly so we don't
     * add an extra DOM level around the (positioned) .live-offside. */
    let offside_node =
      switch (modal_nodes) {
      | [] => offside_main
      | _ => div([offside_main] @ modal_nodes)
      };
    View.{
      inline: Node.div([]),
      overlay: Some(overlay_view(~settings, ~sort, info)),
      offside: Some(offside_node),
      below:
        switch (data_opt, drawer) {
        | (Some(data), true) =>
          Some(
            live_offside_view(
              ~display=Block,
              ~include_nav_bar=false,
              ~drawer_mode_active=true,
              data,
              local,
              view_seg,
              ~settings,
            ),
          )
        | _ => None
        },
      error: false,
    };
  };
};
