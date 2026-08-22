open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

open Js_of_ocaml;
open Language;
open RichProbe;
open RichProbeRegistry;

/* Global, UI-only probe display state (mutable refs). See ZipperBase.re. */

[@deriving (show({with_path: false}), sexp, yojson)]
type probe_model = {
  active_renderer: option(packed_model),
  drawer_mode: bool,
  /* Bumped to force a repaint when only the global open_dropdown ref
   * changed (SetModel repaints only on structural model change). */
  dropdown_redraw: int,
  /* When no renderer is explicitly active, render the first applicable
   * one automatically (canvas value wells). Off for editor probes. */
  [@default false]
  auto_rich: bool,
  /* dbl-click toggles the auto rendering back to the text view */
  [@default false]
  rich_off: bool,
};

let init_probe_model: probe_model = {
  active_renderer: None,
  drawer_mode: false,
  dropdown_redraw: 0,
  auto_rich: false,
  rich_off: false,
};

/* Any deserialization failure resets to defaults (transient UI state). */
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

/* `^^probe@<rid>` trigger-option mapping: a pin whose model selects
   renderer <rid> (in its empty state) round-trips through text. */
let model_string_for_renderer = (rid: string): option(string) =>
  RichProbeRegistry.find(rid)
  |> Option.map((r: packed_renderer) =>
       sexp_of_probe_model({
         ...init_probe_model,
         active_renderer: Some(r.empty_model),
       })
       |> Sexplib.Sexp.to_string
     );

/* Canvas wells pass their stored model (or the default) through this to
   turn on automatic rich rendering. */
let model_string_auto_rich = (stored: option(string)): string => {
  let m =
    switch (stored) {
    | Some(s) =>
      try(probe_model_of_sexp(Sexplib.Sexp.of_string(s))) {
      | _ => init_probe_model
      }
    | None => init_probe_model
    };
  {
    ...m,
    auto_rich: true,
  }
  |> sexp_of_probe_model
  |> Sexplib.Sexp.to_string;
};

let renderer_of_model_string = (model: string): option(string) =>
  switch (probe_model_of_sexp(Sexplib.Sexp.of_string(model))) {
  | {active_renderer: Some(PModel(rid, _, _)), _} => Some(rid)
  | {active_renderer: None, _} => None
  | exception _ => None
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleModal(option(packed_model))
  | RendererAction(packed_action)
  | ToggleWindowMode
  | ToggleAutoRich
  | ToggleDrawerMode
  | SetDrawerMode(bool)
  | ToggleDropdown(string)
  | SetDropdown(option(string))
  | ResetSettings;

type sample_display =
  | Inline
  | Block;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
    | Hybrid
    | StepRange
    | Simple;

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
    /* render the first applicable rich view in place of sample text for
       ANY probe (small content only — the in-chip embed); per-probe
       dbl-click still opts out */
    auto_rich_default: bool,
  };

  type set_action =
    | ToggleAutoRichDefault
    | ToggleWindow
    | SetWindow(Sample.Window.mode)
    | SetSampleBase(sample_base)
    | ToggleBeforeCutoff
    | ToggleAfterCutoff
    | ToggleCallerCutoff
    | ToggleCalleeCutoff;

  let init_drawer: drawer_settings = {width: 80};

  let init: settings = {
    window: Single,
    sample_base: Simple,
    before_cutoff: None,
    after_cutoff: None,
    caller_cutoff: None,
    callee_cutoff: None,
    drawer: init_drawer,
    auto_rich_default: true,
  };

  let skip_unaligned_nav = true;

  let update = (settings: settings, action: set_action): settings =>
    switch (action) {
    | ToggleAutoRichDefault => {
        ...settings,
        auto_rich_default: !settings.auto_rich_default,
      }
    | ToggleWindow => {
        ...settings,
        window: settings.window == Sample.Window.Single ? Many : Single,
      }
    | SetWindow(window) => {
        ...settings,
        window,
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

  /* '/' keyboard mode (orthogonal to open_dropdown): the focused sample's
   * dropdown stays open and follows arrow-nav. CSS-driven, transient. */
  let sticky = ref(false);

  /* Set by Page.view each render; lets the projector's '/' affordance
   * dispatch a global Settings update. */
  let on_sticky_toggle: ref(unit => Virtual_dom.Vdom.Effect.t(unit)) =
    ref(_ => Virtual_dom.Vdom.Effect.Ignore);

  let set_sticky = (b: bool) => {
    sticky := b;
    version := version^ + 1;
  };

  /* DOM id of the open right-click dropdown (None = closed). */
  let open_dropdown: ref(option(string)) = ref(None);
  let set_open_dropdown = (o: option(string)) => {
    open_dropdown := o;
    version := version^ + 1;
  };

  let reset_mode = () => {
    Hashtbl.clear(offset);
    s := init;
    open_dropdown := None;
    sticky := false;
    version := version^ + 1;
  };

  let go = (a: set_action): unit => {
    s := update(s^, a);
    version := version^ + 1;
  };
};

open Settings;
open Node;

type probe_ctx = {
  id: Id.t,
  ap_id: option(Id.t),
  statics: Language.Statics.Info.t,
  settings,
  dynamics: Dynamics.Info.t,
  utility: ProjectorBase.utility,
  parent: external_action => Ui_effect.t(unit),
  local: action => Ui_effect.t(unit),
  sort: Sort.t,
  active_renderer_id: option(string),
  /* auto-rich applies here: value dbl-click toggles rich <-> text
     (instead of ToggleWindowMode) */
  auto_rich_ready: bool,
  /* the explicitly chosen renderer's model, for in-value rendering */
  rich_model: option(packed_model),
  /* auto-rich is on and not toggled off */
  auto_rich_on: bool,
  /* per-probe auto (canvas wells): embed regardless of size — the
     global default only auto-embeds content that fits inline_rows_cap */
  auto_unbounded: bool,
  p_info: info,
};

module WindowState = {
  let get_offset = (k: Id.t): int =>
    switch (Hashtbl.find_opt(offset, k)) {
    | Some(v) => v
    | None => 0
    };

  let set_offset = (k: Id.t, v: int) => Hashtbl.replace(offset, k, v);

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
        ~pinned_interval=dynamics.pinned_interval,
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

let pretty_seg_of_value =
    (utility: utility, ~width: int, exp: Exp.t): Segment.t => {
  let seg =
    utility.term_to_seg(~inline=false, Exp(exp |> DHExp.strip_ascriptions));
  PrettySegment.prettify(~width, seg);
};

/* rich content at most this many rows renders IN the offside row;
   taller content lives in the drawer instead (an explicit activation
   auto-opens it) */
let inline_rows_cap = 4;

module DrawerHeight = {
  /* Cap; taller content scrolls inside `.below-wrapper`. */
  let max_rows = 15;

  /* Row count via Measured (a naive top-level linebreak scan undercounts
   * because reassemble re-nests the formatter's flat output). */
  let row_count = (seg: Segment.t): int =>
    Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty)
    |> Measured.total_rows;

  let sample_rows = (utility: utility, sample: Sample.t): int => {
    let width =
      Hashtbl.find_opt(SampleLength.lengths, sample.id)
      |> Option.value(~default=Settings.s^.drawer.width);
    row_count(pretty_seg_of_value(utility, ~width, sample.value));
  };

  /* Uncapped content height in rows. */
  let content_rows = (info: info): int =>
    switch (info.dynamics, info.statics) {
    | (Some(dynamics), Some(statics)) =>
      let settings = Settings.s^;
      let ap_id = Sample.Focus.cur_var_ap(statics);
      let samples = select_samples(~settings, ~id=info.id, ~ap_id, dynamics);
      switch (samples) {
      | [] => 1
      | _ =>
        let heights = List.map(sample_rows(info.utility), samples);
        List.fold_left(max, 1, heights);
      };
    | _ => 1
    };

  let compute = (info: info): int => min(max_rows, content_rows(info));
};

let pos_rel_to_target = (e: Js.t(Dom_html.mouseEvent)): option(Point.t) => {
  open Float;
  let (col_width, row_height) = JsUtil.font_metrics_from_specimen();
  let text_box =
    e##.currentTarget
    |> Js.Opt.to_option
    |> Option.map(JsUtil.get_child_with_class(_, "code"))
    |> Option.join;
  switch (text_box) {
  | None => None
  | Some(text_box) =>
    let x_rel = of_int(e##.clientX) -. text_box##getBoundingClientRect##.left;
    let y_rel = of_int(e##.clientY) -. text_box##getBoundingClientRect##.top;
    let row = to_int(y_rel /. row_height);
    let col = to_int(round(x_rel /. col_width));
    Some({
      row,
      col,
    });
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
        ["focus"];
      } else {
        step_range_clss();
      };
    }
  | Simple =>
    let relation =
      Sample.Focus.relation(
        ~trimmed=true,
        ~ap_id,
        dynamics.sample_focus,
        sample,
      );
    relation.is_call_cursor ? ["focus"] : ["off-focus"];
  };
};

let cursor_clss =
    (~settings, ~ap_id, dynamics: Dynamics.Info.t, sample: Sample.t)
    : list(string) => {
  color_clss(~settings, ~ap_id, dynamics, sample)
  @ depth_clss(~settings, ~ap_id, dynamics, sample);
};

module Debug = {
  let stack = (stack: CallStack.t): string =>
    stack
    |> List.map((f: CallStack.frame) => Id.str3(f.id))
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

let pin_call = (ctx: probe_ctx) =>
  switch (ctx.ap_id, Dynamics.Info.is_in(ctx.dynamics)) {
  | (Some(ap_id), Some(sample)) =>
    let call_stack = CallStack.extend(ap_id, sample.call_stack);
    ctx.parent(Probe(Pin(call_stack, ap_id)));
  | _ => Effect.Ignore
  };

let focus_call = (ctx: probe_ctx) =>
  switch (Dynamics.Info.is_in(ctx.dynamics)) {
  | Some(sample) when sample.call_stack != [] =>
    ctx.parent(
      SampleFocus(
        TogglePin(
          sample.call_stack,
          Some(Sample.capture_of_sample(sample)),
        ),
      ),
    )
  | _ => Effect.Ignore
  };

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
      /* Thunk, not a prebuilt effect: local(action) runs eagerly, so a
         bare effect would toggle on every render instead of on click. */
      ~alt_toggle: unit => Ui_effect.t(unit),
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
    /* buttons > 0 guards a stale drag flag: capture can be lost without a
       pointerup, which would otherwise resize on shift-hover with no button. */
    let buttons: int = Js.Unsafe.get(e, "buttons");
    switch (
      ValueState.mousedown^,
      Js.to_bool(e##.shiftKey) && buttons > 0 ? pos_rel_to_target(e) : None,
    ) {
    | (Some(_), Some(goal)) =>
      let target_width = max(1, goal.col);
      /* Inline: width isn't linear in budget (Abbreviate is discrete), so
       * bisect. Block: wrap width is target_width directly. */
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
      Attr.on_double_click(_ =>
        ctx.auto_rich_ready ? local(ToggleAutoRich) : local(ToggleWindowMode)
      ),
      /* Suppress the native menu (Ctrl is the escape hatch to it). */
      Attr.on_contextmenu(evt =>
        Key.ctrl_held(evt)
          ? Effect.Ignore
          : Effect.Many([Effect.Stop_propagation, Effect.Prevent_default])
      ),
      Attr.on_pointerdown(evt => {
        let button: int = Js.Unsafe.coerce(evt)##.button;
        let alt = Js.to_bool(Js.Unsafe.coerce(evt)##.altKey);
        let ctrl = Key.ctrl_held(evt);
        button == 2 && ctrl
          /* Ctrl + right-click defers to the native menu. */
          ? Effect.Ignore
          : button == 2 || alt
              /* Right-click/alt-click opens the sample dropdown; stop the
                 event before the editor's own pointerdown menu. Also fires
                 the Focus + sample-capture a normal click would. */
              ? Effect.Many([
                  ctx.parent(FocusById(ctx.id)),
                  val_pointerdown(evt),
                  alt_toggle(),
                  Effect.Stop_propagation,
                  Effect.Prevent_default,
                ])
              : Key.meta_held(evt)
                  /* Stop propagation: Pin can reorder the refractor list,
                     invalidating the wrapper's render-time Focus idx. */
                  ? Effect.Many([
                      Effect.Stop_propagation,
                      Option.is_some(ctx.ap_id)
                        ? pin_call(ctx) : focus_call(ctx),
                    ])
                  : val_pointerdown(evt);
      }),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    {
      /* rich content renders INSIDE the sample chip, inert
         (pointer-events: none), so the chip keeps every sample
         interaction: right/alt-click dropdown (with Hide), click to
         capture, dbl-click toggles. Explicit renderers embed when they
         fit inline_rows_cap (taller ones live in the drawer); auto-rich
         (wells) embeds unconditionally. */
      let render_rich = (r: packed_renderer, pm: packed_model) =>
        r.render_model(
          pm,
          ~info=ctx.p_info,
          ~exp=sample.value,
          ~view_seg=(_, sg) => view_seg(~text_only=false, sg),
          ~local=pa => local(RendererAction(pa)),
          ~parent=ctx.parent,
          ~sort=ctx.sort,
          (),
        );
      let rich_node =
        switch (ctx.rich_model) {
        | Some(pm) =>
          switch (find(RichProbe.renderer_id_of_model(pm))) {
          | Some(r)
              when
                r.can_handle(ctx.sort, sample.value)
                && (
                  switch (r.drawer_rows(ctx.sort, sample.value)) {
                  | Some(n) => n <= inline_rows_cap
                  | None => true
                  }
                ) =>
            render_rich(r, pm)
          | _ => None
          }
        | None when ctx.auto_rich_on =>
          switch (
            List.find_opt(
              (r: packed_renderer) =>
                r.id != "table"
                && r.can_handle(ctx.sort, sample.value)
                && (
                  ctx.auto_unbounded
                  || (
                    switch (r.drawer_rows(ctx.sort, sample.value)) {
                    | Some(n) => n <= inline_rows_cap
                    | None => true
                    }
                  )
                ),
              renderers,
            )
          ) {
          | Some(r) =>
            switch (r.init_model(ctx.sort, sample.value)) {
            | Some(pm) => render_rich(r, pm)
            | None => None
            }
          | None => None
          }
        | None => None
        };
      switch (rich_node) {
      | Some(n) => [div(~attrs=[Attr.classes(["value-rich"])], [n])]
      | None => [view_seg(~text_only=false, seg)]
      };
    },
  );
};

/* Standalone rich rendering for ONE value, outside any probe: the
   in-chip auto logic (first non-table matching renderer, inert) without
   the sample-stream machinery. Aggregate/type wells use this — mixing
   samples from different probes into one navigable stream would break
   the indication/window invariants, so they render value chips instead. */
let standalone_rich =
    (~info: info, ~sort: Sort.t, ~view_seg, value: Exp.t): option(Node.t) =>
  switch (
    List.find_opt(
      (r: packed_renderer) => r.id != "table" && r.can_handle(sort, value),
      renderers,
    )
  ) {
  | Some(r) =>
    switch (r.init_model(sort, value)) {
    | Some(pm) =>
      r.render_model(
        pm,
        ~info,
        ~exp=value,
        ~view_seg,
        ~local=_ => Ui_effect.Ignore,
        ~parent=_ => Ui_effect.Ignore,
        ~sort,
        (),
      )
    | None => None
    }
  | None => None
  };

/* Hard cap for code in the sample dropdown (env values + call args), so a
 * wide sample doesn't make them uselessly long. */
let dropdown_value_width = 50;

let env_val =
    (ctx: probe_ctx, view_seg, _sample, en: Sample.Env.entry): Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ " ≡ "),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        let (seg, _) =
          abbreviated_seg_of(ctx.utility, dropdown_value_width, d);
        view_seg(~text_only=false, seg);
      },
    ],
  );
};

let show_pin = (ctx: probe_ctx, sample: Sample.t) => {
  switch (ctx.ap_id, ctx.dynamics.sample_focus.pinned_stack) {
  | (Some(ap_id), Some(pinned_stack)) =>
    CallStack.equal(pinned_stack, CallStack.extend(ap_id, sample.call_stack))
  | _ => false
  };
};

let show_focus = (ctx: probe_ctx, sample: Sample.t) =>
  switch (ctx.dynamics.sample_focus.pinned_stack) {
  | Some(pinned_stack) => CallStack.equal(pinned_stack, sample.call_stack)
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

/* sample.id is collision-resistant + stable across re-eval, so an open dropdown keeps its identity. */
let dropdown_id = (sample: Sample.t): string =>
  Printf.sprintf("sample-dropdown-%d", sample.id);

let step_into_sample =
    (~parent, ~local, ~sample: Sample.t, ~ap_id: Id.t): Ui_effect.t(unit) => {
  /* ap_id (from statics) enriched with the sample's recorded fn_name/def_id. */
  let dyn = sample.frame;
  let frame: CallStack.frame = {
    id: ap_id,
    name: Option.bind(dyn, (f: CallStack.frame) => f.name),
    fn_def_id: Option.bind(dyn, (f: CallStack.frame) => f.fn_def_id),
  };
  Effect.Many([
    local(SetDropdown(None)),
    parent(Probe(StepInto(sample.call_stack, frame))),
  ]);
};

/* Step-into possible when: (static) the call is a named non-builtin var, or
 * (dynamic) the sample recorded a navigable fn_def_id from user code — the
 * latter covers higher-order and partial-application calls. */
let can_step_into = (statics: Language.Statics.Info.t, sample: Sample.t): bool => {
  let fn_var_name =
    switch (statics) {
    | InfoExp({user_term: {term: Ap(_, {term: Var(name), _}, _), _}, _}) =>
      Some(name)
    | _ => None
    };
  let is_builtin = name =>
    Environment.lookup(Builtins.env_init, name) != None;
  let static_ok =
    switch (fn_var_name) {
    | Some(name) => !is_builtin(name)
    | None => false
    };
  /* Suppress the dynamic offer for direct builtin call sites: their fn_def_id
     points to library code with nowhere to step. */
  let static_fn_is_builtin =
    switch (fn_var_name) {
    | Some(name) => is_builtin(name)
    | None => false
    };
  let dynamic_ok =
    !static_fn_is_builtin
    && (
      switch (sample.frame) {
      | Some(f) => Option.is_some(f.fn_def_id)
      | None => false
      }
    );
  static_ok || dynamic_ok;
};

let pin_action = (ctx: probe_ctx, sample: Sample.t) => {
  let is_pinned = show_pin(ctx, sample);
  div(
    ~attrs=[
      Attr.classes(
        ["action-item", "pin-action"] @ (is_pinned ? ["pinned"] : []),
      ),
      Attr.on_pointerdown(_
        /* Stop propagation: Pin can reorder the refractor list, invalidating
           the wrapper's render-time Focus idx. */
        => Effect.Many([Effect.Stop_propagation, pin_call(ctx)])),
    ],
    [
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
      Attr.on_pointerdown(_
        /* Stop propagation: see pin_action above */
        => Effect.Many([Effect.Stop_propagation, focus_call(ctx)])),
    ],
    [
      text(is_focused ? "Unpin enclosing call" : "Pin enclosing call"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("P")]),
    ],
  );
};

let step_into_action = (ctx: probe_ctx, sample: Sample.t, ap_id: Id.t) =>
  div(
    ~attrs=[
      Attr.classes(["action-item", "step-into-action"]),
      Attr.on_pointerdown(_
        /* Stop propagation so the wrapper's Focus doesn't move the cursor
           back after we jump. */
        =>
          Effect.Many([
            Effect.Stop_propagation,
            step_into_sample(
              ~parent=ctx.parent,
              ~local=ctx.local,
              ~sample,
              ~ap_id,
            ),
          ])
        ),
    ],
    [
      text("Step into"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("Enter")]),
    ],
  );

let rich_probe_action =
    (ctx: probe_ctx, sample: Sample.t, r: packed_renderer): Node.t => {
  let is_active = ctx.active_renderer_id == Some(r.id);
  let label = (is_active ? "Hide " : "View as ") ++ r.id;
  div(
    ~attrs=[
      Attr.classes(["action-item", "rich-probe-action"]),
      Attr.on_pointerdown(_
        /* Stop propagation so the wrapper's Focus doesn't also fire. */
        =>
          Effect.Many([
            Effect.Stop_propagation,
            ctx.local(ToggleModal(r.init_model(ctx.sort, sample.value))),
          ])
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

let sample_primary_actions =
    (
      ctx: probe_ctx,
      ~can_step_into: bool,
      ~include_rich: bool=true,
      sample: Sample.t,
    )
    : list(Node.t) => {
  let rich_items = include_rich ? rich_probe_items(ctx, sample) : [];
  switch (ctx.ap_id) {
  | Some(ap_id) =>
    [pin_action(ctx, sample)]
    @ (can_step_into ? [step_into_action(ctx, sample, ap_id)] : [])
    @ rich_items
  | None when sample.call_stack != [] =>
    [focus_action(ctx, sample)] @ rich_items
  | None => rich_items
  };
};

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

let fn_header = (fn_name: string): list(Node.t) => [
  Node.span(~attrs=[Attr.classes(["fn-name"])], [Node.text(fn_name)]),
  Node.span(~attrs=[Attr.classes(["paren"])], [Node.text("(")]),
];

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

let sample_call_display =
    (ctx: probe_ctx, view_seg, sample: Sample.t): list(Node.t) =>
  switch (sample.args, get_fn_name_from_statics(ctx.statics)) {
  | (Some(arg_val), Some(fn_name)) =>
    let length = dropdown_value_width;
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

let filtered_env_entries =
    (~filter_vars: list(string), sample: Sample.t): list(Sample.Env.entry) =>
  sample.env
  |> ListUtil.dedup
  |> Sample.Env.remove_opaques
  |> List.filter((en: Sample.Env.entry) =>
       !List.mem(en.binding.name, filter_vars)
     );

/* Variable bindings. filter_vars: names already shown in the call display. */
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

let sample_context_sections =
    (ctx: probe_ctx, ~include_rich: bool=true, view_seg, sample: Sample.t)
    : (bool, bool, list(Node.t)) => {
  let filter_vars = List.filter_map(Fun.id, get_arg_var_info(ctx.statics));
  let env_elems = filtered_env_entries(~filter_vars, sample);
  let has_env = env_elems != [];
  let has_call = Option.is_some(sample.args);
  let primary =
    sample_primary_actions(
      ctx,
      ~can_step_into=can_step_into(ctx.statics, sample),
      ~include_rich,
      sample,
    );
  let call_display = sample_call_display(ctx, view_seg, sample);
  let env = sample_environment(ctx, ~filter_vars, view_seg, sample);
  /* Emit no nodes when empty so the caller suppresses the whole menu. */
  let nodes =
    primary == [] && call_display == [] && env == []
      ? []
      : [div(~attrs=[Attr.classes(["context-actions"])], primary)]
        @ call_display
        @ env;
  (has_env, has_call, nodes);
};

let sample_context_menu =
    (~show_env, ~drawer, ctx: probe_ctx, view_seg, sample: Sample.t)
    : list(Node.t) => {
  let (has_env, has_call, nodes) =
    sample_context_sections(ctx, view_seg, sample);
  /* In drawer mode `.below-wrapper`'s overflow would clip the menu, so promote it to a FloatingElement (position:fixed, tracked to its anchor). */
  let floating = drawer && show_env;
  let float_attrs =
    floating
      ? [
        Attr.create("data-float-anchor-class", "sample"),
        Attr.create("data-float-anchor-edge", "bottom"),
        Attr.create("data-float-local-top", "0"),
        Attr.create("data-float-local-left", "3"),
        /* Start hidden; update_all() positions + reveals after measuring. */
        Attr.create(
          "style",
          "position: fixed; visibility: hidden; top: 0; left: 0;",
        ),
      ]
      : [];
  switch (nodes) {
  | [] => []
  | _ => [
      div(
        ~attrs=
          [
            Attr.classes(
              ["sample-context-menu"]
              @ (floating ? ["floating-fixed"] : [])
              @ (has_env || has_call ? [] : ["no-env"])
              @ (show_env ? ["dropdown-active"] : []),
            ),
            Attr.id(dropdown_id(sample)),
          ]
          @ float_attrs,
        nodes,
      ),
    ]
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
  let is_indicated = indicated_sample_id == Some(sample.id);
  let did = dropdown_id(sample);
  let show_env = Settings.open_dropdown^ == Some(did);
  /* Only build the dropdown when something can reveal it (open or sticky):
   * building it for every sample dominated render cost in auto-probe All
   * mode. open_dropdown/sticky bump Settings.version to invalidate the cache. */
  let render_dropdown =
    show_env
    || Settings.sticky^
    && {
      let hide_env = hide_env(ctx.statics);
      let has_rich =
        switch (Dynamics.Info.most_aligned_sample(ctx.ap_id, ctx.dynamics)) {
        | Some(indicated) =>
          List.exists(
            r => r.can_handle(ctx.sort, indicated.value),
            renderers,
          )
        | None => false
        };
      !(hide_env && ctx.ap_id == None) || sample.call_stack != [] || has_rich;
    };
  /* `indicated-sample` marks the most-aligned sample, the DOM anchor
   * SampleAnchor uses to compensate scroll on Left/Right.
   * NB: samples are deliberately NOT `menu-trigger` — that would exempt them
   * from click-outside dismissal and break closing a dropdown by clicking
   * another sample; the opening click is protected by the listener instead. */
  let sample_classes =
    ["sample"] @ (is_indicated ? ["indicated-sample"] : []);
  div(
    /* data-sample-id targets the exact sample for SampleAnchor; the
     * indicated-sample class can tie and mark several at once. */
    ~attrs=[
      Attr.classes(sample_classes),
      Attr.create("data-sample-id", string_of_int(sample.id)),
    ],
    [
      value_view(
        ~display,
        ~alt_toggle=() => local(ToggleDropdown(did)),
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
        ? sample_context_menu(
            ~show_env,
            ~drawer=display == Block,
            ctx,
            view_seg,
            sample,
          )
        : []
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
      ~pinned_interval=dynamics.pinned_interval,
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
    div(
      ~attrs=[
        Attr.classes(["empty-status", "not-aligned"]),
        Attr.title("Samples not aligned with focus — click to align"),
        Attr.on_pointerdown(mv_least_distant_sample(ctx)),
        Attr.on_double_click(_ =>
          ctx.auto_rich_ready
            ? local(ToggleAutoRich) : local(ToggleWindowMode)
        ),
      ],
      [text("⊖")],
    )
  | Evaluating =>
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
      ~pinned_interval=dynamics.pinned_interval,
      dynamics.samples,
    );
  let cursor_idx =
    Sample.Selection.most_aligned_index(
      ~ap_id,
      dynamics.sample_focus,
      samples,
    );
  switch (cursor_idx) {
  | Some(idx) =>
    let next_idx_maybe = idx - offset;
    if (next_idx_maybe >= 0 && next_idx_maybe < List.length(samples)) {
      let sample = List.nth(samples, next_idx_maybe);
      /* Anchor scroll only when the indication actually moves (an arrow at
       * the ends is a no-op), scoped to this probe+sample. */
      SampleAnchor.capture(~scope=Id.cls(ctx.id), ~sample_id=sample.id, ());
      parent(
        SampleFocus(Capture(Sample.capture_of_sample(sample), ap_id)),
      );
    } else {
      Effect.Ignore;
    };
  | _ => Effect.Ignore
  };
};

/* Chevron hit-rect drawn LAST so it wins z-order over the diamond's bottom
 * corners; shape centered at (0,0) so CSS `scaleY(-1)` flips it in drawer mode. */
let nav_bar_view = (ctx: probe_ctx, ~num_total, ~show_arrows: bool) => {
  let (disable_left, disable_right) = {
    let samples =
      Sample.Selection.filter_by_pin(
        ~ap_id=ctx.ap_id,
        ~pinned=ctx.dynamics.sample_focus.pinned_stack,
        ~pinned_interval=ctx.dynamics.pinned_interval,
        ctx.dynamics.samples,
      );
    switch (
      Sample.Selection.most_aligned_index(
        ~ap_id=ctx.ap_id,
        ctx.dynamics.sample_focus,
        samples,
      )
    ) {
    | Some(idx) => (idx <= 0, idx >= num_total - 1)
    | None => (true, true)
    };
  };
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
  let meta = Util.Os.is_mac^ ? {js|⌘|js} : "Ctrl+";
  let arrow_group = (~side, ~disabled: bool, ~offset: int, ~hit_x: string) => {
    /* A disabled arrow stays clickable but inert: swallow pointerdown so it
     * doesn't fall through to the editor and move the caret. */
    let event_attrs =
      disabled
        ? [Attr.on_pointerdown(_ => Effect.Stop_propagation)]
        : [Attr.on_click(_ => move_cursor(ctx, offset))];
    Node.create_svg(
      "g",
      ~attrs=
        [Attr.classes(["nav-" ++ side] @ (disabled ? ["disabled"] : []))]
        @ event_attrs,
      [
        title(side == "left" ? "Previous sample (←)" : "Next sample (→)"),
        polygon(side == "left" ? "-7,-2 -1,-8 -1,4" : "1,-8 7,-2 1,4"),
        hit_rect(~x=hit_x, ~y="-10", ~w="9", ~h="11"),
      ],
    );
  };
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

let num_samples_view = (~count: int) => {
  let description = count < 1000 ? string_of_int(count) : "1k+";
  let tooltip = string_of_int(count) ++ (count == 1 ? " sample" : " samples");
  div(
    ~attrs=[Attr.title(tooltip), Attr.classes(["num-samples"])],
    [text(description)],
  );
};

/* The sample-count circle, or nothing. Hidden when there's only a single
 * sample; change the guard to `count >= 1` to always show it. It sits at
 * the start of the samples row (see live_offside_view); in drawer mode CSS
 * pins it in the left gutter near the top. */
let count_badge_nodes = (~count: int) =>
  count > 1 ? [num_samples_view(~count)] : [];

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
  /* expect_blur stops the focus keeper re-grabbing the probe; blur drops focus; schedule_editor restores it to .code-editor after render. */
  let blur_to_editor = () => {
    FocusEffect.expect_blur();
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    FocusEffect.schedule_editor();
  };
  switch (key.key) {
  | D("E" | "e") when key.meta == Down || key.ctrl == Down => parent(Remove)
  | D("Escape") when Settings.open_dropdown^ != None =>
    Many([local(SetDropdown(None)), Stop_propagation, Prevent_default])
  | D("Escape") when key.shift == Down =>
    blur_to_editor();
    Many([local(ResetSettings), parent(SampleFocus(Reset))]);
  | D("Escape") when drawer_mode_active =>
    Many([local(SetDrawerMode(false)), Stop_propagation, Prevent_default])
  | D("Escape") =>
    blur_to_editor();
    Many([Stop_propagation, Prevent_default]);
  | D("Enter") when key.meta == Down || key.ctrl == Down =>
    blur_to_editor();
    Many([
      parent(EscapeToLineEnd(Probe)),
      Stop_propagation,
      Prevent_default,
    ]);
  | D("ArrowLeft") when key.meta == Down || key.ctrl == Down =>
    blur_to_editor();
    Many([
      parent(EscapeToLineEnd(Probe)),
      Stop_propagation,
      Prevent_default,
    ]);
  | D("Home") =>
    blur_to_editor();
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
    /* Prevent_default stops aggressive horizontal scroll. */
    Many([move_cursor(ctx, -1), Stop_propagation, Prevent_default])
  | D("ArrowLeft") =>
    Many([move_cursor(ctx, 1), Stop_propagation, Prevent_default])
  | D("ArrowDown") when key.meta == Down || key.ctrl == Down =>
    /* Enter drawer mode (Cmd/Ctrl+ArrowUp exits). */
    Many([local(SetDrawerMode(true)), Stop_propagation, Prevent_default])
  | D("ArrowUp") when key.meta == Down || key.ctrl == Down =>
    /* Exit drawer mode. */
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
        step_into_sample(~parent, ~local, ~sample, ~ap_id),
      ])
    | _ => Many([Stop_propagation, Prevent_default])
    }
  | D("/") =>
    /* Toggle sticky mode (web/Settings owns it). */
    Many([Settings.on_sticky_toggle^(), Stop_propagation, Prevent_default])
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
    let auto_rich_ready =
      (model.auto_rich || settings.auto_rich_default)
      && model.active_renderer == None
      && (
        switch (Dynamics.Info.most_aligned_sample(ap_id, dynamics)) {
        | Some(sample) =>
          List.exists(
            (r: packed_renderer) =>
              r.id != "table" && r.can_handle(sort, sample.value),
            renderers,
          )
        | None => false
        }
      );
    let ctx = {
      id,
      ap_id,
      statics,
      settings,
      dynamics,
      utility: info.utility,
      parent,
      local,
      sort,
      active_renderer_id,
      auto_rich_ready,
      rich_model: model.active_renderer,
      auto_rich_on:
        (model.auto_rich || settings.auto_rich_default) && !model.rich_off,
      auto_unbounded: model.auto_rich,
      p_info: info,
    };
    let filtered_samples =
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=dynamics.sample_focus.pinned_stack,
        ~pinned_interval=dynamics.pinned_interval,
        dynamics.samples,
      );
    let num_total = List.length(filtered_samples);
    /* Arrow Up/Down probe-navigation reachability. When pinned, this
     * restricts motion to probes that won't silently realign the dynamic
     * cursor. Written to the DOM as `data-cursor-aligned`, read back by
     * `JsUtil.navigate_probes` when walking visually-adjacent probes.
     *
     * Short-form logic (pinned case):
     *
     *   A probe is reachable iff it has a (pin-filter-visible) sample
     *   whose call_stack either:
     *     (a) equals the cursor's effective_stack — same invocation, no
     *         state change on arrow, OR
     *     (b) is stack-related (one is a suffix of the other) AND the
     *         target's enclosing fn_def_id differs from the cursor's —
     *         walking up the call chain or back down across a function
     *         body boundary. Bidirectional so walking up has an inverse.
     *
     *   fn_def_id is the dynamic "which function body am I in" test, read
     *   from the innermost frame of effective_stack for the cursor and
     *   from any sample's innermost frame for the target (all samples of
     *   a probe share an enclosing fn).
     *
     * Principles upheld:
     *   1. Reachable ⊆ Visible (filtered_samples is already pinned).
     *   2. No silent realignment: moves never change which sample other
     *      probes display (rule (a) is strict equality; rule (b) crosses
     *      a fn boundary, which is a legitimate semantic step-out/in).
     *   3. Cycle / no traps: bidirectional rule (b) means walking up
     *      always has a walk-back-down inverse in the reachable set.
     *   4. Pin-gated: unpinned retains legacy `most_aligned_index`.
     *
     * Recursion compromise: because rule (b) requires a different fn
     * body, same-fn-body ancestor ap probes (i.e., recursive self-call
     * sites from inside a recursive function) are not arrow-reachable.
     * To switch recursion levels, navigate up to the function parameter
     * and use the focus bar's left/right (call-stack axis). Rationale:
     * within a recursive function, lexical and dynamic geometry diverge,
     * and blocking cross-invocation same-fn moves is the minimal fix
     * that keeps (2) — no realignment — intact.
     *
     * Orientation note: step_end order over reachable samples coincides
     * with visual (top/left) order in Hazel's top-down source, because a
     * computation always finishes before its enclosing computation does.
     * That's why `JsUtil.navigate_probes` can sort visually and get the
     * same answer a pure-dynamic step_end formulation would give. */
    let is_cursor_aligned =
      switch (dynamics.sample_focus.pinned_stack) {
      | None =>
        /* Unpinned: legacy alignment (looser, has fallback tiers). */
        Sample.Selection.most_aligned_index(
          ~ap_id,
          dynamics.sample_focus,
          filtered_samples,
        )
        != None
      | Some(_) =>
        Sample.Selection.is_reachable_pinned(
          ~cursor=dynamics.sample_focus,
          filtered_samples,
        )
      };
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

/* In drawer mode the inline slot holds just the nav-bar (so controls don't jump
 * when toggling); the focusable .live-offside lives in the `below` slot. */
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

let live_offside_view =
    (
      ~display: sample_display,
      ~include_nav_bar: bool,
      ~drawer_mode_active: bool,
      /* When set (drawer mode, rich renderer active), replaces the sample
       * views entirely — the drawer shows the rich rendering instead. */
      ~rich_content: option(Node.t)=None,
      /* Content taller than the drawer cap; gates the wrapper's
       * scroll-affordance fade via the drawer-overflow class. */
      ~scrollable: bool=false,
      data: offside_data,
      local,
      view_seg: View.seg,
      ~settings: settings,
    )
    : Node.t => {
  let {ctx, id, num_total, num_shown, groups, is_cursor_aligned, empty_status} = data;
  let base_classes =
    ["live-offside", settings.window |> Sample.Window.show_mode]
    @ (Settings.sticky^ ? ["sticky"] : [])
    @ (scrollable ? ["drawer-overflow"] : []);
  /* on_close is a thunk: a bare local(SetDropdown(None)) would fire every render. */
  SampleMenuListener.sync(
    ~menu_open=Settings.open_dropdown^ != None,
    ~on_close=() => local(SetDropdown(None)),
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
    switch (empty_status, rich_content) {
    | (Some(status), _) => [empty_status_view(ctx, ~status, local)]
    | (None, Some(content)) => [content]
    | (None, None) =>
      /* single_line follows the display mode (Block has real linebreaks). */
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
      /* Sample-count circle at the start of the samples row. */
      let count_badge = count_badge_nodes(~count=num_total);
      let samples_part =
        group_views == []
          ? []
          : [
            div(
              ~attrs=[Attr.classes(["sample-groups"])],
              count_badge @ group_views,
            ),
          ];
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
    switch (Dynamics.Info.most_aligned_sample(ap_id, di)) {
    | Some(closure) => Some(closure.value)
    | None =>
      let samples = select_samples(~settings, ~id=info.id, ~ap_id, di);
      ListUtil.hd_opt(samples) |> Option.map((s: Sample.t) => s.value);
    };
  | _ => None
  };
};

/* The active rich renderer's rendering of the indicated sample's value,
 * if that renderer still applies to it. */
let rich_content =
    (
      ~settings,
      model: probe_model,
      info: info,
      ~local: action => Ui_effect.t(unit),
      ~parent,
      ~view_seg,
      ~sort,
    )
    : option(Node.t) =>
  switch (model.active_renderer, get_current(~settings, info)) {
  | (Some(pm), Some(exp)) =>
    switch (find(RichProbe.renderer_id_of_model(pm))) {
    | Some(renderer) when renderer.can_handle(sort, exp) =>
      renderer.render_model(
        pm,
        ~info,
        ~exp,
        ~view_seg,
        ~local=pa => local(RendererAction(pa)),
        ~parent,
        ~sort,
        (),
      )
    | _ => None
    }
  | _ => None
  };

/* Chrome around a rich rendering replacing the drawer's sample view.
 * `overflowing` marks content taller than the reserved rows, letting CSS
 * keep `.below-wrapper`'s overflow clip (for scrolling) only when needed —
 * otherwise the clip would cut off the table's column menus. */
let rich_drawer_view =
    (
      ~local: action => Ui_effect.t(unit),
      ~overflowing: bool,
      ~closable: bool=true,
      content: Node.t,
    )
    : Node.t =>
  div(
    ~attrs=[
      Attr.classes(["rich-drawer"] @ (overflowing ? ["overflowing"] : [])),
    ],
    (
      closable
        ? [
          div(
            ~attrs=[
              Attr.classes(["rich-drawer-close"]),
              Attr.title("Close"),
              Attr.on_click(_ => local(ToggleModal(None))),
            ],
            [text("×")],
          ),
        ]
        : []
    )
    @ [content],
  );

/* Rows the active rich renderer wants in the drawer, when it applies to
 * the indicated value. */
let rich_drawer_rows = (model: probe_model, info: info): option(int) => {
  let sort =
    switch (info.statics) {
    | Some(statics) => Language.Statics.Info.sort_of(statics)
    | None => Sort.Exp
    };
  switch (model.active_renderer) {
  | Some(pm) =>
    switch (
      find(RichProbe.renderer_id_of_model(pm)),
      get_current(~settings=Settings.s^, info),
    ) {
    | (Some(r), Some(exp)) => r.drawer_rows(sort, exp)
    | _ => None
    }
  | None when model.auto_rich =>
    switch (get_current(~settings=Settings.s^, info)) {
    | Some(exp) =>
      List.find_opt(
        (r: packed_renderer) => r.id != "table" && r.can_handle(sort, exp),
        renderers,
      )
      |> Option.map((r: packed_renderer) => r.drawer_rows(sort, exp))
      |> Option.join
    | None => None
    }
  | None => None
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = probe_model;
  let init_model: model = init_probe_model;
  /* Fall back to defaults on any sexp parse failure (old serialized models). */
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
      pointer:
        Some(
          id =>
            /* A stale Focus idx may resolve to a probe with no DOM; a missed
               focus beats a crash. */
            switch (JsUtil.get_elem_by_id_opt(Id.cls(id))) {
            | Some(elem) => elem##focus
            | None => ()
            },
        ),
      keyboard: None,
    };

  let placeholder = (model: model, info) =>
    if (model.drawer_mode) {
      let rows =
        switch (rich_drawer_rows(model, info)) {
        | Some(n) => min(DrawerHeight.max_rows, n)
        | None => DrawerHeight.compute(info)
        };
      ProjectorCore.Shape.{
        horizontal: 0,
        vertical: Tab(rows),
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
    | ToggleAutoRich => {
        ...model,
        rich_off: !model.rich_off,
      }
    | ToggleDrawerMode =>
      Settings.version := Settings.version^ + 1;
      /* Toggling moves the focusable .live-offside between DOM slots, which
       * drops focus; schedule a restore via after_display. */
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
      {
        ...model,
        dropdown_redraw: model.dropdown_redraw + 1,
      };
    | SetDropdown(o) =>
      Settings.set_open_dropdown(o);
      {
        ...model,
        dropdown_redraw: model.dropdown_redraw + 1,
      };
    | ResetSettings =>
      Settings.reset_mode();
      SampleLength.reset();
      model;
    | ToggleModal(pm) =>
      switch (model.active_renderer) {
      | None =>
        /* activation: content taller than the inline cap opens the
           drawer (chevron / Cmd+ArrowUp toggles back) */
        let wants_drawer =
          switch (
            rich_drawer_rows(
              {
                ...model,
                active_renderer: pm,
              },
              info,
            )
          ) {
          | Some(n) => n > inline_rows_cap
          | None => false
          };
        {
          ...model,
          active_renderer: pm,
          drawer_mode: model.drawer_mode || wants_drawer,
        };
      | Some(_) => {
          ...model,
          active_renderer: None,
        }
      }
    | RendererAction(pa) =>
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
    switch (
      rich_content(~settings, model, info, ~local, ~parent, ~view_seg, ~sort)
    ) {
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
              (
                model.active_renderer != None
                  ? [
                    div(
                      ~attrs=[
                        Attr.classes(["modal-close-btn"]),
                        Attr.title("Close"),
                        Attr.on_click(_ => local(ToggleModal(None))),
                      ],
                      [text("×")],
                    ),
                  ]
                  : []
              )
              @ [content],
            ),
          ],
        ),
      ]
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
    /* Inline mode: offside = .live-offside (nav-bar + samples), below = none.
     * Drawer mode: offside = nav-bar wrapper, below = .live-offside (samples).
     * The focusable .live-offside always goes wherever the samples live. */
    let data_opt =
      prepare_offside(info, local, parent, ~settings, ~sort, ~model);
    let drawer = model.drawer_mode;
    let offside_main =
      switch (data_opt, drawer) {
      | (None, _) => empty_view(~id=info.id, ~settings)
      | (Some(data), false) =>
        /* rich content embeds inside each sample chip (value_view);
           no whole-row replacement in inline mode */
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
    /* Content taller than the drawer cap → the drawer scrolls; gates the
     * wrapper's scroll-affordance fade and the rich view's overflow clip. */
    let drawer_overflow =
      drawer
      && (
        switch (rich_drawer_rows(model, info)) {
        | Some(n) => n > DrawerHeight.max_rows
        | None => DrawerHeight.content_rows(info) > DrawerHeight.max_rows
        }
      );
    /* In drawer mode an active rich renderer replaces the sample view in
     * the drawer itself; the anchored modal overlay is inline-mode only
     * (anchored to the nav-bar stub, it renders detached/clipped). */
    let rich_drawer =
      drawer
      && (
        switch (rich_drawer_rows(model, info)) {
        | Some(n) => n > inline_rows_cap
        | None => false
        }
      )
        ? rich_content(
            ~settings,
            model,
            info,
            ~local,
            ~parent,
            ~view_seg,
            ~sort,
          )
          |> Option.map(content =>
               rich_drawer_view(
                 ~local,
                 ~overflowing=drawer_overflow,
                 /* auto-rich (no explicit renderer) has nothing to close:
                    dismissal would just re-trigger */
                 ~closable=model.active_renderer != None,
                 content,
               )
             )
        : None;
    /* the anchored modal is retired: small rich views replace the
       offside row, big ones live in the drawer */
    let modal_nodes = [];
    let _unused_modal = modal_overlay;
    /* Wrap in a div only when the modal is open, to avoid an extra DOM level
     * around the positioned .live-offside otherwise. */
    let offside_node =
      switch (modal_nodes) {
      | [] => offside_main
      | _ => div([offside_main] @ modal_nodes)
      };
    View.{
      inline: Node.div([]),
      overlay: None,
      offside: Some(offside_node),
      below:
        switch (data_opt, drawer) {
        | (Some(data), true) =>
          Some(
            live_offside_view(
              ~display=Block,
              ~include_nav_bar=false,
              ~drawer_mode_active=true,
              ~rich_content=rich_drawer,
              ~scrollable=drawer_overflow,
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
