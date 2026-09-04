open Util_web;
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
type probe_model = {active_renderer: option(packed_model)};

/* Any deserialization failure resets to closed-modal — the record is
 * pure transient UI state. Known failure modes are logged for
 * debuggability; unknown ones still degrade gracefully. */
let probe_model_of_sexp = sexp =>
  switch (probe_model_of_sexp(sexp)) {
  | model => model
  | exception (RichProbeRegistry.Unknown_renderer(rid)) =>
    print_endline("probe_model_of_sexp: unknown renderer " ++ rid);
    {active_renderer: None};
  | exception (Failure(msg)) =>
    print_endline("probe_model_of_sexp: malformed payload: " ++ msg);
    {active_renderer: None};
  | exception _ => {active_renderer: None}
  };

/* `^^probe_<rid>` trigger-option mapping: a pin whose model selects
   renderer <rid> (in its empty state) round-trips through text. */
let model_string_for_renderer = (rid: string): option(string) =>
  RichProbeRegistry.find(rid)
  |> Option.map((r: packed_renderer) =>
       sexp_of_probe_model({active_renderer: Some(r.empty_model)})
       |> Sexplib.Sexp.to_string
     );

let renderer_of_model_string = (model: string): option(string) =>
  switch (probe_model_of_sexp(Sexplib.Sexp.of_string(model))) {
  | {active_renderer: Some(PModel(rid, _, _))} => Some(rid)
  | {active_renderer: None} => None
  | exception _ => None
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleModal(option(packed_model))
  | RendererAction(packed_action)
  | ToggleWindowMode
  | ToggleShowEnv
  | ResetSettings;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
    | Hybrid
    | StepRange;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type settings = {
    window: Sample.Window.mode,
    sample_base,
    before_cutoff: option(int),
    after_cutoff: option(int),
    caller_cutoff: option(int),
    callee_cutoff: option(int),
  };

  type set_action =
    | ToggleWindow
    | SetSampleBase(sample_base)
    | ToggleBeforeCutoff
    | ToggleAfterCutoff
    | ToggleCallerCutoff
    | ToggleCalleeCutoff;

  let init: settings = {
    window: Single,
    sample_base: Hybrid,
    before_cutoff: None,
    after_cutoff: None,
    caller_cutoff: None,
    callee_cutoff: None,
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

  /* When true, the context menu dropdown is shown for the indicated sample
   * without hovering. Toggled by '/' key. Persists across probe navigation. */
  let show_env = ref(false);

  let reset_mode = () => {
    Hashtbl.clear(offset);
    s := init;
    version := version^ + 1;
    show_env := false;
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

/* Find first compatible renderer for an expression */
let find_compatible_renderer =
    (sort: Sort.t, exp: Exp.t): option(RichProbe.packed_renderer) =>
  List.find_opt(r => r.can_handle(sort, exp), renderers);

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
    (ctx: probe_ctx, ~num_total, view_seg, local, sample: Sample.t) => {
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
      let width_at = (b: int): int =>
        abbreviated_seg_of(utility, b, sample.value) |> snd;
      let budget = find_best_budget(width_at, target_width);
      local(ChangeLength(sample.id, budget));
    | _ => Effect.Ignore
    };
  };

  let length =
    if (!SampleLength.is_explicit(sample) && num_total == 1) {
      150;
    } else {
      SampleLength.get(settings.window, sample);
    };
  let (seg, length) = abbreviated_seg_of(utility, length, sample.value);

  div(
    ~attrs=[
      Attr.classes(
        ["value", length_cls(length)]
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
        Key.meta_held(evt)
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
      div(~attrs=[Attr.classes(["pin-icon"])], []),
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
      div(~attrs=[Attr.classes(["pin-icon"])], []),
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
      div(~attrs=[Attr.classes(["step-into-icon"])], []),
      text("Step into"),
      span(~attrs=[Attr.classes(["shortcut"])], [text("Enter")]),
    ],
  );

/* Rich probe action: open a domain-specific visualization via ToggleModal.
   One menu item per compatible renderer; r.badge supplies the icon.
   Label flips to "Hide <id>" when this renderer's modal is already open,
   since dispatching ToggleModal again closes it. */
let rich_probe_action =
    (ctx: probe_ctx, local, sample: Sample.t, r: packed_renderer): Node.t => {
  let is_active = ctx.active_renderer_id == Some(r.id);
  let label = (is_active ? "Hide " : "View as ") ++ r.id;
  div(
    ~attrs=[
      Attr.classes(["action-item", "rich-probe-action"]),
      Attr.on_pointerdown(_ =>
        local(ToggleModal(r.init_model(ctx.sort, sample.value)))
      ),
    ],
    [r.badge, text(label)],
  );
};

let rich_probe_items =
    (ctx: probe_ctx, local, _sample: Sample.t): list(Node.t) =>
  switch (Dynamics.Info.most_aligned_sample(ctx.ap_id, ctx.dynamics)) {
  | None => []
  | Some(indicated) =>
    renderers
    |> List.filter_map(r =>
         r.can_handle(ctx.sort, indicated.value)
           ? Some(rich_probe_action(ctx, local, indicated, r)) : None
       )
  };

/* Context actions for a sample (Pin/Unpin, Step Into, rich-probe views, etc.) */
let sample_context_actions =
    (ctx: probe_ctx, local, ~can_step_into: bool, sample: Sample.t)
    : list(Node.t) => {
  let rich_items = rich_probe_items(ctx, local, sample);
  switch (ctx.ap_id) {
  | Some(ap_id) => [
      div(
        ~attrs=[Attr.classes(["context-actions"])],
        [pin_action(ctx, sample)]
        @ (can_step_into ? [step_into_action(ctx, sample, ap_id)] : [])
        @ rich_items,
      ),
    ]
  | None when sample.call_stack != [] => [
      div(
        ~attrs=[Attr.classes(["context-actions"])],
        [focus_action(ctx, sample)] @ rich_items,
      ),
    ]
  | None when rich_items != [] => [
      div(~attrs=[Attr.classes(["context-actions"])], rich_items),
    ]
  | None => []
  };
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

/* Sample context menu (dropdown) combining actions and environment */
let sample_context_menu =
    (~show_env, ctx: probe_ctx, local, view_seg, sample: Sample.t): Node.t => {
  /* Get variable names shown in call display to filter from environment */
  let filter_vars = List.filter_map(Fun.id, get_arg_var_info(ctx.statics));
  let env_elems = filtered_env_entries(~filter_vars, sample);
  let has_env = env_elems != [];
  let has_call = Option.is_some(sample.args);
  div(
    ~attrs=
      [
        Attr.classes(
          ["sample-context-menu"]
          @ (has_env || has_call ? [] : ["no-env"])
          @ (show_env ? ["dropdown-active"] : []),
        ),
      ]
      @ SafeTriangle.CSSDropdown.menu_attrs(dropdown_id(sample)),
    sample_context_actions(
      ctx,
      local,
      ~can_step_into=can_step_into(ctx.statics),
      sample,
    )
    @ sample_call_display(ctx, view_seg, sample)
    @ sample_environment(ctx, ~filter_vars, view_seg, sample),
  );
};

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (statics: Language.Statics.Info.t): bool =>
  switch (statics) {
  | InfoExp({user_term: {term: Var(_), _}, _}) => true
  | InfoPat(_) => true
  | _ => false
  };

let sample_view =
    (
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
  let show_env = Settings.show_env^ && indicated_sample_id == Some(sample.id);
  div(
    ~attrs=
      [Attr.classes(["sample"])]
      @ (
        has_dropdown
          ? SafeTriangle.CSSDropdown.trigger_attrs(dropdown_id(sample)) : []
      ),
    [value_view(ctx, ~num_total, view_seg, local, sample)]
    @ pin_view(ctx, sample)
    @ (
      has_dropdown
        ? [sample_context_menu(~show_env, ctx, local, view_seg, sample)] : []
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

let nav_bar_view = (ctx: probe_ctx, ~num_total) => {
  let nav_arrow = (cond: bool, offset: int): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
        Attr.on_click(_ => move_cursor(ctx, offset)),
      ],
      [],
    );
  let show_left = num_total < Sample.Window.max_samples(ctx.settings.window);
  let show_right = num_total < Sample.Window.max_samples(ctx.settings.window);
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
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

let key_handler = (ctx: probe_ctx, ~id: Id.t, local, evt) => {
  let {ap_id, parent, _} = ctx;
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("E" | "e") when key.meta == Down || key.ctrl == Down => parent(Remove)
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([local(ResetSettings), parent(SampleFocus(Reset))]);
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
    // Prevent_default below stops aggressive horizontal scroll
    Many([move_cursor(ctx, -1), Stop_propagation, Prevent_default])
  | D("ArrowLeft") =>
    Many([move_cursor(ctx, 1), Stop_propagation, Prevent_default])
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
  | D("/") => Many([local(ToggleShowEnv), Stop_propagation, Prevent_default])
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

let offside_view =
    (
      info: info,
      local,
      parent,
      ~settings: settings,
      ~sort: Sort.t,
      ~model: probe_model,
      view_seg: View.seg,
    ) =>
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
      sort,
      active_renderer_id,
    };
    /* Filter samples once and reuse for both num_total and selection */
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

    /* Check if this probe is the target of a pending step-into focus */
    let is_evaluating =
      switch (dynamics.sample_focus.pending_focus) {
      | Some({probe_id, _}) => probe_id == id
      | None => false
      };

    /* Determine what to show when no samples are displayed */
    let empty_status =
      Sample.Selection.get_empty_status(
        ~num_total,
        ~num_shown,
        ~is_evaluating,
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
        Attr.on_keydown(key_handler(ctx, ~id, local)),
        Attr.classes([
          "live-offside",
          settings.window |> Sample.Window.show_mode,
        ]),
      ],
      switch (empty_status) {
      | Some(status) => [empty_status_view(ctx, ~status, local)]
      | None =>
        /* Overflow indicator: shown when samples ARE displayed but more exist */
        let overflow_view =
          num_shown > 0 && num_shown < num_total
            ? [nav_bar_view(ctx, ~num_total), ellipsis_view(local)] : [];
        let view_seg_line = (~text_only, segment) =>
          view_seg(
            ~single_line=true,
            ~background=false,
            ~text_only,
            Sort.Exp,
            segment,
          );
        let indicated_sample_id =
          indicated_sample(ctx) |> Option.map((s: Sample.t) => s.id);
        let sample_view =
          sample_view(
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
        (
          group_views == []
            ? []
            : [div(~attrs=[Attr.classes(["sample-groups"])], group_views)]
        )
        @ overflow_view;
      },
    );
  | _ => empty_view(~id=info.id, ~settings)
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let init = (any: Any.t) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some({active_renderer: None})
    | Any(_) => Some({active_renderer: None}) /* Grout don't have sorts */
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

  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (model: probe_model, _info: info, action: action): probe_model => {
    switch (action) {
    | ChangeLength(id, len) =>
      SampleLength.set(id, len);
      Settings.version := Settings.version^ + 1;
      model;
    | ToggleWindowMode =>
      Settings.go(ToggleWindow);
      model;
    | ToggleShowEnv =>
      Settings.show_env := ! Settings.show_env^;
      Settings.version := Settings.version^ + 1;
      model;
    | ResetSettings =>
      Settings.reset_mode();
      SampleLength.reset();
      model;
    | ToggleModal(pm) =>
      switch (model.active_renderer) {
      | None => {active_renderer: pm}
      | Some(_) => {active_renderer: None}
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
    View.{
      inline: Node.div([]),
      overlay: Some(overlay_view(~settings, ~sort, info)),
      offside:
        Some(
          div(
            [
              offside_view(
                info,
                local,
                parent,
                ~settings,
                ~sort,
                ~model,
                view_seg,
              ),
            ]
            @ modal_overlay(
                ~settings,
                model,
                info,
                ~local,
                ~parent,
                ~view_seg,
                ~sort,
              ),
          ),
        ),
      error: false,
    };
  };
};
