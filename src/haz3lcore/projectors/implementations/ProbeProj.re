open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

open Js_of_ocaml;
open Language;

/* Global probe display state. See ZipperBase.re for full probe state documentation.
 * - Settings.s: Global display settings (window mode, cutoffs)
 * - Settings.offset: Per-probe window scroll offsets
 * - SampleLength.lengths: Per-sample display lengths
 * These use mutable refs for simplicity since they're UI-only state. */

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleWindowMode
  | ResetSettings;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
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
    | ToggleSampleBase
    | ToggleBeforeCutoff
    | ToggleAfterCutoff
    | ToggleCallerCutoff
    | ToggleCalleeCutoff;

  let init: settings = {
    window: Single,
    sample_base: Calls,
    before_cutoff: None,
    after_cutoff: None,
    caller_cutoff: None,
    callee_cutoff: None,
  };

  let update = (settings: settings, action: set_action): settings =>
    switch (action) {
    | ToggleWindow => {
        ...settings,
        window: settings.window == Sample.Window.Single ? Many : Single,
      }
    | ToggleSampleBase => {
        ...settings,
        sample_base:
          switch (settings.sample_base) {
          | Calls => StepRange
          | StepRange => Calls
          },
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

  let reset_mode = () => {
    Hashtbl.clear(offset);
    s := init;
  };

  let go = (a: set_action): unit => s := update(s^, a);
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
};

/* Stateful window offset management (GUI-specific) */
module WindowState = {
  let get_offset = (k: Id.t): int =>
    switch (Hashtbl.find_opt(offset, k)) {
    | Some(v) => v
    | None => 0
    };

  let set_offset = (k: Id.t, v: int) => Hashtbl.add(offset, k, v);

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

  let set = (id: int, length: int): unit => Hashtbl.add(lengths, id, length);
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
        ~pinned=dynamics.sample_cursor.pinned_stack,
        dynamics.samples,
      )
    };
  let first_idx =
    Sample.Selection.first_related_index(
      ~trimmed=true,
      ~ap_id,
      dynamics.sample_cursor,
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

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> Unicode.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
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

let cursor_clss =
    (~settings, ~ap_id, dynamics: Dynamics.Info.t, sample: Sample.t)
    : list(string) => {
  switch (settings.sample_base) {
  | Calls =>
    let relation =
      Sample.Cursor.relation(
        ~trimmed=true,
        ~ap_id,
        dynamics.sample_cursor,
        sample,
      );
    let cursor_class =
      switch (
        relation.is_call_cursor,
        relation.is_call_above_call_cursor,
        relation.is_below_indicated_call,
      ) {
      | (true, _, _) => ["cursor"]
      | (_, Some(0), _) => ["cursor-caller", "direct"]
      | (_, Some(_), _) when settings.caller_cutoff == None => [
          "cursor-caller",
          "indirect",
        ]
      | (_, _, Some(0)) => ["cursor-callee", "direct"]
      | (_, _, Some(_)) when settings.callee_cutoff == None => [
          "cursor-callee",
          "indirect",
        ]
      | (_, _, _) => ["cursor-unrelated"]
      };
    let level_class =
      switch (relation.relative_level_to_cursor) {
      | Same => ["level0"]
      | Below(n)
          when
            settings.before_cutoff == None
            || Some(n) <= settings.before_cutoff => [
          "below",
          "L" ++ string_of_int(n),
        ]
      | Above(n)
          when
            settings.after_cutoff == None || Some(n) <= settings.after_cutoff => [
          "above",
          "L" ++ string_of_int(n),
        ]
      | _ => []
      };
    cursor_class @ level_class;
  | StepRange =>
    switch (
      Sample.Cursor.step_containment(
        ~focus_range=dynamics.sample_cursor.step_range,
        sample,
      )
    ) {
    | StepEqual => ["cursor", "level0"]
    | StepContainedWithin => ["cursor-caller", "direct", "above", "L1"]
    | StepContains => ["cursor-callee", "direct", "below", "L1"]
    | StepDisjointBefore => ["cursor-unrelated", "above", "L1"]
    | StepDisjointAfter => ["cursor-unrelated", "below", "L1"]
    | StepNoFocus => ["cursor-unrelated"]
    }
  };
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
      switch (Sample.Cursor.cur_call(ap_id, sample)) {
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
    ctx.parent(
      SampleCursor(
        TogglePin([
          {
            id: ap_id,
            name: None,
            fn_def_id: None,
          },
          ...sample.call_stack,
        ]),
      ),
    )
  | _ => Effect.Ignore
  };

let focus_call = (ctx: probe_ctx) =>
  switch (Dynamics.Info.is_in(ctx.dynamics)) {
  | Some(sample) when sample.call_stack != [] =>
    ctx.parent(SampleCursor(TogglePin(sample.call_stack)))
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
      SampleCursor(Capture(Sample.capture_of_sample(sample), ap_id)),
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
      let target_width = max(0, goal.col);
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
      // Attr.title(Debug.str(~ap_id, sample)),
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
  switch (ctx.ap_id, ctx.dynamics.sample_cursor.pinned_stack) {
  | (Some(ap_id), Some(pinned_stack)) =>
    /* Compare by ID only - function names may differ */
    Sample.ids_of_stack(pinned_stack)
    == [ap_id, ...Sample.ids_of_stack(sample.call_stack)]
  | _ => false
  };
};

let show_focus = (ctx: probe_ctx, sample: Sample.t) =>
  switch (ctx.ap_id, ctx.dynamics.sample_cursor.pinned_stack) {
  | (None, Some(pinned_stack)) =>
    Sample.ids_of_stack(pinned_stack)
    == Sample.ids_of_stack(sample.call_stack)
  | _ => false
  };

let pin_view = (ctx: probe_ctx, sample: Sample.t) =>
  show_pin(ctx, sample) || show_focus(ctx, sample)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

/* Generate unique dropdown ID for a sample */
let dropdown_id = (sample_id: int): string =>
  "sample-dropdown-" ++ string_of_int(sample_id);

/* Step into handler for sample context menu */
let step_into_sample =
    (~parent, ~sample: Sample.t, ~ap_id: Id.t): Ui_effect.t(unit) =>
  parent(Probe(StepInto(sample.call_stack, ap_id)));

/* Check if step-into is possible for this probe's function call.
 * Requires: Ap of a named variable that isn't a built-in. */
let can_step_into = (statics: Language.Statics.Info.t): bool =>
  switch (statics) {
  | InfoExp({term: {term: Ap(_, fn_exp, _), _}, _}) =>
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

/* Context actions for a sample (Pin/Unpin, Step Into, etc.) */
let sample_context_actions =
    (ctx: probe_ctx, ~can_step_into: bool, sample: Sample.t): list(Node.t) =>
  switch (ctx.ap_id) {
  | Some(ap_id) => [
      div(
        ~attrs=[Attr.classes(["context-actions"])],
        [pin_action(ctx, sample)]
        @ (can_step_into ? [step_into_action(ctx, sample, ap_id)] : []),
      ),
    ]
  | None when sample.call_stack != [] => [
      div(
        ~attrs=[Attr.classes(["context-actions"])],
        [focus_action(ctx, sample)],
      ),
    ]
  | None => []
  };

/* Get function name from statics info if this is an Ap expression */
let get_fn_name_from_statics =
    (statics: Language.Statics.Info.t): option(string) =>
  switch (statics) {
  | InfoExp({term: {term: Ap(_, fn_exp, _), _}, _}) =>
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
  | InfoExp({term: {term: Ap(_, _, arg), _}, _}) =>
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
let sample_context_menu = (ctx: probe_ctx, view_seg, sample: Sample.t): Node.t => {
  /* Get variable names shown in call display to filter from environment */
  let filter_vars = List.filter_map(Fun.id, get_arg_var_info(ctx.statics));
  let env_elems = filtered_env_entries(~filter_vars, sample);
  let has_env = env_elems != [];
  let has_call = Option.is_some(sample.args);
  div(
    ~attrs=
      [
        Attr.classes(
          ["sample-context-menu"] @ (has_env || has_call ? [] : ["no-env"]),
        ),
      ]
      @ SafeTriangle.CSSDropdown.menu_attrs(dropdown_id(sample.id)),
    sample_context_actions(
      ctx,
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
  | InfoExp({term: {term: Var(_), _}, _}) => true
  | InfoPat(_) => true
  | _ => false
  };

let sample_view =
    (ctx: probe_ctx, ~num_total, view_seg, local, sample: Sample.t) => {
  let hide_env = hide_env(ctx.statics);
  let has_dropdown =
    !(hide_env && ctx.ap_id == None) || sample.call_stack != [];
  div(
    ~attrs=
      [Attr.classes(["sample"])]
      @ (
        has_dropdown
          ? SafeTriangle.CSSDropdown.trigger_attrs(dropdown_id(sample.id))
          : []
      ),
    [value_view(ctx, ~num_total, view_seg, local, sample)]
    @ pin_view(ctx, sample)
    @ (has_dropdown ? [sample_context_menu(ctx, view_seg, sample)] : []),
  );
};

/* Select a default sample by preferring the closest match to the current
 * dynamic cursor. */
let mv_least_distant_sample = (ctx: probe_ctx, _evt): Effect.t(unit) => {
  let {ap_id, dynamics, parent, _} = ctx;
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=dynamics.sample_cursor.pinned_stack,
      dynamics.samples,
    );
  switch (
    Sample.Selection.closest_to_cursor(
      ~ap_id,
      ~cursor=dynamics.sample_cursor,
      samples,
    )
  ) {
  | Some(selected) =>
    parent(
      SampleCursor(Capture(Sample.capture_of_sample(selected), ap_id)),
    )
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
        Attr.on_pointerdown(_ => ctx.parent(SampleCursor(Reset))),
      ],
      [text("⍟")] //📌◌🔒
    )
  | NotAligned =>
    /* Reuse existing ellipsis behavior for not-aligned case */
    div(
      ~attrs=[
        Attr.classes(["empty-status", "not-aligned"]),
        Attr.title("Samples not aligned with cursor — click to align"),
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
      ~pinned=dynamics.sample_cursor.pinned_stack,
      dynamics.samples,
    );
  let cursor_idx =
    Sample.Selection.first_related_index(
      ~trimmed=true,
      ~ap_id,
      dynamics.sample_cursor,
      samples,
    );
  switch (cursor_idx) {
  /* Cursor would be outside window, reset to next visible sample */
  | Some(idx) =>
    let next_idx_maybe = idx - offset;
    if (next_idx_maybe >= 0 && next_idx_maybe < List.length(samples)) {
      let sample = List.nth(samples, next_idx_maybe);
      parent(
        SampleCursor(Capture(Sample.capture_of_sample(sample), ap_id)),
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
      ~pinned=dynamics.sample_cursor.pinned_stack,
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
  let goal = cur - 1;
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
  Dynamics.Info.first_cursor_sample(ctx.ap_id, ctx.dynamics);

let key_handler = (ctx: probe_ctx, ~id: Id.t, local, evt) => {
  let {ap_id, parent, _} = ctx;
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("E" | "e") when key.meta == Down || key.ctrl == Down => parent(Remove)
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([local(ResetSettings), parent(SampleCursor(Reset))]);
  | D("Escape") =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Ignore;
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
  | D(" ") =>
    Many([local(ToggleWindowMode), Stop_propagation, Prevent_default])
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
  | _ => Many([Stop_propagation])
  };
};

let empty_view = (~id: Id.t, ~settings: settings) =>
  Node.div(
    ~attrs=[
      Attr.id(Id.cls(id)),
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
    (info: info, local, parent, ~settings: settings, view_seg: View.seg) =>
  switch (info.dynamics, info.statics) {
  | (Some(dynamics), Some(statics)) =>
    let id = info.id;
    let ap_id = Sample.Cursor.cur_var_ap(statics);
    let ctx = {
      ap_id,
      statics,
      settings,
      dynamics,
      utility: info.utility,
      parent,
    };
    /* Filter samples once and reuse for both num_total and selection */
    let filtered_samples =
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=dynamics.sample_cursor.pinned_stack,
        dynamics.samples,
      );
    let num_total = List.length(filtered_samples);
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
      switch (dynamics.sample_cursor.pending_focus) {
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
        let sample_view = sample_view(ctx, ~num_total, view_seg_line, local);
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

let overlay_view = (info: info): Node.t =>
  switch (info.dynamics, info.statics) {
  | (Some(dynamics), Some(statics)) =>
    let ap_id = Sample.Cursor.cur_var_ap(statics);
    div(
      ~attrs=[
        Attr.classes(["overlay"] @ (Option.is_some(ap_id) ? ["ap"] : [])),
      ],
      [num_samples_view(~ap_id, dynamics)],
    );
  | _ => Node.div([])
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  let model_of_sexp = _ => ();
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let init = (any: Any.t) =>
    switch (any) {
    | Exp(_)
    | Pat(_) => Some()
    | Any(_) => Some() /* Grout don't have sorts */
    | _ => None
    };

  let dynamics = true;

  let focusable =
    Focusable.{
      pointer: Some(id => JsUtil.get_elem_by_id(Id.cls(id))##focus),
      keyboard: None,
    };

  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (_, _, a: action) => {
    switch (a) {
    | ChangeLength(id, len) => SampleLength.set(id, len)
    | ToggleWindowMode => Settings.go(ToggleWindow)
    | ResetSettings =>
      Settings.reset_mode();
      SampleLength.reset();
    };
  };

  let view = ({info, local, parent, view_seg, _}: View.args(model, action)) => {
    let settings = Settings.s^;
    View.{
      inline: Node.div([]),
      overlay: Some(overlay_view(info)),
      offside: Some(offside_view(~settings, info, local, parent, view_seg)),
    };
  };
};
