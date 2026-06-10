open Util;
open Virtual_dom.Vdom;
open Js_of_ocaml;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;
open Language;
open ProbeProj;
open Settings;
open Node;

/* Probe projector web view: sample value rendering, the sample context
 * menu, keyboard handling, and the rich-probe modal. The probe logic
 * (sample selection, display settings, model/action state machine) lives
 * in haz3lcore's ProbeProj, whose helpers are reused throughout. */

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
        KeyEvent.meta_held(evt)
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

/* Step into handler for sample context menu */
let step_into_sample =
    (~parent, ~sample: Sample.t, ~ap_id: Id.t): Ui_effect.t(unit) =>
  parent(Probe(StepInto(sample.call_stack, ap_id)));

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
    (ctx: probe_ctx, local, sample: Sample.t, r: RichProbeView.packed_view)
    : Node.t => {
  let is_active = ctx.active_renderer_id == Some(r.core.id);
  let label = (is_active ? "Hide " : "View as ") ++ r.core.id;
  div(
    ~attrs=[
      Attr.classes(["action-item", "rich-probe-action"]),
      Attr.on_pointerdown(_ =>
        local(ToggleModal(r.core.init_model(ctx.sort, sample.value)))
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
    RichProbeViewRegistry.views
    |> List.filter_map((r: RichProbeView.packed_view) =>
         r.core.can_handle(ctx.sort, indicated.value)
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
      List.exists(
        (r: RichProbeView.packed_view) =>
          r.core.can_handle(ctx.sort, indicated.value),
        RichProbeViewRegistry.views,
      )
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
  let key = KeyEvent.mk(KeyDown, evt);
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
  | D("c" | "C") when KeyEvent.meta_held(evt) || KeyEvent.ctrl_held(evt) =>
    switch (indicated_sample(ctx)) {
    | Some(sample) =>
      let seg = ctx.utility.term_to_seg(~inline=true, Exp(sample.value));
      let str = ctx.utility.seg_to_string(seg);
      let _ =
        Js.Unsafe.global##.navigator##.clipboard##writeText(Js.string(str));
      Many([Stop_propagation, Prevent_default]);
    | None => Many([Stop_propagation, Prevent_default])
    }
  | D("z" | "Z") when KeyEvent.ctrl_held(evt) || KeyEvent.meta_held(evt) =>
    Ignore // Defer to parent editor undo for now
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

/* Modal overlay for dynamic renderer display */
let modal_overlay =
    (
      ~settings,
      model: probe_model,
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
    /* Find the renderer view and check if it can still handle the expression */
    switch (RichProbeViewRegistry.find_view(rid)) {
    | Some(renderer) when renderer.core.can_handle(sort, exp) =>
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

module V: ProjectorView = {
  module L = ProbeProj.M;

  let focusable =
    Focusable.{
      pointer: Some(id => {JsUtil.get_elem_by_id(Id.cls(id))##focus}),
      keyboard: None,
    };

  let view =
      (
        {info, local, parent, view_seg, model, status, _}:
          View.args(L.model, L.action),
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
