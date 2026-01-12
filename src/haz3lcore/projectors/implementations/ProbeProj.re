open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

open Js_of_ocaml;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleShowAllVals(int)
  | NoOp;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
    | Steps
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
          | Calls => Steps
          | Steps => StepRange
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
      di: Dynamics.Info.t,
    )
    : list(Sample.t) => {
  let samples =
    switch (filtered) {
    | Some(s) => s
    | None =>
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=di.dyn_cursor.pinned_stack,
        di.samples,
      )
    };
  let first_idx =
    Sample.Selection.first_related_index(
      ~trimmed=true,
      ~ap_id,
      di.dyn_cursor,
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

let abbreviate = (exp: Exp.t, available: int): Exp.t => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  abbr_exp;
};

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

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
  let row_height = 10.0;
  let col_width = 10.0;
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
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };

module ValueState = {
  let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

  let click_coords: ref(option(Point.t)) = ref(Option.None);
};

let cursor_clss =
    (
      ~settings: settings,
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      sample: Sample.t,
    )
    : list(string) => {
  switch (settings.sample_base) {
  | Calls =>
    let relation =
      DynCursor.relation(~trimmed=true, ~ap_id, di.dyn_cursor, sample);
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

  | Steps =>
    let relation =
      DynCursor.relation(~trimmed=true, ~ap_id, di.dyn_cursor, sample);
    let cursor_class =
      switch (
        relation.is_call_cursor,
        relation.is_call_above_call_cursor,
        relation.is_below_indicated_call,
      ) {
      | (true, _, _) when sample.iter == di.dyn_cursor.iter => ["cursor"]
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
      switch (relation.is_before_cursor) {
      | n when n == 0 => ["level0"]
      | n when n > 0 =>
        settings.before_cutoff == None || Some(n) <= settings.before_cutoff
          ? ["below", "L" ++ string_of_int(n)] : []
      | n when n < 0 =>
        settings.after_cutoff == None || Some(- n) <= settings.after_cutoff
          ? ["above", "L" ++ string_of_int(- n)] : []
      | _ => []
      };
    cursor_class @ level_class;

  | StepRange =>
    /* StepRange mode: color samples based on step-range containment
       relative to the focused (cursor) sample. Returns complete class
       list matching the legend categories:
       - At Cursor (StepEqual): cursor + level0
       - Inside (StepContainedWithin): cursor-callee + below
       - Contains (StepContains): cursor-caller + above
       - Before (StepDisjointBefore): cursor-unrelated + above
       - After (StepDisjointAfter): cursor-unrelated + below
       - Off Cursor (StepNoFocus): cursor-unrelated only */
    switch (
      DynCursor.step_containment(
        ~focus_range=di.dyn_cursor.step_range,
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
  let stack = (stack: Probe.call_stack): string =>
    stack |> List.map(Id.str3) |> String.concat("\n");

  let str = (~ap_id: option(Id.t), sample: Sample.t): string =>
    "sample id: "
    ++ string_of_int(sample.id)
    ++ "\n"
    ++ "ap:"
    ++ (
      switch (DynCursor.cur_call(ap_id, sample)) {
      | Some([ap_id, ..._]) => Id.str3(ap_id)
      | _ => "None"
      }
    )
    // ++ "\nvalue:\n"
    // ++ DHExp.show(sample.value)
    ++ "\nstack:\n"
    ++ stack(sample.call_stack)
    ++ "\nstep-range:\n"
    ++ Printf.sprintf("[%d, %d]", sample.step_start, sample.step_end)
    ++ "\ntime: "
    ++ Printf.sprintf("%.0f", sample.time);
};

let pin_call = (~parent, ~ap_id: option(Id.t), ~di: Dynamics.Info.t) =>
  switch (ap_id, Dynamics.Info.is_in(di)) {
  | (Some(ap_id), Some(dyn_cursor)) =>
    print_endline("actually pinning call");
    parent(DynCursor(TogglePinCall([ap_id, ...dyn_cursor.call_stack])));
  | _ =>
    print_endline("ignoring");
    Effect.Ignore;
  };

let value_view =
    (
      ~ap_id: option(Id.t),
      ~settings: settings,
      ~num_total: int,
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent: external_action => Ui_effect.t(unit),
      sample: Sample.t,
      _index: int,
    ) => {
  let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
    if (Js.to_bool(e##.shiftKey)) {
      let target =
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      JsUtil.setPointerCapture(target, e##.pointerId);
      ValueState.mousedown := Some(target);
      ValueState.click_coords :=
        Some({
          row: e##.clientY,
          col: e##.clientX,
        });
    };
    parent(DynCursor(Capture(sample, ap_id)));
  };

  let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
    let target =
      e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
    if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
      JsUtil.releasePointerCapture(target, e##.pointerId);
    };
    ValueState.mousedown := None;
    ValueState.click_coords := None;
    Effect.Ignore;
  };

  let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) => {
    switch (ValueState.mousedown^) {
    | Some(_) when Js.to_bool(e##.shiftKey) =>
      let goal = pos_rel_to_target(e);
      local(ChangeLength(sample.id, goal.col));
    | _ => Effect.Ignore
    };
  };

  /* Crude way of giving more space when there's only one sample shown.
   * Really should figure out total length of all samples and divide accordingly */
  let length = SampleLength.get(settings.window, sample);
  /* Use pre-computed num_total instead of filtering again */
  let length = length == 12 && num_total == 1 ? 150 : length;
  let (seg, length) = abbreviated_seg_of(utility, length, sample.value);

  div(
    ~attrs=[
      Attr.title(Debug.str(~ap_id, sample)),
      Attr.classes(
        ["value", length_cls(length)]
        @ cursor_clss(~settings, ~ap_id, di, sample)
        @ (Option.is_some(ap_id) ? ["ap"] : [])
        @ (!ValueChecker.is_value(sample.value) ? ["indet"] : []),
      ),
      //Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      Attr.on_pointerdown(evt =>
        Key.meta_held(evt)
          ? pin_call(~parent, ~ap_id, ~di) : val_pointerdown(evt)
      ),
      Attr.on_pointerup(val_pointerup),
      Attr.on_mousemove(val_mousemove),
    ],
    [view_seg(~text_only=false, Sort.Exp, seg)],
  );
};

let env_val =
    (
      ~settings: settings,
      sample,
      view_seg,
      utility: utility,
      en: Sample.Env.entry,
    )
    : Node.t => {
  Node.div(
    ~attrs=[Attr.classes(["live-env-entry"])],
    [
      Node.text(en.binding.name ++ " ≡ "),
      switch (en.value) {
      | Opaque => Node.text("Opaque")
      | Val(d) =>
        let (seg, _) =
          abbreviated_seg_of(
            utility,
            SampleLength.get(settings.window, sample),
            d,
          );
        view_seg(~text_only=false, Sort.Exp, seg);
      },
    ],
  );
};

let show_pin = (~ap_id: option(Id.t), di: Dynamics.Info.t, sample: Sample.t) => {
  switch (ap_id, di.dyn_cursor.pinned_stack) {
  | (Some(ap_id), Some(pinned_stack)) =>
    pinned_stack == [ap_id, ...sample.call_stack]
  | _ => false
  };
};

let pin_view = (~ap_id: option(Id.t), di: Dynamics.Info.t, sample: Sample.t) =>
  show_pin(~ap_id, di, sample)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

/* Generate unique dropdown ID for a sample */
let dropdown_id = (sample_id: int): string =>
  "sample-dropdown-" ++ string_of_int(sample_id);

/* Step into handler for sample context menu */
let step_into_sample =
    (~parent, ~sample: Sample.t, ~ap_id: Id.t): Ui_effect.t(unit) =>
  parent(Refractor(StepIntoSample(sample, ap_id)));

/* Context actions for a sample (Pin/Unpin, Step Into, etc.) */
let sample_context_actions =
    (~parent, ~ap_id: option(Id.t), ~di: Dynamics.Info.t, sample: Sample.t)
    : list(Node.t) =>
  switch (ap_id) {
  | Some(ap_id) =>
    let is_pinned = show_pin(~ap_id=Some(ap_id), di, sample);
    [
      div(
        ~attrs=[Attr.classes(["context-actions"])],
        [
          /* Pin/Unpin action */
          div(
            ~attrs=[
              Attr.classes(
                ["action-item", "pin-action"] @ (is_pinned ? ["pinned"] : []),
              ),
              Attr.on_pointerdown(_ =>
                pin_call(~parent, ~ap_id=Some(ap_id), ~di)
              ),
            ],
            [
              div(~attrs=[Attr.classes(["pin-icon"])], []),
              text(is_pinned ? "Unpin" : "Pin"),
              span(~attrs=[Attr.classes(["shortcut"])], [text("P")]),
            ],
          ),
          /* Step Into action */
          div(
            ~attrs=[
              Attr.classes(["action-item", "step-into-action"]),
              Attr.on_pointerdown(_
                /* Stop propagation to prevent parent wrapper's Focus action
                   from moving cursor back to the probe after we jump */
                =>
                  Effect.Many([
                    Effect.Stop_propagation,
                    step_into_sample(~parent, ~sample, ~ap_id),
                  ])
                ),
            ],
            [
              div(~attrs=[Attr.classes(["step-into-icon"])], []),
              text("Step into"),
              span(~attrs=[Attr.classes(["shortcut"])], [text("Enter")]),
            ],
          ),
        ],
      ),
    ];
  | None => []
  };

/* Environment section showing variable bindings */
let sample_environment =
    (~settings: settings, sample: Sample.t, view_seg, utility: utility)
    : list(Node.t) => {
  let elems = sample.env |> ListUtil.dedup |> Sample.Env.remove_opaques;
  elems == []
    ? []
    : [
      div(
        ~attrs=[Attr.classes(["environment-section"])],
        [
          div(
            ~attrs=[Attr.classes(["live-env"])],
            List.map(env_val(~settings, sample, view_seg, utility), elems),
          ),
        ],
      ),
    ];
};

/* Sample context menu (dropdown) combining actions and environment */
let sample_context_menu =
    (
      ~settings: settings,
      ~parent,
      ~ap_id,
      ~di,
      sample: Sample.t,
      view_seg,
      utility: utility,
    )
    : Node.t =>
  div(
    ~attrs=
      [Attr.classes(["sample-context-menu"])]
      @ SafeTriangle.CSSDropdown.menu_attrs(dropdown_id(sample.id)),
    sample_context_actions(~parent, ~ap_id, ~di, sample)
    @ sample_environment(~settings, sample, view_seg, utility),
  );

let sample_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      ~settings: settings,
      ~num_total: int,
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent,
      (index: int, sample: Sample.t),
    ) => {
  let has_dropdown = !(hide_env && ap_id == None);
  div(
    ~attrs=
      [Attr.classes(["sample"])]
      @ (
        has_dropdown
          ? SafeTriangle.CSSDropdown.trigger_attrs(dropdown_id(sample.id))
          : []
      ),
    [
      value_view(
        ~ap_id,
        ~settings,
        ~num_total,
        di,
        utility,
        view_seg,
        local,
        parent,
        sample,
        index,
      ),
    ]
    @ pin_view(~ap_id, di, sample)
    @ (
      has_dropdown
        ? [
          sample_context_menu(
            ~settings,
            ~parent,
            ~ap_id,
            ~di,
            sample,
            view_seg,
            utility,
          ),
        ]
        : []
    ),
  );
};

let sample_group_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      ~settings: settings,
      ~num_total: int,
      di: Dynamics.Info.t,
      utility,
      view_seg,
      local,
      parent,
      groups: list(list((int, Sample.t))),
    ) => {
  let group_views =
    List.map(
      samples =>
        Node.div(
          ~attrs=[Attr.classes(["sample-group"])],
          List.map(
            sample_view(
              ~ap_id,
              ~hide_env,
              ~settings,
              ~num_total,
              di,
              utility,
              view_seg,
              local,
              parent,
            ),
            samples,
          ),
        ),
      groups,
    );
  group_views == []
    ? [] : [div(~attrs=[Attr.classes(["sample-groups"])], group_views)];
};

/* Select a default sample by preferring the closest match to the current
 * dynamic cursor. */
let mv_least_distant_sample =
    (
      ~ap_id: option(Id.t),
      parent: external_action => Ui_effect.t(unit),
      dynamics: option(Dynamics.Info.t),
      _evt,
    )
    : Effect.t(unit) =>
  switch (dynamics) {
  | Some(di) =>
    let samples =
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=di.dyn_cursor.pinned_stack,
        di.samples,
      );
    switch (
      Sample.Selection.closest_to_cursor(
        ~ap_id,
        ~cursor=di.dyn_cursor,
        samples,
      )
    ) {
    | Some(selected) => parent(DynCursor(Capture(selected, ap_id)))
    | None => Effect.Ignore
    };
  | None => Effect.Ignore
  };

let ellipsis_view =
    (
      ~ap_id: option(Id.t),
      local,
      parent: external_action => Ui_effect.t(unit),
      info: info,
    )
    : Node.t =>
  div(
    ~attrs=[
      Attr.classes(["ellipsis"]),
      Attr.on_pointerdown(
        mv_least_distant_sample(~ap_id, parent, info.dynamics),
      ),
      Attr.on_double_click(_ => local(ToggleShowAllVals(0))),
    ],
    [text("⋯")],
  );

/* Unified view for explaining why no samples are shown */
let empty_status_view =
    (
      ~ap_id: option(Id.t),
      ~status: Sample.Selection.empty_status,
      local,
      parent: external_action => Ui_effect.t(unit),
      info: info,
    )
    : Node.t =>
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
        Attr.on_pointerdown(_ => parent(DynCursor(Reset))),
      ],
      [text("⍟")] //📌◌🔒
    )
  | NotAligned =>
    /* Reuse existing ellipsis behavior for not-aligned case */
    div(
      ~attrs=[
        Attr.classes(["empty-status", "not-aligned"]),
        Attr.title("Samples not aligned with cursor — click to align"),
        Attr.on_pointerdown(
          mv_least_distant_sample(~ap_id, parent, info.dynamics),
        ),
        Attr.on_double_click(_ => local(ToggleShowAllVals(0))),
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

let move_cursor =
    (
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      parent: external_action => Ui_effect.t(unit),
      offset: int,
    ) => {
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=di.dyn_cursor.pinned_stack,
      di.samples,
    );
  let cursor_idx =
    Sample.Selection.first_related_index(
      ~trimmed=true,
      ~ap_id,
      di.dyn_cursor,
      samples,
    );
  switch (cursor_idx) {
  /* Cursor would be outside window, reset to next visible sample */
  | Some(idx) =>
    let next_idx_maybe = idx - offset;
    if (next_idx_maybe >= 0 && next_idx_maybe < List.length(samples)) {
      parent(DynCursor(Capture(List.nth(samples, next_idx_maybe), ap_id)));
    } else {
      Effect.Ignore;
    };
  | _ => Effect.Ignore
  };
};

let nav_bar_view =
    (
      ap_id: option(Id.t),
      ~settings: settings,
      di: Dynamics.Info.t,
      num_total: int,
      parent: external_action => Ui_effect.t(unit),
    ) => {
  let nav_arrow = (cond: bool, offset: int): Node.t =>
    Node.div(
      ~attrs=[
        Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
        Attr.on_click(_ => move_cursor(~ap_id, di, parent, offset)),
      ],
      [],
    );
  let show_left = num_total < Sample.Window.max_samples(settings.window);
  let show_right = num_total < Sample.Window.max_samples(settings.window);
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
  );
};

let equals_view =
  div(~attrs=[Attr.classes(["live-equals"])], [text("≡")]);

let num_samples_view = (~ap_id: option(Id.t), di: Dynamics.Info.t) => {
  let num_samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=di.dyn_cursor.pinned_stack,
      di.samples,
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

let syntax_str = (utility: utility) =>
  Core.Memo.general(seg => {
    let max_len = 30;
    let seg = Segment.unparenthesize(seg);
    let str = utility.seg_to_string(seg);
    let str = StringUtil.replace(StringUtil.regexp("\n"), str, " ");
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  });
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let round_up = (~settings: settings, utility: utility, sample): unit => {
  let (_, cur) =
    abbreviated_seg_of(
      utility,
      SampleLength.get(settings.window, sample),
      sample.value,
    );
  let goal = cur + 1;
  let (_, max_len) =
    seg_of_exp(utility, DHExp.strip_ascriptions(sample.value));
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(utility, target, sample.value) |> snd;
    if (attempt_len < goal && target <= max_len) {
      find_target(target + 1);
    } else {
      target;
    };
  };
  SampleLength.set(sample.id, find_target(goal));
};

let round_down =
    (~settings: settings, utility: utility, sample: Sample.t): unit => {
  let (_, cur) =
    abbreviated_seg_of(
      utility,
      SampleLength.get(settings.window, sample),
      sample.value,
    );
  let goal = cur - 1;
  let rec find_target = (target: int): int => {
    let attempt_len =
      abbreviated_seg_of(utility, target, sample.value) |> snd;
    if (attempt_len > goal && target > 0) {
      find_target(target - 1);
    } else {
      target;
    };
  };
  SampleLength.set(sample.id, find_target(goal));
};

let indicated_sample =
    (~ap_id: option(Id.t), di: Dynamics.Info.t): option(Sample.t) =>
  Dynamics.Info.first_cursor_sample(ap_id, di);

let key_handler =
    (
      local,
      ~id: Id.t,
      ~ap_id: option(Id.t),
      ~settings: settings,
      di: Dynamics.Info.t,
      utility,
      parent: external_action => Ui_effect.t(unit),
      evt,
    ) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Settings.reset_mode();
    SampleLength.reset();
    parent(DynCursor(Reset));
  | D("Escape") =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Ignore;
  | D("ArrowRight") when key.shift == Down =>
    switch (indicated_sample(~ap_id, di)) {
    | Some(sample) => round_up(~settings, utility, sample)
    | None => ()
    };
    Many([local(NoOp), Stop_propagation, Prevent_default]);
  | D("ArrowLeft") when key.shift == Down =>
    switch (indicated_sample(~ap_id, di)) {
    | Some(sample) => round_down(~settings, utility, sample)
    | None => ()
    };
    Many([local(NoOp), Stop_propagation, Prevent_default]);
  | D("ArrowRight") =>
    // Prevent_default below stops aggressive horizontal scroll
    Many([
      move_cursor(~ap_id, di, parent, -1),
      Stop_propagation,
      Prevent_default,
    ])
  | D("ArrowLeft") =>
    Many([
      move_cursor(~ap_id, di, parent, 1),
      Stop_propagation,
      Prevent_default,
    ])
  | D(" ") =>
    Settings.go(ToggleWindow);
    Many([local(NoOp), Stop_propagation, Prevent_default]); // trigger redraw
  | D("p") =>
    /* Pin/Unpin the indicated sample */
    switch (indicated_sample(~ap_id, di), ap_id) {
    | (Some(_), Some(ap_id)) =>
      Many([
        pin_call(~parent, ~ap_id=Some(ap_id), ~di),
        Stop_propagation,
        Prevent_default,
      ])
    | _ =>
      print_endline("pin: no sample or ap_id");
      Many([Stop_propagation, Prevent_default]);
    }
  | D("Enter") =>
    /* Step into the indicated sample */
    switch (indicated_sample(~ap_id, di), ap_id) {
    | (Some(sample), Some(ap_id)) =>
      Many([
        Stop_propagation,
        Prevent_default,
        step_into_sample(~parent, ~sample, ~ap_id),
      ])
    | _ =>
      print_endline("step into: no sample or ap_id");
      Many([Stop_propagation, Prevent_default]);
    }
  | _ => Many([Stop_propagation])
  };
};

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (info: info): bool =>
  switch (info.statics) {
  | Some(
      InfoExp({term: {term: Var(_) | Probe({term: Var(_), _}, _), _}, _}),
    ) =>
    true
  | Some(InfoPat(_)) => true
  | _ => false
  };

let offside_view =
    (
      info: info,
      local,
      parent,
      ~settings: settings,
      view_seg:
        (~background: bool=?, ~text_only: bool=?, Sort.t, list(syntax)) =>
        Node.t,
      utility: utility,
    ) =>
  switch (info.dynamics) {
  | Some(di) =>
    let id = info.id;
    let ap_id = DynCursor.cur_ap(info.statics);
    let hide_env = hide_env(info);
    /* Filter samples once and reuse for both num_total and selection */
    let filtered_samples =
      Sample.Selection.filter_by_pin(
        ~ap_id,
        ~pinned=di.dyn_cursor.pinned_stack,
        di.samples,
      );
    let num_total = List.length(filtered_samples);
    let samples =
      select_samples(~settings, ~id, ~ap_id, ~filtered=filtered_samples, di);
    let (num_shown, groups) = Sample.Selection.collate(samples);

    /* Check if this probe is the target of a pending step-into focus */
    let is_evaluating =
      switch (di.dyn_cursor.pending_focus) {
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

    /* Overflow indicator: shown when samples ARE displayed but more exist */
    let has_overflow = num_shown > 0 && num_shown < num_total;
    let overflow_extras = [
      nav_bar_view(~settings, ap_id, di, num_total, parent),
      ellipsis_view(~ap_id, local, parent, info),
    ];

    Node.div(
      ~attrs=[
        Attr.id(Id.cls(id)),
        Attr.tabindex(0),
        Attr.on_keydown(
          key_handler(local, ~id, ~ap_id, ~settings, di, utility, parent),
        ),
        Attr.classes([
          "live-offside",
          settings.window |> Sample.Window.show_mode,
        ]),
      ],
      switch (empty_status) {
      | Some(status) => [
          empty_status_view(~ap_id, ~status, local, parent, info),
        ]
      | None =>
        [equals_view]
        @ sample_group_view(
            ~ap_id,
            ~hide_env,
            ~settings,
            ~num_total,
            di,
            utility,
            (~text_only: bool) => view_seg(~text_only, ~background=false),
            local,
            parent,
            groups,
          )
        @ (has_overflow ? overflow_extras : [])
      },
    );
  | None =>
    /* No dynamics info means probe was never evaluated */
    let ap_id = DynCursor.cur_ap(info.statics);
    Node.div(
      ~attrs=[
        Attr.id(Id.cls(info.id)),
        Attr.classes([
          "live-offside",
          settings.window |> Sample.Window.show_mode,
        ]),
      ],
      [
        empty_status_view(~ap_id, ~status=NoSamplesExist, local, parent, info),
      ],
    );
  };

let update = (() as m, _info: info, a: action) => {
  switch (a) {
  | ChangeLength(id, len) => SampleLength.set(id, len)
  | ToggleShowAllVals(_) => Settings.go(ToggleWindow)
  | NoOp => m
  };
};

let overlay_view = (info: info): Node.t =>
  switch (info.dynamics) {
  | Some(di) =>
    let ap_id = DynCursor.cur_ap(info.statics);
    div(
      ~attrs=[
        Attr.classes(["overlay"] @ (Option.is_some(ap_id) ? ["ap"] : [])),
      ],
      [num_samples_view(~ap_id, di)],
    );
  | None => Node.div([])
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
    | Any(_) => Some() /* Grout don't have sorts rn */
    | _ => None
    };

  let dynamics = true;

  let focusable =
    Focusable.{
      pointer: Some(id => JsUtil.get_elem_by_id(Id.cls(id))##focus),
      keyboard: None,
    };

  let placeholder = (_, info: info) =>
    ProjectorCore.Shape.inline(
      String.length(syntax_str(info.utility, info.syntax)),
    );

  let update = update;

  let view = ({info, local, parent, view_seg, _}: View.args(model, action)) => {
    let settings = Settings.s^;
    View.{
      inline: Node.div([]),
      overlay: Some(overlay_view(info)),
      offside:
        Some(
          offside_view(
            ~settings,
            info,
            local,
            parent,
            view_seg,
            info.utility,
          ),
        ),
    };
  };
};
