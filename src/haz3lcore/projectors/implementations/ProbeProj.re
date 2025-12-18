open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

open Js_of_ocaml;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type sample = Sample.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ChangeLength(int, int)
  | ToggleShowAllVals(int)
  | NoOp;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type window =
    | Single
    | Many;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sample_base =
    | Calls
    | Steps
    | StepRange;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type settings = {
    window,
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

  let init = {
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
        window: settings.window == Single ? Many : Single,
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

module Window = {
  let max_samples = (window: window) =>
    switch (window) {
    | Single => 1
    | Many => 30
    };

  let get_offset = (k: Id.t): int =>
    switch (Hashtbl.find_opt(offset, k)) {
    | Some(v) => v
    | None => 0
    };

  /* We are displaying a certain window of samples near the dynamic cursor.
   * If the synamic cursor moves, we want to readjust this window to show the
   * cursor, but only if necessary. Thus we compare the cursor position to the
   * current window bounds, and make the minimum change to the window necessary
   * to show the cursor. As an edge case, if there are less total samples than
   * the window size, we set the window to begin at zero. */
  let new_offest =
      (cursor_idx: int, home: int, max_samples: int, all_samples: int): int =>
    if (all_samples <= max_samples) {
      0;
    } else if (cursor_idx < home) {
      cursor_idx;
    } else if (cursor_idx >= home + max_samples) {
      cursor_idx - max_samples + 1;
    } else {
      home;
    };

  let set_offset = (k: Id.t, v: int) => Hashtbl.add(offset, k, v);

  let reform = (~window: window, id, all_samples, cursor_idx): (int, int) => {
    let max = max_samples(window);
    let new_home = new_offest(cursor_idx, get_offset(id), max, all_samples);
    set_offset(id, new_home);
    (new_home, max);
  };
};

let is_value = (exp: Exp.t) =>
  ValueChecker.check_value(Environment.empty, exp) == Value;

module ClosureLength = {
  let lengths: Hashtbl.t(int, int) = Hashtbl.create(100);

  let reset = () => {
    Hashtbl.clear(lengths);
  };

  let get = (window: window, sample: sample): int =>
    Hashtbl.find_opt(lengths, sample.id)
    |> Option.value(
         // TODO(andrew): relax 5, special-case multilines eg `case`
         ~default=
           /*!is_value(sample.value)
             ? 5 :*/ window
           == Single
             ? 150 : 12,
       );

  let set = (id: int, length: int): unit => Hashtbl.add(lengths, id, length);
};

/* Remove opaque values like function literals */
let rm_opaques: list(Sample.Env.entry) => list(Sample.Env.entry) =
  List.filter_map((en: Sample.Env.entry) =>
    switch (en.value) {
    | Opaque => None
    | Val(_) => Some(en)
    }
  );

module Samples = {
  let filter_frames_by_pin =
      (~ap_id: option(Id.t), di: Dynamics.Info.t): list(sample) =>
    switch (di.dyn_cursor.pinned_stack) {
    | Some(pinned_ap) =>
      List.filter(
        (sample: sample) =>
          ListUtil.hd_opt(pinned_ap) == ap_id  //TODO(andrew): should this clause exist?? why does this need to know ap_id..
          || ListUtil.is_suffix_of(pinned_ap, sample.call_stack),
        di.samples,
      )
    | None => di.samples
    };

  let total = (~ap_id: option(Id.t), di: Dynamics.Info.t): int =>
    List.length(filter_frames_by_pin(~ap_id, di));

  let first_related_index =
      (
        ~trimmed: bool,
        ~ap_id: option(Id.t),
        dyn_cursor: DynCursor.t,
        samples,
      )
      : option(int) => {
    let find = (rel: DynCursor.relation => bool): option(int) =>
      List.find_index(
        (sample: sample) =>
          rel(DynCursor.relation(~trimmed, ~ap_id, dyn_cursor, sample)),
        samples,
      );
    switch (find(relation => relation.is_call_cursor)) {
    | Some(idx) => Some(idx)
    | None =>
      switch (find(relation => relation.is_below_indicated_call == Some(0))) {
      | Some(idx) => Some(idx)
      | None =>
        let a = find(relation => relation.is_below_indicated_call != None);
        a == None ? find(DynCursor.is_related) : a;
      }
    };
  };

  let best_suffix_match =
      (~cursor: Probe.call_stack, samples: list(sample)): option(sample) =>
    List.fold_left(
      (best: option((sample, int)), sample: sample) => {
        let score = ListUtil.common_suffix_length(cursor, sample.call_stack);
        switch (best) {
        | Some((_, best_score)) when best_score >= score => best
        | _ => Some((sample, score))
        };
      },
      None,
      samples,
    )
    |> Option.map(fst);

  let closet_to_related_index =
      (~ap_id: option(Id.t), ~di: Dynamics.Info.t, samples: list(sample))
      : option(sample) =>
    switch (samples) {
    | [] => None
    | [first_sample, ..._] as all_samples =>
      let selected: sample =
        switch (
          first_related_index(
            ~trimmed=false,
            ~ap_id,
            di.dyn_cursor,
            all_samples,
          )
        ) {
        | Some(idx) =>
          List.nth_opt(all_samples, idx)
          |> Option.value(~default=first_sample)
        | None =>
          switch (
            best_suffix_match(
              ~cursor=DynCursor.trimmed_stack(di.dyn_cursor),
              all_samples,
            )
          ) {
          | Some(sample) => sample
          | None => first_sample
          }
        };
      Some(selected);
    };

  let select_samples =
      (
        ~settings: settings,
        ~id: Id.t,
        ~ap_id: option(Id.t),
        di: Dynamics.Info.t,
      )
      : list(sample) => {
    let samples = filter_frames_by_pin(~ap_id, di);
    let first_idx =
      first_related_index(~trimmed=false, ~ap_id, di.dyn_cursor, samples);
    if (first_idx == None && settings.window == Single) {
      [];
    } else {
      let cursor_idx =
        switch (first_idx) {
        | Some(idx) => idx
        | None => 0
        };
      let all_samples = List.length(samples);
      let (l, r) =
        Window.reform(~window=settings.window, id, all_samples, cursor_idx);
      ListUtil.slice(l, r, samples) |> List.rev;
    };
  };

  let group_by_predicate =
      /* Precondition: Items to be grouped are contigious in list */
      (should_group: ('a, 'a) => bool, xs: list('a)): list(list('a)) => {
    List.fold_left(
      (acc: list(list('a)), item: 'a) => {
        switch (acc) {
        | [] => [[item]]
        | [[rep, ..._] as first, ...init] when should_group(rep, item) => [
            first @ [item],
            ...init,
          ]
        | _ => [[item]] @ acc
        }
      },
      [],
      xs,
    );
  };

  let is_same_call = ((_, c1: sample), (_, c2: sample)): bool => {
    switch (List.rev(c2.call_stack), List.rev(c1.call_stack)) {
    | ([], _)
    | (_, []) => false
    | ([f1, ..._], [f2, ..._]) => f1 == f2
    };
  };

  let group = (samples: list((int, sample))): list(list((int, sample))) => {
    let grouped =
      samples |> group_by_predicate(is_same_call) |> List.map(List.rev);
    /* Flatten if all groups are singletons */
    List.for_all(group => List.length(group) == 1, grouped)
      ? [List.concat(grouped)] : grouped;
  };

  let collate = (samples: list(sample)): (int, list(list((int, sample)))) => {
    let numbered_samples =
      List.mapi((i, c) => (List.length(samples) - i - 1, c), samples);
    (List.length(samples), group(numbered_samples));
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
      sample: sample,
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

  let str = (~ap_id: option(Id.t), sample: sample): string =>
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
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent: external_action => Ui_effect.t(unit),
      sample: sample,
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
  let length = ClosureLength.get(settings.window, sample);
  let length = length == 12 && Samples.total(~ap_id, di) == 1 ? 150 : length;
  let (seg, length) = abbreviated_seg_of(utility, length, sample.value);

  div(
    ~attrs=[
      Attr.title(Debug.str(~ap_id, sample)),
      Attr.classes(
        ["value", length_cls(length)]
        @ cursor_clss(~settings, ~ap_id, di, sample)
        @ (Option.is_some(ap_id) ? ["ap"] : [])
        @ (!is_value(sample.value) ? ["indet"] : []),
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
            ClosureLength.get(settings.window, sample),
            d,
          );
        view_seg(~text_only=false, Sort.Exp, seg);
      },
    ],
  );
};

let show_pin = (~ap_id: option(Id.t), di: Dynamics.Info.t, sample: sample) => {
  switch (ap_id, di.dyn_cursor.pinned_stack) {
  | (Some(ap_id), Some(pinned_stack)) =>
    pinned_stack == [ap_id, ...sample.call_stack]
  | _ => false
  };
};

let pin_view = (~ap_id: option(Id.t), di: Dynamics.Info.t, sample) =>
  show_pin(~ap_id, di, sample)
    ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

let env_view =
    (
      ~settings: settings,
      ~parent,
      ~ap_id,
      ~di,
      sample: sample,
      view_seg,
      utility: utility,
    )
    : Node.t =>
  div(
    ~attrs=[Attr.classes(["sample-dropdown"])],
    (
      ap_id != Option.None
        ? {
          let show_pin = show_pin(~ap_id, di, sample);
          [
            div(
              ~attrs=[
                Attr.classes(
                  ["live-env-header"] @ (show_pin ? ["pinned"] : []),
                ),
                Attr.on_pointerdown(_ => pin_call(~parent, ~ap_id, ~di)),
              ],
              [
                div(~attrs=[Attr.classes(["pin-icon"])], []),
                text(show_pin ? "Unpin" : "Pin"),
              ],
            ),
          ];
        }
        : []
    )
    @ {
      let elems = sample.env |> ListUtil.dedup |> rm_opaques;
      elems == []
        ? []
        : [
          div(
            ~attrs=[Attr.classes(["live-env"])],
            List.map(env_val(~settings, sample, view_seg, utility), elems),
          ),
        ];
    },
  );

let sample_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      ~settings: settings,
      di: Dynamics.Info.t,
      utility: utility,
      view_seg,
      local,
      parent,
      (index: int, sample: sample),
    ) =>
  div(
    ~attrs=[Attr.classes(["sample"])],
    [
      value_view(
        ~ap_id,
        ~settings,
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
      hide_env && ap_id == None
        ? []
        : [
          env_view(
            ~settings,
            ~parent,
            ~ap_id,
            ~di,
            sample,
            view_seg,
            utility,
          ),
        ]
    ),
  );

let sample_group_view =
    (
      ~ap_id: option(Id.t),
      ~hide_env: bool,
      ~settings: settings,
      di: Dynamics.Info.t,
      utility,
      view_seg,
      local,
      parent,
      groups: list(list((int, sample))),
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
    let samples = Samples.filter_frames_by_pin(~ap_id, di);
    switch (Samples.closet_to_related_index(~ap_id, ~di, samples)) {
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

let move_cursor =
    (
      ~ap_id: option(Id.t),
      di: Dynamics.Info.t,
      parent: external_action => Ui_effect.t(unit),
      offset: int,
    ) => {
  let samples = Samples.filter_frames_by_pin(~ap_id, di);
  let cursor_idx =
    Samples.first_related_index(
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
  let show_left = num_total < Window.max_samples(settings.window);
  let show_right = num_total < Window.max_samples(settings.window);
  div(
    ~attrs=[Attr.classes(["nav-bar"])],
    [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
  );
};

let equals_view =
  div(~attrs=[Attr.classes(["live-equals"])], [text("≡")]);

let num_samples_view = (~ap_id: option(Id.t), di: Dynamics.Info.t) => {
  let num_samples = Samples.total(~ap_id, di);
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
      ClosureLength.get(settings.window, sample),
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
  ClosureLength.set(sample.id, find_target(goal));
};

let round_down = (~settings: settings, utility: utility, sample: sample): unit => {
  let (_, cur) =
    abbreviated_seg_of(
      utility,
      ClosureLength.get(settings.window, sample),
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
  ClosureLength.set(sample.id, find_target(goal));
};

let indicated_sample =
    (~ap_id: option(Id.t), di: Dynamics.Info.t): option(sample) =>
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
  /* PLAN: inter-probe navigation
      ultimately need to be able to issue a parent action to move to and focus on
     another projector. for now, should be able to use the Project(Focus(id)) action
     to do both in one; will need to rethink when we want to /create/ probes as well.
     the probe that we want to move to is going to depend on the dynamic cursor, but
     also maybe the row of the sample we're on. alternatively, can maybe avoid
     row based logic by using sample creation time instead. In any case, want a function
     that takes the dynamic cursor and emits a new dynamic cursor and the id of a
     probe to jump to. Not sure this is the best approach at all, but for now maybe
     we could add all probe data to a common mutable structure in this module, when
     projectorview.all is called, and use this to calculate the probe id to jump to.
     like basically we're going to treat this mutable cache as a db, and do certain
     queries. specifically, return all probe_ids that have a sample with equal
     dynamic cursor to current, and take the one with the timestamp closet to but
     before/after the current dynamic cursor sample timestamp. */
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("Escape") when key.shift == Down =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Settings.reset_mode();
    ClosureLength.reset();
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
    // hack: Prevent_default below stops aggressive horizontal scroll
    // noop to trigger redraw
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
    let num_total = Samples.total(~ap_id, di);
    let samples = Samples.select_samples(~settings, ~id, ~ap_id, di);
    let (num_shown, groups) = Samples.collate(samples);
    let is_cut_off =
      num_shown != num_total && (num_shown != 0 || num_total != 0);
    let extras = [
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
        Attr.classes(["live-offside", settings.window |> show_window]),
      ],
      (num_shown > 0 ? [equals_view] : [])
      @ sample_group_view(
          ~ap_id,
          ~hide_env,
          ~settings,
          di,
          utility,
          (~text_only) => view_seg(~text_only, ~background=false),
          local,
          parent,
          groups,
        )
      @ (is_cut_off ? extras : []),
    );
  | _ => Node.div([])
  };

let update = (() as m, _info: info, a: action) => {
  switch (a) {
  | ChangeLength(id, len) => ClosureLength.set(id, len)
  | ToggleShowAllVals(_) => Settings.go(ToggleWindow)
  | NoOp => m
  };
};

// let is_pinned = (ap_id: option(Id.t), di: Dynamics.Info.t): bool =>
//   switch (Dynamics.Info.is_in(di)) {
//   | Some(dyn_cursor) =>
//     di.dyn_cursor.pinned_stack
//     == DynCursor.cur_call(ap_id, dyn_cursor)
//   | _ => false
//   };

// let view = (~settings: settings, local, parent, info: info): Node.t =>
//   div(
//     ~attrs=[
//       Attr.id(Id.cls(info.id)),
//       Attr.tabindex(0),
//       Attr.on_keydown(
//         key_handler(
//           ~settings,
//           local,
//           ~id=info.id,
//           ~ap_id=cur_ap(info),
//           Option.value(info.dynamics, ~default=Dynamics.Info.init),
//           info.utility,
//           parent,
//         ),
//       ),
//       Attr.classes(
//         ["main"]
//         @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
//         @ (
//           switch (info.dynamics) {
//           | Some(di) => is_pinned(cur_ap(info), di) ? ["pinned"] : []
//           | None => []
//           }
//         ),
//       ),
//       Attr.on_double_click(_ =>
//         switch (
//           cur_ap(info),
//           info.dynamics |> OptUtil.and_then(Dynamics.Info.is_in),
//         ) {
//         | (Some(ap_id), Some(dyn_cursor)) =>
//           parent(
//             DynCursor(TogglePinCall([ap_id, ...dyn_cursor.call_stack])),
//           )
//         | _ => Effect.Ignore
//         }
//       ),
//       Attr.on_pointerdown(_
//         /* Select a default cell if one is not already selected */
//         => probe_default(parent, info)),
//       Attr.on_pointerup(_ => {
//         JsUtil.get_elem_by_id(Id.cls(info.id))##blur;
//         Effect.Ignore;
//       }),
//     ],
//     [text(syntax_str(info.utility, info.syntax)) /*, icon*/],
//   );

let overlay_view = (info: info): Node.t =>
  switch (info.dynamics) {
  | Some(di) =>
    let ap_id = DynCursor.cur_ap(info.statics);
    div(
      ~attrs=[
        Attr.classes(
          ["overlay"] @ (Option.is_some(ap_id) ? ["ap"] : []),
          // @ (is_pinned(ap_id, di) ? ["pinned"] : []),
        ),
      ],
      [num_samples_view(~ap_id, di)] /*@ pin_view(info)*/,
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
      /*2 +*/ String.length(syntax_str(info.utility, info.syntax)),
    );

  let update = update;

  let view = ({info, local, parent, view_seg, _}: View.args(model, action)) => {
    let settings = Settings.s^;
    View.{
      inline: Node.div([]),
      // switch (info.syntax) {
      // | [Grout({id, _})] when id == Id.invalid => Node.div([])
      // | _ => view(~settings, local, parent, info)
      // },
      overlay:
        switch (info.syntax) {
        | [Grout({id, _})] when id == Id.invalid =>
          Some(overlay_view(info))
        | _ => Some(overlay_view(info))
        },
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
