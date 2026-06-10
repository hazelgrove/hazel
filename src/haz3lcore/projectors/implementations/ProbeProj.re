open Util;
open ProjectorBase;

open Language;
open RichProbe;
open RichProbeRegistry;

/* Probe projector logic: sample selection, display settings, and the
 * model/action state machine. The web (Vdom) view — sample rendering,
 * keyboard handling, rich-probe modal — lives in
 * src/web/projectors/ProbeProjView.re, reusing the helpers below. */

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

/* Filter environment entries: dedup, remove opaques, exclude filter_vars */
let filtered_env_entries =
    (~filter_vars: list(string), sample: Sample.t): list(Sample.Env.entry) =>
  sample.env
  |> ListUtil.dedup
  |> Sample.Env.remove_opaques
  |> List.filter((en: Sample.Env.entry) =>
       !List.mem(en.binding.name, filter_vars)
     );

/* Don't redundantly show an env for variable references, patterns */
let hide_env = (statics: Language.Statics.Info.t): bool =>
  switch (statics) {
  | InfoExp({user_term: {term: Var(_), _}, _}) => true
  | InfoPat(_) => true
  | _ => false
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

[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector with type model = probe_model and type action = a = {
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

  let error = (_, _): option(ProjectorBase.error) => None;
};
