open Util;

/* Specifies which environment bindings to capture for a sample.
 * This could be extended to specify other aspects of capture. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type capture_spec = {refs: Binding.s};

let empty_capture_spec: capture_spec = {refs: []};

/* Maps expression/pattern IDs to their capture specifications.
 * Presence in this map means "collect a sample when evaluated". */
type targets = Id.Map.t(capture_spec);

let no_targets: targets = Id.Map.empty;

/* Deriving functions for targets type alias */
let pp_targets = (fmt, targets: targets) =>
  Id.Map.pp(pp_capture_spec, fmt, targets);
let show_targets = (targets: targets) =>
  Format.asprintf("%a", pp_targets, targets);
let sexp_of_targets = (targets: targets) =>
  Id.Map.sexp_of_t(sexp_of_capture_spec, targets);
let targets_of_sexp = sexp => Id.Map.t_of_sexp(capture_spec_of_sexp, sexp);
let yojson_of_targets = (targets: targets) =>
  Id.Map.yojson_of_t(yojson_of_capture_spec, targets);
let targets_of_yojson = json =>
  Id.Map.t_of_yojson(capture_spec_of_yojson, json);
let equal_targets = (t1: targets, t2: targets) =>
  Id.Map.equal(equal_capture_spec, t1, t2);

/* A probe sample records a value and an environment,
 * along with a `call_stack` which records partial information
 * about the execution trace prior to the sample being taken */

module Env = {
  /* To avoid unnecessary de/serialization from evaluation worker,
   * we refrain from retaining certain large un-educational values,
   * such as closures. Which values are made opaque can be modulated
   * via the below `elide` function */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type elided_value = CallStack.elided_value;

  /* A probe environment entry is a variable binding
   * along with its corresponding elided value */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type entry = {
    binding: Binding.t,
    value: elided_value,
  };

  /* A probe environment is a summarized version of the
   * dynamic environment of the probed expression */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = list(entry);

  let empty = [];

  /* Selectively elide dynamic information not currently
   * being used in the live probe UI, for (putative, unbenchmarked)
   * performance purposes for worker de/serialization */
  let elide = (env: Environment.t(Exp.t), d: DHExp.t): elided_value =>
    switch ((d |> DHExp.strip_ascriptions).term) {
    | Fun(_)
    | FixF(_)
    | Closure(_)
    | BuiltinFun(_) => Opaque
    | _ => Val(d |> DHExp.strip_ascriptions |> Substitution.in_exp(env))
    };

  let mk_entry = (env: Environment.t(Exp.t), {name, id, _}: Binding.t) =>
    switch (Environment.lookup(env, name)) {
    | Some(d) =>
      let binding =
        Binding.{
          name,
          id,
        };
      Some({
        binding,
        value: elide(env, d),
      });
    | None => None
    };

  let filter = (env: Environment.t(Exp.t), bound_in: Binding.s) =>
    List.filter_map(mk_entry(env), bound_in);

  /* Remove opaque values (like function literals) from environment entries */
  let remove_opaques: list(entry) => list(entry) =
    List.filter_map((en: entry) =>
      switch (en.value) {
      | Opaque => None
      | Val(_) => Some(en)
      }
    );
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type origin =
  | Probe
  | Print; /* Print statements for probes study */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: int, /* Primary ID (unique-ish) */
  syntax_id: Id.t, /* Syntax ID of probed expression */
  value: DHExp.t, /* Value of expression */
  env: Env.t, /* (Filtered) Environment Values  */
  call_stack: CallStack.t, /* Call stacks as ap ids */
  args: option(Env.elided_value), /* Argument value if probe is on an Ap */
  time: float, /* Time of evaluation */
  seq: int, /* Sequence number: a count index of each sample taken */
  origin, /* Is this sample from a probe or a print statement */
  step_start: int, /* Step count when expression began evaluation */
  step_end: int /* Step count when expression finished evaluation */
};

let seq_counter = ref(0);

let mk =
    (
      ~origin: origin=Probe,
      ~args: option(Env.elided_value)=None,
      ~step_start: int,
      ~step_end: int,
      syntax_id: Id.t,
      value: DHExp.t,
      env: Environment.t(Exp.t),
      stack: CallStack.t,
      spec: capture_spec,
    )
    : t => {
  /* Below hash provides a coarse-grained identification of
   * samples currently used to keep display-length data between
   * similar runs. May want to alter this or simply used a fresh
   * UUID depending on future desiderata */
  id: Hashtbl.hash((stack, syntax_id)),
  syntax_id,
  value,
  env: Env.filter(env, spec.refs),
  call_stack: stack,
  args,
  time: Clock.precise_timestamp(),
  seq: {
    seq_counter := seq_counter^ + 1;
    seq_counter^;
  },
  origin,
  step_start,
  step_end,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type sample = t;

/* Samples recorded during evaluation, indexed by the
 * syntax ids of their initial expressions */
module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(list(sample));

  let empty = Id.Map.empty;

  /* Samples are stored in reverse order (prepend for O(1) insert),
   * so we reverse on lookup to return them in evaluation order */
  let lookup = (id, map) =>
    Id.Map.find_opt(id, map) |> Option.map(List.rev);

  /* Fold over the map, reversing each sample list to evaluation order */
  let fold = (f, map: t, init) =>
    Id.Map.fold(
      (id, samples, acc) => f(id, List.rev(samples), acc),
      map,
      init,
    );

  /* Prepend for O(1) insertion - list is reversed on lookup */
  let extend = (id, report, map: t) =>
    Id.Map.update(
      id,
      opt =>
        switch (opt) {
        | Some(a) => Some([report, ...a])
        | None => Some([report])
        },
      map,
    );
};

/* Display mode for probe samples */
module Window = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Single /* Show one sample aligned with sightline */
    | Many; /* Show multiple samples in a scrollable window */

  /* Max samples to display for each mode */
  let max_samples = (mode: mode): int =>
    switch (mode) {
    | Single => 1
    | Many => 30
    };

  /* Calculate new window offset to keep cursor visible.
   * Returns the minimum adjustment needed to show cursor_idx
   * within the window [home, home + max_samples). */
  let adjusted_offset =
      (~cursor_idx: int, ~current_offset: int, ~max_samples: int, ~total: int)
      : int =>
    if (total <= max_samples) {
      0;
    } else if (cursor_idx < current_offset) {
      cursor_idx;
    } else if (cursor_idx >= current_offset + max_samples) {
      cursor_idx - max_samples + 1;
    } else {
      current_offset;
    };
};

/* THE SIGHTLINE
 *
 * The sample focus is a sightline through the call tree: a positioned
 * path that determines which sample each probe displays. It stores
 * (call_stack, index) where call_stack is the full path and index
 * divides it into three zones:
 *
 *   above:  where you ARE (context — calls you're nested inside)
 *   focus:  where you're LOOKING (the active depth)
 *   below:  where you've BEEN or are PEEKING (retained/extended path)
 *
 * A probe aligns with the sightline by suffix matching: a sample is
 * on the sightline if its call stack is a suffix of the sightline's
 * path. This enforces coherence (samples at different depths must be
 * on the same path through the call tree).
 *
 * The below-focus zone is populated two ways:
 *
 *   Retention: navigating shallower keeps deeper frames. If the new
 *     sample's stack is a suffix of the current sightline, we keep the
 *     full sightline and lower the index.
 *
 *   Extension: clicking an app probe prepends the application as a
 *     frame below the focus (peeking into a call without entering it).
 *
 * Both produce frames below the index that participate identically in
 * alignment. This is what makes one sightline sufficient: the full
 * path encodes the user's chosen branch at every depth without needing
 * separate state per nesting level.
 *
 * ALIGNMENT (read side — Selection.most_aligned_index):
 *
 * Two-tier suffix scan, in priority order:
 *   1. Above-focus scan (effective_stack): "where am I?" — finds the
 *      sample at the user's active depth. Governs direct interaction.
 *   2. Full sightline scan (call_stack): "where was I?" — recovers
 *      retained/extended selections for probes at other nesting levels.
 *
 * Where you are beats where you've been. The effective stack caps the
 * scan to handle recursive functions (where all frames share one ID
 * and suffix matching would otherwise always pick the deepest sample).
 *
 * See plans/sample-focus-sightline.md for the full exegesis. */
module Focus = {
  open OptUtil.Syntax;

  /* Pending focus state for step-into functionality.
   * When set, the system will focus the matching sample
   * after evaluation completes. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type pending_focus = {
    probe_id: Id.t, /* The probe we're stepping into */
    target_stack: CallStack.t /* The call stack to match */
  };

  /* Focus.t fields:
   * - call_stack: Full call context; may be deeper than effective cursor
   * - index: Effective depth in call_stack (-1 = top-level, 0+ = inside calls)
   * - pinned_stack: If set, filters samples to those under this call context
   * - indicated_call: Syntax ID of function call under syntax cursor (for step-into)
   * - time: Timestamp of focused sample (for time-based correlation)
   * - seq: Sequence number of focused sample (for ordering-based correlation)
   * - step_range: (start, end) step counts of focused sample
   * - pending_focus: After step-into, where to focus when evaluation completes */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    call_stack: CallStack.t,
    index: int,
    pinned_stack: option(CallStack.t),
    indicated_call: option(Id.t),
    time: option(float),
    seq: int,
    step_range: option((int, int)),
    pending_focus: option(pending_focus),
  };

  let init: t = {
    call_stack: [],
    index: (-1),
    pinned_stack: None,
    indicated_call: None,
    time: None,
    seq: 0,
    step_range: None,
    pending_focus: None,
  };

  /* The above-focus portion of the sightline: call_stack sliced to
   * index + 1 elements from the outer end. This is where you ARE —
   * the active position used for tier 1 alignment. The full call_stack
   * extends deeper (below-focus) for tier 2 alignment. */
  let effective_stack = (cursor: t): CallStack.t =>
    ListUtil.slice(0, cursor.index + 1, cursor.call_stack |> List.rev)
    |> List.rev;

  /* Relative call-stack depth between two samples */
  type relative_level =
    | Above(int)
    | Below(int)
    | Same
    | Unrelated;

  /* Step-range containment relationship between two samples */
  type step_containment =
    | StepEqual /* Same step range */
    | StepContainedWithin /* This sample is strictly inside the focus */
    | StepContains /* This sample strictly contains the focus */
    | StepDisjointBefore /* This sample finishes before focus starts */
    | StepDisjointAfter /* This sample starts after focus finishes */
    | StepNoFocus; /* No focus sample to compare against */

  let step_containment =
      (~focus_range: option((int, int)), sample: sample): step_containment =>
    switch (focus_range) {
    | None => StepNoFocus
    | Some((fs, fe)) =>
      let (ss, se) = (sample.step_start, sample.step_end);
      if (ss == fs && se == fe) {
        StepEqual;
      } else if (ss >= fs && se <= fe) {
        StepContainedWithin;
      } else if (ss <= fs && se >= fe) {
        StepContains;
      } else if (se < fs) {
        StepDisjointBefore;
      } else {
        StepDisjointAfter;
      };
    };

  /* How is a sample related to the cursor? */
  type relation = {
    is_call_cursor: bool,
    is_more_precise_than_cursor: bool,
    relative_level_to_cursor: relative_level,
    is_call_above_call_cursor: option(int),
    is_below_indicated_call: option(int),
  };

  let is_below = ListUtil.suffix_at_depth(~eq=CallStack.equal_frame);

  let relative_level = (cs1: CallStack.t, cs2: CallStack.t): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Unrelated
    };

  let cur_call = (ap_id: option(Id.t), sample: sample): option(CallStack.t) => {
    let* ap_id = ap_id;
    Some(CallStack.extend(ap_id, sample.call_stack));
  };

  /* Returns Some(ap_id) only when cursor is on an application with a variable
     in function position (enabling step-into and pinning). Excludes constructor
     applications and applications with non-variable functions (e.g. lambdas). */
  let cur_var_ap = (info: Info.t): option(Id.t) =>
    switch (info) {
    | InfoExp({user_term: {term: Ap(_, {term: Var(_), _}, _), _} as ap, _}) =>
      Some(Exp.rep_id(ap))
    | _ => None
    };

  let relation =
      (~trimmed: bool, ~ap_id: option(Id.t), cursor: t, sample: sample)
      : relation => {
    let this = sample.call_stack;
    let cursor_stack = trimmed ? effective_stack(cursor) : cursor.call_stack;
    {
      is_call_cursor: CallStack.equal(cursor_stack, this),
      is_more_precise_than_cursor:
        List.length(cursor.call_stack) > List.length(sample.call_stack),
      relative_level_to_cursor: relative_level(cursor_stack, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(ap_id, sample);
        is_below(cur_call, cursor_stack);
      },
      is_below_indicated_call: {
        let* cur_ap = cursor.indicated_call;
        is_below(CallStack.extend(cur_ap, cursor_stack), this);
      },
    };
  };

  let is_related = (relation: relation): bool =>
    switch (relation.relative_level_to_cursor) {
    | Above(_)
    | Below(_)
    | Same => true
    | Unrelated => false
    };
};

/* Sample selection and filtering logic */
module Selection = {
  /* Categorizes why no samples are shown for a probe */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type empty_status =
    | NoSamplesExist /* Probe was never evaluated */
    | HiddenByPin /* Samples exist but filtered by current pin */
    | NotAligned /* Single mode: samples exist but none align with sightline */
    | Evaluating; /* Waiting for evaluation after step-into */

  /* Determine why no samples are shown.
   * Returns None if samples ARE shown, Some(status) if empty.
   * ~is_evaluating: true if we're waiting for this probe's samples after step-into */
  let get_empty_status =
      (~num_total: int, ~num_shown: int, ~is_evaluating: bool=false, ())
      : option(empty_status) =>
    if (num_shown > 0) {
      None;
    } else if (is_evaluating) {
      Some(Evaluating);
    } else if (num_total == 0) {
      Some(HiddenByPin);
    } else {
      Some(NotAligned);
    };

  /* Filter samples by pinned call stack.
   * Print-origin samples are excluded — they are only for the Printarium. */
  let filter_by_pin =
      (
        ~ap_id: option(Id.t),
        ~pinned: option(CallStack.t),
        samples: list(t),
      )
      : list(t) => {
    let samples = List.filter((s: t) => s.origin != Print, samples);
    switch (pinned) {
    | Some(pinned_stack) =>
      /* Extract just the Id.t from head of pinned_stack for comparison */
      let pinned_head_id =
        Option.map(
          (f: CallStack.frame) => f.id,
          ListUtil.hd_opt(pinned_stack),
        );
      /* Compare by ID only - pinned_stack may have None for function names
       * but actual samples have real names from evaluation */
      let pinned_ids = CallStack.ids_of_stack(pinned_stack);
      List.filter(
        (sample: t) => {
          let sample_ids = CallStack.ids_of_stack(sample.call_stack);
          pinned_head_id == ap_id
          /* Sample is at or below pin (current behavior) */
          || ListUtil.is_suffix_of(pinned_ids, sample_ids)
          /* Probe is on an application in the pinned call chain,
           * and sample is above pin on same ancestral path (breadcrumbs) */
          || ListUtil.is_suffix_of(sample_ids, pinned_ids)
          && (
            switch (ap_id) {
            | Some(id) => List.mem(id, pinned_ids)
            | None => false
            }
          );
        },
        samples,
      );
    | None => samples
    };
  };

  /* Find the sample most aligned with the sightline.
   *
   * Alignment = suffix matching: a sample is on the sightline if its
   * call stack is a suffix of the sightline's path.
   *
   * Two-tier scan:
   *   1a. Suffix match against effective_stack (above-focus portion).
   *       "Where am I?" — finds the sample at the user's active depth.
   *   1b. Suffix match against full call_stack (including below-focus).
   *       "Where was I?" — recovers retained or extended selections
   *       for probes at other nesting levels.
   *
   * Tier 1a takes priority: where you are beats where you've been.
   * The effective stack caps the scan to handle recursive functions
   * (where identical frame IDs make all depths mutual suffixes).
   *
   * Fallback tiers:
   *   2.  Direct callee of indicated call (step-into).
   *   3.  Any related sample (Above/Below/Same relative to effective stack).
   */
  let most_aligned_index =
      (~ap_id: option(Id.t), cursor: Focus.t, samples: list(t))
      : option(int) => {
    let suffix_scan = (stack: CallStack.t): option(int) =>
      List.fold_left(
        (best: option((int, int)), (i, sample: t)) => {
          let slen = List.length(sample.call_stack);
          if (slen > 0
              && slen > (best |> Option.map(snd) |> Option.value(~default=0))
              && ListUtil.is_suffix_of(
                   ~eq=CallStack.equal_frame,
                   sample.call_stack,
                   stack,
                 )) {
            Some((i, slen));
          } else {
            best;
          };
        },
        None,
        List.mapi((i, s) => (i, s), samples),
      )
      |> Option.map(fst);
    /* Tier 1a: suffix match against above-focus (where you are) */
    let eff = Focus.effective_stack(cursor);
    let effective_match = suffix_scan(eff);
    /* Tier 1b: suffix match against full sightline (where you've been) */
    let full_match =
      switch (effective_match) {
      | Some(_) => effective_match
      | None => suffix_scan(cursor.call_stack)
      };
    /* Fallback tiers use above-focus stack (depth-relative comparisons) */
    let find = (predicate: Focus.relation => bool): option(int) =>
      List.find_index(
        (sample: t) =>
          predicate(Focus.relation(~trimmed=true, ~ap_id, cursor, sample)),
        samples,
      );
    let result =
      switch (full_match) {
      | Some(_) as result => result
      | None =>
        switch (find(rel => rel.is_call_cursor)) {
        | Some(_) as result => result
        | None =>
          switch (find(rel => rel.is_below_indicated_call == Some(0))) {
          | Some(_) as result => result
          | None =>
            let indirect = find(rel => rel.is_below_indicated_call != None);
            indirect == None ? find(Focus.is_related) : indirect;
          }
        }
      };
    result;
  };

  /* Find sample most aligned with the sightline (returns the sample
   * rather than the index). Used by ArrowUp/Down navigation and probe
   * click alignment. Same suffix-scan logic as most_aligned_index. */
  let most_aligned_sample =
      (~ap_id: option(Id.t), ~cursor: Focus.t, samples: list(t)): option(t) =>
    switch (samples) {
    | [] => None
    | [first, ..._] =>
      switch (most_aligned_index(~ap_id, cursor, samples)) {
      | Some(idx) => List.nth_opt(samples, idx)
      | None => Some(first)
      }
    };

  /* Check if two samples belong to the same function call */
  let is_same_call = (s1: t, s2: t): bool =>
    switch (List.rev(s2.call_stack), List.rev(s1.call_stack)) {
    | ([], _)
    | (_, []) => false
    | ([f1, ..._], [f2, ..._]) => CallStack.equal_frame(f1, f2)
    };

  /* Group samples by function call, with indices */
  let group_by_call = (samples: list(t)): list(list(t)) => {
    let grouped =
      samples
      |> ListUtil.group_consecutive((s1, s2) => is_same_call(s1, s2))
      |> List.map(List.rev);
    /* Flatten if all groups are singletons */
    List.for_all(g => List.length(g) == 1, grouped)
      ? [List.concat(grouped)] : grouped;
  };

  /* Number and group samples for display */
  let collate = (samples: list(t)): (int, list(list(t))) => (
    List.length(samples),
    group_by_call(samples),
  );

  /* Select samples to display based on cursor position and window mode.
   * Pure function - offset is passed in and new offset returned. */
  let select =
      (
        ~mode: Window.mode,
        ~offset: int,
        ~ap_id: option(Id.t),
        ~pinned: option(CallStack.t),
        ~cursor: Focus.t,
        samples: list(t),
      )
      : (list(t), int) => {
    let filtered = filter_by_pin(~ap_id, ~pinned, samples);
    let first_idx = most_aligned_index(~ap_id, cursor, filtered);
    if (first_idx == None && mode == Single) {
      ([], offset);
    } else {
      let cursor_idx = first_idx |> Option.value(~default=0);
      let total = List.length(filtered);
      let max = Window.max_samples(mode);
      let new_offset =
        Window.adjusted_offset(
          ~cursor_idx,
          ~current_offset=offset,
          ~max_samples=max,
          ~total,
        );
      let selected = ListUtil.slice(new_offset, max, filtered) |> List.rev;
      (selected, new_offset);
    };
  };
};

/* Backward-compatible alias used by older callsites. */
module Cursor = Focus;

/* Lightweight capture data for passing through actions.
   In a submodule to avoid type inference issues with Sample.t
   which has overlapping field names. */
module Capture = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    time: float,
    seq: int,
    call_stack: CallStack.t,
    step_start: int,
    step_end: int,
  };
};

let capture_of_sample = (sample: t): Capture.t => {
  time: sample.time,
  seq: sample.seq,
  call_stack: sample.call_stack,
  step_start: sample.step_start,
  step_end: sample.step_end,
};
