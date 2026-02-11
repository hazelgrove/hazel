open Util;

/* Specifies which environment bindings to capture for a sample.
 * This could be extended to specify other aspects of capture. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type capture_spec = {refs: Binding.s};

let empty_capture_spec: capture_spec = {refs: []};

/* A single frame in the call stack: app_id + optional function_name.
 * function_name is extracted at evaluation time from the closure/function.
 * The name field is purely informational for display; equality compares only id. */
[@deriving (show({with_path: false}), sexp, yojson)]
type stack_frame = {
  id: Id.t,
  name: option(string),
};

let equal_stack_frame = (a: stack_frame, b: stack_frame): bool =>
  a.id == b.id;

/* Call context represented as a list of stack frames.
 * The head is the most recent (innermost) call. */
[@deriving (show({with_path: false}), sexp, yojson)]
type call_stack = list(stack_frame);

let equal_call_stack = (a: call_stack, b: call_stack): bool =>
  List.equal(equal_stack_frame, a, b);

/* Extract just the IDs from a call stack, discarding function names. */
let ids_of_stack = (cs: call_stack): list(Id.t) =>
  List.map((f: stack_frame) => f.id, cs);

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
  type elided_value =
    | Opaque
    | Val(DHExp.t);

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
  let elide = (env: Environment.t(Exp.t), d: DHExp.t) =>
    switch ((d |> DHExp.strip_ascriptions).term) {
    | Fun(_)
    | FixF(_)
    | Closure(_) => Opaque
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
  call_stack, /* Call stacks as ap ids */
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
      stack: call_stack,
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
  time: JsUtil.precise_timestamp(),
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
    | Single /* Show one sample aligned with cursor */
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

/* The dynamic cursor points to a stage in evaluation, associated
 * with probe sample collection. This is primarily reified as a call stack,
 * represented as a list of ids of function application forms which have
 * been called but have not yet returned.
 *
 * CONSISTENCY AND INTENT PRESERVATION
 *
 * Two goals govern how probe sample displays update during navigation:
 *
 * 1. CONSISTENCY (mandatory): Samples shown at different call depths must
 *    be consistent with each other in the call stack sense. If you navigate
 *    to a sample at one depth, samples emphasized at other depths must be
 *    above or below it in the same execution context.
 *
 * 2. INTENT PRESERVATION (when consistency allows): When multiple samples
 *    would be consistent with a navigation action, prefer keeping the
 *    user's previous selection rather than resetting to a default.
 *
 * Example in single-sample display mode:
 *
 *   let grid = [[1, 2], [3, 4]] in
 *   map(grid, fun ^^probe(row) ->
 *     map(row, fun ^^probe(x) -> x))
 *
 * Outer probe samples: [1,2], [3,4]
 * Inner probe samples: 1, 2, 3, 4
 * Samples 1,2 are below [1,2]; samples 3,4 are below [3,4].
 *
 * CONSISTENCY examples (navigation forces change):
 *
 *   # Frame A: Arrow outer from [1,2] to [3,4] #
 *   fun ^^probe(row) ->           # [1,2] -> [3,4]
 *     map(row, fun ^^probe(x) ..) # 2 -> 3 (must change: 2 not below [3,4])
 *
 *   # Frame B: Arrow inner from 2 to 3 #
 *   fun ^^probe(row) ->           # [1,2] -> [3,4] (must change: 3 not below [1,2])
 *     map(row, fun ^^probe(x) ..) # 2 -> 3
 *
 * INTENT PRESERVATION example (consistency underdetermines):
 *
 *   # Frame C1: User arrows inner from 1 to 2 #
 *   fun ^^probe(row) ->           # [1,2]
 *     map(row, fun ^^probe(x) ..) # 1 -> 2
 *
 *   # Frame C2: User clicks on outer [1,2] (already above inner 2) #
 *   fun ^^probe(row) ->           # [1,2] (cursor moves to outer depth)
 *     map(row, fun ^^probe(x) ..) # 2 (unchanged - both 1 and 2 are consistent)
 *
 *   Without intent preservation, inner would reset to 1 (first/default).
 *   With intent preservation, inner stays at 2.
 *
 * MECHANISM: The cursor maintains both a full `call_stack` and an `index`.
 * The `index` is the effective cursor depth; `call_stack` may retain deeper
 * call information. When clicking "up" to a shallower sample that already
 * contains the current selection, we keep the deeper stack but lower the
 * index. Use `trimmed_stack(cursor)` to get the effective stack.
 *
 * WHY CURSOR STORES COORDINATES, NOT A SAMPLE REFERENCE
 *
 * The cursor points to a position in the evaluation trace, not to a specific
 * sample. This is because:
 *
 * 1. MULTIPLE PROBES, ONE CURSOR: Many probes share the same cursor. Each
 *    probe shows the sample at its location that matches the cursor's
 *    call stack depth. A direct Sample reference would only work for one probe.
 *
 * 2. SAMPLES ARE EPHEMERAL: Samples are recomputed on every edit. The cursor
 *    must survive across evaluations. Call stacks provide stable coordinates
 *    because they're based on syntax IDs, which persist across edits.
 *
 * 3. INTENT PRESERVATION: Storing (call_stack, index) lets us preserve user
 *    intent when navigating. A shallower click can keep the deeper stack info
 *    so we remember the user's prior selection if they navigate back down.
 *
 * RELATIONSHIP TO Sample.t: Sample.t records a single evaluation moment
 * (value, environment, call_stack, timing). Cursor.t identifies which
 * evaluation moment the user is focused on across all probes. They share
 * the call_stack type as the common coordinate system. */
module Cursor = {
  open OptUtil.Syntax;

  /* Pending focus state for step-into functionality.
   * When set, the system will focus the matching sample
   * after evaluation completes. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type pending_focus = {
    probe_id: Id.t, /* The probe we're stepping into */
    target_stack: call_stack /* The call stack to match */
  };

  /* Cursor.t fields:
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
    call_stack,
    index: int,
    pinned_stack: option(call_stack),
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

  let trimmed_stack = (cursor: t): call_stack =>
    ListUtil.slice(0, cursor.index + 1, cursor.call_stack |> List.rev)
    |> List.rev;

  /* If the cursor is on a call, and the provided call stack is
   * downstream of that call, return how many aps downstream it is */
  let depth_in_indicated_calls_stack =
      (cursor: t, call_stack: call_stack): option(int) => {
    let* cur_ap = cursor.indicated_call;
    let cur_frame: stack_frame = {
      id: cur_ap,
      name: None,
    };
    ListUtil.suffix_at_depth(
      ~eq=equal_stack_frame,
      [cur_frame] @ trimmed_stack(cursor),
      call_stack,
    );
  };

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
    is_before_cursor: int,
  };

  let is_below = ListUtil.suffix_at_depth(~eq=equal_stack_frame);

  let relative_level = (cs1: call_stack, cs2: call_stack): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Unrelated
    };

  let cur_call = (ap_id: option(Id.t), sample: sample): option(call_stack) => {
    let* ap_id = ap_id;
    Some([
      {
        id: ap_id,
        name: None,
      },
      ...sample.call_stack,
    ]);
  };

  /* Returns Some(ap_id) only when cursor is on an application with a variable
     in function position (enabling step-into and pinning). Excludes constructor
     applications and applications with non-variable functions (e.g. lambdas). */
  let cur_var_ap = (info: option(Info.t)): option(Id.t) =>
    switch (info) {
    | Some(
        InfoExp({term: {term: Ap(_, {term: Var(_), _}, _), _} as ap, _}),
      ) =>
      Some(Exp.rep_id(ap))
    | _ => None
    };

  let relation =
      (~trimmed: bool, ~ap_id: option(Id.t), cursor: t, sample: sample)
      : relation => {
    let this = sample.call_stack;
    let cursor_stack = trimmed ? trimmed_stack(cursor) : cursor.call_stack;
    {
      is_call_cursor: equal_call_stack(cursor_stack, this),
      is_more_precise_than_cursor:
        List.length(cursor.call_stack) > List.length(sample.call_stack),
      relative_level_to_cursor: relative_level(cursor_stack, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(ap_id, sample);
        is_below(cur_call, cursor_stack);
      },
      is_below_indicated_call: {
        let* cur_ap = cursor.indicated_call;
        is_below(
          [
            {
              id: cur_ap,
              name: None,
            },
          ]
          @ cursor_stack,
          this,
        );
      },
      is_before_cursor: sample.seq - cursor.seq,
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
    | NotAligned /* Single mode: samples exist but none align with cursor */
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

  /* Filter samples by pinned call stack */
  let filter_by_pin =
      (~ap_id: option(Id.t), ~pinned: option(call_stack), samples: list(t))
      : list(t) =>
    switch (pinned) {
    | Some(pinned_stack) =>
      /* Extract just the Id.t from head of pinned_stack for comparison */
      let pinned_head_id =
        Option.map((f: stack_frame) => f.id, ListUtil.hd_opt(pinned_stack));
      /* Compare by ID only - pinned_stack may have None for function names
       * but actual samples have real names from evaluation */
      let pinned_ids = ids_of_stack(pinned_stack);
      List.filter(
        (sample: t) =>
          pinned_head_id == ap_id
          || ListUtil.is_suffix_of(
               pinned_ids,
               ids_of_stack(sample.call_stack),
             ),
        samples,
      );
    | None => samples
    };

  /* Find index of first sample related to cursor position */
  let first_related_index =
      (
        ~trimmed: bool,
        ~ap_id: option(Id.t),
        cursor: Cursor.t,
        samples: list(t),
      )
      : option(int) => {
    let find = (predicate: Cursor.relation => bool): option(int) =>
      List.find_index(
        (sample: t) =>
          predicate(Cursor.relation(~trimmed, ~ap_id, cursor, sample)),
        samples,
      );
    /* Priority: exact cursor match > direct callee > any related */
    switch (find(rel => rel.is_call_cursor)) {
    | Some(_) as result => result
    | None =>
      switch (find(rel => rel.is_below_indicated_call == Some(0))) {
      | Some(_) as result => result
      | None =>
        let indirect = find(rel => rel.is_below_indicated_call != None);
        indirect == None ? find(Cursor.is_related) : indirect;
      }
    };
  };

  /* Find sample with best call stack suffix match to cursor */
  let best_suffix_match =
      (~cursor_stack: call_stack, samples: list(t)): option(t) =>
    List.fold_left(
      (best: option((t, int)), sample: t) => {
        let score =
          ListUtil.common_suffix_length(
            ~eq=equal_stack_frame,
            cursor_stack,
            sample.call_stack,
          );
        switch (best) {
        | Some((_, best_score)) when best_score >= score => best
        | _ => Some((sample, score))
        };
      },
      None,
      samples,
    )
    |> Option.map(fst);

  /* Find sample closest to cursor-related position */
  let closest_to_cursor =
      (~ap_id: option(Id.t), ~cursor: Cursor.t, samples: list(t))
      : option(t) =>
    switch (samples) {
    | [] => None
    | [first, ..._] =>
      switch (first_related_index(~trimmed=false, ~ap_id, cursor, samples)) {
      | Some(idx) => List.nth_opt(samples, idx)
      | None =>
        switch (
          best_suffix_match(
            ~cursor_stack=Cursor.trimmed_stack(cursor),
            samples,
          )
        ) {
        | Some(_) as result => result
        | None => Some(first)
        }
      }
    };

  /* Check if two samples belong to the same function call */
  let is_same_call = (s1: t, s2: t): bool =>
    switch (List.rev(s2.call_stack), List.rev(s1.call_stack)) {
    | ([], _)
    | (_, []) => false
    | ([f1, ..._], [f2, ..._]) => equal_stack_frame(f1, f2)
    };

  /* Group samples by function call, with indices */
  let group_by_call = (samples: list((int, t))): list(list((int, t))) => {
    let grouped =
      samples
      |> ListUtil.group_consecutive(((_, s1), (_, s2)) =>
           is_same_call(s1, s2)
         )
      |> List.map(List.rev);
    /* Flatten if all groups are singletons */
    List.for_all(g => List.length(g) == 1, grouped)
      ? [List.concat(grouped)] : grouped;
  };

  /* Number and group samples for display */
  let collate = (samples: list(t)): (int, list(list((int, t)))) => {
    let numbered =
      List.mapi((i, s) => (List.length(samples) - i - 1, s), samples);
    (List.length(samples), group_by_call(numbered));
  };

  /* Select samples to display based on cursor position and window mode.
   * Pure function - offset is passed in and new offset returned. */
  let select =
      (
        ~mode: Window.mode,
        ~offset: int,
        ~ap_id: option(Id.t),
        ~pinned: option(call_stack),
        ~cursor: Cursor.t,
        samples: list(t),
      )
      : (list(t), int) => {
    let filtered = filter_by_pin(~ap_id, ~pinned, samples);
    let first_idx =
      first_related_index(~trimmed=false, ~ap_id, cursor, filtered);
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

/* Lightweight capture data for passing through actions.
   In a submodule to avoid type inference issues with Sample.t
   which has overlapping field names. */
module Capture = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    time: float,
    seq: int,
    call_stack,
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
