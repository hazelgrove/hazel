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
  frame: option(CallStack.frame),
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
      ~frame: option(CallStack.frame)=None,
      ~step_start: int,
      ~step_end: int,
      syntax_id: Id.t,
      value: DHExp.t,
      env: Environment.t(Exp.t),
      stack: CallStack.t,
      spec: capture_spec,
    )
    : t => {
  /* content-derived id; cheap discriminators first so hash_param's bounded
   * traversal can't collide deep stacks sharing a prefix */
  id: Hashtbl.hash_param(64, 256, (List.length(stack), syntax_id, stack)),
  syntax_id,
  value,
  env: Env.filter(env, spec.refs),
  call_stack: stack,
  args,
  frame,
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

/* Two orderings: RAW (newest-first; `extend` builds, `lookup`/`fold` reverse
 * on read) and FINALIZED (`finalize` converts once for display). */
module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(list(sample));

  let empty = Id.Map.empty;

  let lookup = (id, map) =>
    Id.Map.find_opt(id, map) |> Option.map(List.rev);

  let fold = (f, map: t, init) =>
    Id.Map.fold(
      (id, samples, acc) => f(id, List.rev(samples), acc),
      map,
      init,
    );

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

  let finalize = (map: t): t => Id.Map.map(List.rev, map);

  /* Ascription dominance: a non-empty call_stack sample is dominated by
   * an existing empty call_stack sample for the same syntax_id. Guards
   * against duplicates from Asc distribution through typed functions,
   * where inner values get re-evaluated at deeper call stacks (the
   * different-stack smear; the same-stack flavor is prevented at source
   * by span suppression in Evaluator.eval_3). Shared by the inline
   * minting path and the batch fold — both go through
   * ObsTrace.fold_step. */
  let dominated = (sample: sample, map: t): bool =>
    switch (Id.Map.find_opt(sample.syntax_id, map)) {
    | Some(existing) =>
      List.exists(
        (s: sample) => sample.call_stack != [] && s.call_stack == [],
        existing,
      )
    | None => false
    };
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
/* A reference to an observation span (equivalently: to a sample — every
 * sample IS a span close). Identity, not projection: the probed syntax id
 * plus the call-stack instance it evaluated at. Edit-stable for the same
 * reason SampleLength's content hash is: built from syntax ids, so it
 * re-matches automatically across whole-program re-evaluations, and dies
 * (drops) when no counterpart exists. Focus stores these alongside the
 * legacy coordinate projections, which derive from them and are slated
 * for deletion once consumers migrate. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type span_ref = {
  probe_id: Id.t,
  stack: CallStack.t,
  /* Start step of the referenced span. Within a run (probe_id, stack)
   * practically names one span: iterations of map/fold bodies differ in
   * stack depth, since both are recursive. `opened` pins the exact
   * instance anyway, and lets by_ref recover it by step when stack ids
   * change across runs. None for call pins, which name the call by its
   * stack (resolves to the first span at that stack). */
  opened: option(int),
};

let ref_of_sample = (s: t): span_ref => {
  probe_id: s.syntax_id,
  stack: s.call_stack,
  opened: Some(s.step_start),
};

let ref_matches = (r: span_ref, s: t): bool =>
  r.probe_id == s.syntax_id
  && CallStack.ids_of_stack(r.stack) == CallStack.ids_of_stack(s.call_stack)
  && (
    switch (r.opened) {
    | None => true
    | Some(o) => o == s.step_start
    }
  );

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
    /* Span references: the identities the coordinates above are
     * projections of. anchor = the focused sample; pinned_span = the
     * pinned sample. Populated wherever the actual sample is in hand;
     * Selection.most_aligned_index reads them first. */
    anchor: option(span_ref),
    pinned_span: option(span_ref),
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
    anchor: None,
    pinned_span: None,
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
  /* Pin-gated "strict" reachability for ArrowUp/Down probe navigation.
   *
   * Given a cursor and the pin-filtered samples of a candidate probe,
   * returns true iff some sample of that probe satisfies:
   *   (a) sample.call_stack equals cursor.effective_stack, OR
   *   (b) the target's enclosing fn_def_id differs from the cursor's
   *       AND the two stacks are related (one is a suffix of the other).
   *
   * See ProbeProj.is_cursor_aligned for the full design rationale.
   * Pure function, exposed so tests can assert reachability directly. */
  let is_reachable_pinned =
      (~cursor: Focus.t, target_samples: list(sample)): bool => {
    let effective = Focus.effective_stack(cursor);
    let fn_of_innermost = (stack: CallStack.t): option(Id.t) =>
      switch (stack) {
      | [] => None
      | [frame, ..._] => frame.fn_def_id
      };
    let cursor_fn = fn_of_innermost(effective);
    let target_fn =
      switch (target_samples) {
      | [] => None
      | [s, ..._] => fn_of_innermost(s.call_stack)
      };
    List.exists(
      (sample: sample) =>
        if (CallStack.equal(sample.call_stack, effective)) {
          true;
              /* rule (a) */
        } else if (target_fn != cursor_fn) {
          /* rule (b) */
          ListUtil.is_suffix_of(
            ~eq=CallStack.equal_frame,
            sample.call_stack,
            effective,
          )
          || ListUtil.is_suffix_of(
               ~eq=CallStack.equal_frame,
               effective,
               sample.call_stack,
             );
        } else {
          false;
        },
      target_samples,
    );
  };

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
        ~pinned_interval: option((int, int))=None,
        samples: list(t),
      )
      : list(t) => {
    let samples = List.filter((s: t) => s.origin != Print, samples);
    switch (pinned) {
    | Some(_) when pinned_interval != None =>
      /* Pinned-interval semantics, three rules:
       * 1. The pinned probe itself shows ALL its samples (escape) —
       *    other instances of the pinned call stay browsable.
       * 2. Other probes keep samples whose span lies WITHIN the pinned
       *    span — "observed during the pinned evaluation". Total (no
       *    stack-id degeneracy) and correct for pinning any span:
       *    a call pin keeps its dynamic extent, a leaf pin only its
       *    instant.
       * 3. Call probes additionally keep samples whose span CONTAINS
       *    the pinned span — dynamic ancestors, i.e. the derivation
       *    spine above the pin. Interval generalization of the legacy
       *    breadcrumb compromise (calls-only keeps it a spine, not
       *    every enclosing expression). Whether "above" belongs in
       *    chips at all or only in the focus bar is an open design
       *    question — this rule is the one to delete if the latter. */
      let (ps, pe) = Option.get(pinned_interval);
      let is_pinned_probe =
        switch (
          Option.bind(pinned, pinned_stack => ListUtil.hd_opt(pinned_stack)),
          ap_id,
        ) {
        | (Some(head), Some(ap)) => head.id == ap
        | _ => false
        };
      List.filter(
        (sample: t) =>
          is_pinned_probe
          || sample.step_start >= ps
          && sample.step_end <= pe
          || ap_id != None
          && sample.step_start <= ps
          && sample.step_end >= pe,
        samples,
      );
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

  /* Lines that run below the end of the sightline have no sample on it,
   * so something has to choose which of their calls to show, and the
   * choice must be made for all of them together: chosen line by line,
   * neighbouring lines show different calls (both branches of an if at
   * once, a recursive helper's arguments from its first call beside its
   * result from its last).
   *
   * The coordinated choice, made once from every probe's samples: below
   * the sightline, show the first call of each function in execution
   * order (for recursion, the outermost), and only inside calls that are
   * themselves shown. A call of
   * a function already shown, on the sightline or by an earlier call,
   * shows nothing, nor does anything beneath it. Frames with no probed
   * samples (library functions like map, unprobed helpers) pass through.
   *
   * Returns the chosen sample of each probe the choice covers. */
  let coordinated_below =
      (~root: CallStack.t, all: Id.Map.t(list(t))): Id.Map.t(span_ref) => {
    let ids = CallStack.ids_of_stack;
    let root_ids = ids(root);
    let n_root = List.length(root_ids);
    let shown: Hashtbl.t(Id.t, unit) = Hashtbl.create(64);
    /* instance (a call, as its stack's ids) → its samples, in eval order */
    let instances: Hashtbl.t(list(Id.t), list(t)) = Hashtbl.create(64);
    Id.Map.iter(
      (probe_id, samples) =>
        List.iter(
          (s: t) =>
            if (s.origin != Print) {
              let s_ids = ids(s.call_stack);
              let n = List.length(s_ids);
              if (n <= n_root) {
                if (ListUtil.is_suffix_of(s_ids, root_ids)) {
                  Hashtbl.replace(shown, probe_id, ());
                };
              } else if (ListUtil.is_suffix_of(root_ids, s_ids)) {
                let prev =
                  Hashtbl.find_opt(instances, s_ids)
                  |> Option.value(~default=[]);
                Hashtbl.replace(instances, s_ids, [s, ...prev]);
              };
            },
          samples,
        ),
      all,
    );
    /* Execution order: a call's key is the first step anything beneath it
     * ran, so a call sorts before the calls it makes (ties: shallower
     * first), and separate calls (one test's run, then the next) in the
     * order they ran. */
    let entered: Hashtbl.t(list(Id.t), int) = Hashtbl.create(64);
    Hashtbl.iter(
      (key, samples) => {
        let first =
          List.fold_left(
            (m, s: t) => min(m, s.step_start),
            max_int,
            samples,
          );
        let rec up = (k: list(Id.t)) =>
          if (List.length(k) > n_root) {
            switch (Hashtbl.find_opt(entered, k)) {
            | Some(m) when m <= first => ()
            | _ => Hashtbl.replace(entered, k, first)
            };
            switch (k) {
            | [_, ...parent] => up(parent)
            | [] => ()
            };
          };
        up(key);
      },
      instances,
    );
    let ordered =
      Hashtbl.fold(
        (key, rev_samples, acc) =>
          [
            (
              Hashtbl.find(entered, key),
              List.length(key),
              key,
              List.rev(rev_samples),
            ),
            ...acc,
          ],
        instances,
        [],
      )
      |> List.sort(((e1, d1, _, _), (e2, d2, _, _)) =>
           e1 != e2 ? compare(e1, e2) : compare(d1, d2)
         );
    /* true: shown; false: not shown (its function is shown elsewhere) */
    let status: Hashtbl.t(list(Id.t), bool) = Hashtbl.create(64);
    let rec inside_shown = (key: list(Id.t)): bool =>
      switch (key) {
      | [] => true
      | [_, ...parent] =>
        List.length(parent) <= n_root
        || (
          switch (Hashtbl.find_opt(status, parent)) {
          | Some(shown) => shown
          | None => inside_shown(parent)
          }
        )
      };
    List.fold_left(
      (chosen, (_, _, key, samples)) =>
        if (inside_shown(key)
            && !
                 List.exists(
                   (s: t) => Hashtbl.mem(shown, s.syntax_id),
                   samples,
                 )) {
          Hashtbl.replace(status, key, true);
          List.fold_left(
            (chosen, s: t) =>
              Id.Map.mem(s.syntax_id, chosen)
                ? chosen : Id.Map.add(s.syntax_id, ref_of_sample(s), chosen),
            chosen,
            samples,
          )
          |> (
            chosen => {
              List.iter(
                (s: t) => Hashtbl.replace(shown, s.syntax_id, ()),
                samples,
              );
              chosen;
            }
          );
        } else {
          Hashtbl.replace(status, key, false);
          chosen;
        },
      Id.Map.empty,
      ordered,
    );
  };

  /* One render asks once per probe; the samples and focus are shared. */
  let coordinated_memo:
    ref(option((Id.Map.t(list(t)), CallStack.t, Id.Map.t(span_ref)))) =
    ref(None);
  let coordinated = (~root: CallStack.t, all: Id.Map.t(list(t))) =>
    switch (coordinated_memo^) {
    | Some((a, r, chosen)) when a === all && CallStack.equal(r, root) => chosen
    | _ =>
      let chosen = coordinated_below(~root, all);
      coordinated_memo := Some((all, root, chosen));
      chosen;
    };

  /* How a line with no sample on the sightline picks one.
   * - Coordinated(pick): this probe's sample in the coordinated choice
   *   (coordinated_below), if it has one; otherwise the line shows ⊖.
   * - PerLine: the line's first related sample, chosen on its own. Only
   *   for callers without the whole program's samples. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type below =
    | Coordinated(option(span_ref))
    | PerLine;

  /* This probe's place in the coordinated choice under `cursor`. */
  let below_of =
      (~cursor: Focus.t, ~probe_id: Id.t, all: Id.Map.t(list(t))): below =>
    Coordinated(
      Id.Map.find_opt(probe_id, coordinated(~root=cursor.call_stack, all)),
    );

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
   *   3.  Below the sightline: the coordinated choice (~below); or, for
   *       PerLine callers, any related sample.
   */
  let rec most_aligned_index =
          (
            ~ap_id: option(Id.t),
            ~below: below=PerLine,
            cursor: Focus.t,
            samples: list(t),
          )
          : option(int) => {
    /* Reference-first: if the cursor holds the IDENTITY of one of
     * this probe's samples — the anchored (clicked) sample, or the
     * pinned sample — display it by reference; no matching. The
     * stack-pattern tiers below remain the fallback for probes the
     * cursor holds no reference into (and for refs whose sample no
     * longer exists after an edit — graceful degradation). Fixes the
     * fact-pin misalignment: a pinned recursive call's stack PATTERN
     * matches two distinct spans; its reference names exactly one. */
    let by_ref = (r: option(span_ref)): option(int) =>
      switch (r) {
      | None => None
      | Some(r) =>
        switch (List.find_index(s => ref_matches(r, s), samples)) {
        | Some(_) as hit => hit
        | None =>
          /* Stack ids can change between semantically identical runs
           * (a call site rebuilt by an edit), killing the exact match.
           * The step timeline is identical for semantics-preserving
           * edits, so fall back to the span's start step (unique per
           * probe within a run). Semantic edits shift steps → this also
           * misses → tiers. */
          switch (r.opened) {
          | None => None
          | Some(o) =>
            List.find_index(
              (s: t) => r.probe_id == s.syntax_id && o == s.step_start,
              samples,
            )
          }
        }
      };
    let tiers = () => {
      most_aligned_by_tiers(~ap_id, ~below, ~by_ref, cursor, samples);
    };
    switch (by_ref(cursor.anchor)) {
    | Some(_) as res => res
    | None =>
      switch (by_ref(cursor.pinned_span)) {
      | Some(_) as res => res
      | None => tiers()
      }
    };
  }

  and most_aligned_by_tiers =
      (
        ~ap_id: option(Id.t),
        ~below: below,
        ~by_ref: option(span_ref) => option(int),
        cursor: Focus.t,
        samples: list(t),
      )
      : option(int) => {
    let root_ids = CallStack.ids_of_stack(cursor.call_stack);
    /* on the sightline: at or above its end, the top level included */
    let is_on_sightline = (s: t) =>
      ListUtil.is_suffix_of(CallStack.ids_of_stack(s.call_stack), root_ids);
    let coordinated_pick =
      switch (below) {
      | Coordinated(pick) => by_ref(pick)
      | PerLine => None
      };
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
    /* Pinned: the pin is a declared context, so alignment must be
     * deterministic under it — history-free. Pick the candidate closest
     * to the pin's level (shallowest stack; candidates were already
     * filtered to the pinned span's extent). Without this, tier 1b's
     * where-you've-been scan leaks click history into the pinned view:
     * under recursion's identical frame ids it selects the DEEPEST
     * previously-visited level (the fact-slide "x shows 1 instead of 3"
     * misalignment, first observed on the autofocus branch in April —
     * a side effect of sightline preservation, not a dev behavior).
     * Explicit focus-bar navigation still reaches deeper levels via
     * tier 1a above. */
    let pinned_shallowest = () =>
      switch (cursor.pinned_stack, cursor.pinned_span) {
      | (None, None) => None
      | _ =>
        List.mapi((i, s: t) => (i, s), samples)
        /* off the sightline, only the coordinated choice decides */
        |> List.filter(((_, s)) =>
             switch (below) {
             | Coordinated(_) => is_on_sightline(s)
             | PerLine => true
             }
           )
        |> List.map(((i, s: t)) => (i, List.length(s.call_stack)))
        |> List.fold_left(
             (best, (i, depth)) =>
               switch (best) {
               | Some((_, d)) when d <= depth => best
               | _ => Some((i, depth))
               },
             None,
           )
        |> Option.map(fst)
      };
    /* Tier 1b: suffix match against full sightline (where you've been) */
    let full_match =
      switch (effective_match) {
      | Some(_) => effective_match
      | None =>
        switch (cursor.pinned_stack, cursor.pinned_span) {
        | (None, None) => suffix_scan(cursor.call_stack)
        | _ =>
          /* under a pin, a line below the sightline takes the coordinated
             choice before the pin's shallowest-candidate rule */
          switch (coordinated_pick) {
          | Some(_) as res => res
          | None => pinned_shallowest()
          }
        }
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
            switch (below) {
            | Coordinated(_) =>
              switch (coordinated_pick) {
              | Some(_) as result => result
              | None =>
                /* a sample on the sightline the suffix scans skip: the
                   top level's empty stack. Off it (below, or in another
                   branch), only the coordinated choice decides. */
                List.find_index(is_on_sightline, samples)
              }
            | PerLine =>
              let indirect = find(rel => rel.is_below_indicated_call != None);
              indirect == None ? find(Focus.is_related) : indirect;
            }
          }
        }
      };
    result;
  };

  /* Find sample most aligned with the sightline (returns the sample
   * rather than the index). Used by ArrowUp/Down navigation and probe
   * click alignment. Same suffix-scan logic as most_aligned_index. */
  let most_aligned_sample =
      (
        ~ap_id: option(Id.t),
        ~below: below=PerLine,
        ~cursor: Focus.t,
        samples: list(t),
      )
      : option(t) =>
    switch (samples) {
    | [] => None
    | [first, ..._] =>
      switch (most_aligned_index(~ap_id, ~below, cursor, samples)) {
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
        ~pinned_interval: option((int, int))=None,
        ~below: below=PerLine,
        ~cursor: Focus.t,
        samples: list(t),
      )
      : (list(t), int) => {
    let filtered = filter_by_pin(~ap_id, ~pinned, ~pinned_interval, samples);
    let first_idx = most_aligned_index(~ap_id, ~below, cursor, filtered);
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
    probe_id: Id.t, /* the clicked sample's identity (with call_stack) */
    time: float,
    seq: int,
    call_stack: CallStack.t,
    step_start: int,
    step_end: int,
  };
};

let capture_of_sample = (sample: t): Capture.t => {
  probe_id: sample.syntax_id,
  time: sample.time,
  seq: sample.seq,
  call_stack: sample.call_stack,
  step_start: sample.step_start,
  step_end: sample.step_end,
};
