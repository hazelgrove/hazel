open Util;

/* A probe sample records a value and an environment,
 * along with a `stack` which records
 * partial information about the execution trace prior to
 * the creation of the sample */

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
  | Print; /* Println for probes study */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: int, /* Primary ID (unique-ish) */
  syntax_id: Id.t, /* Syntax ID of probed expression */
  value: DHExp.t, /* Value of expression */
  env: Env.t, /* (Filtered) Environment Values  */
  call_stack: Probe.call_stack, /* Call stacks as ap ids */
  time: float, /* Time of evaluatation */
  iter: int, /* A count index of each sample taken */
  origin,
  step_start: int, /* Step count when expression began evaluation */
  step_end: int /* Step count when expression finished evaluation */
};

let iter = ref(0);

let mk =
    (
      ~origin: origin=Probe,
      ~step_start: int,
      ~step_end: int,
      syntax_id: Id.t,
      value: DHExp.t,
      env: Environment.t(Exp.t),
      call_stack: Probe.call_stack,
      pr: Probe.t,
    )
    : t => {
  /* Below hash provides a coarse-grained identification of
   * samples currently used to keep display-length data between
   * similar runs. May want to alter this or simply used a fresh
   * UUID depending on future desiderata */
  id: Hashtbl.hash((call_stack, syntax_id)),
  syntax_id,
  value,
  env: Env.filter(env, pr.refs),
  call_stack,
  time: JsUtil.precise_timestamp(),
  iter: {
    iter := iter^ + 1;
    iter^;
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
  let lookup = Id.Map.find_opt;

  let extend = (id, report, map: t) =>
    Id.Map.update(
      id,
      opt =>
        switch (opt) {
        | Some(a) => Some(a @ [report])
        | None => Some([report])
        },
      map,
    );
};

/* Categorizes why no samples are shown for a probe */
[@deriving (show({with_path: false}), sexp, yojson)]
type empty_status =
  | NoSamplesExist /* Probe was never evaluated */
  | HiddenByPin /* Samples exist but filtered by current pin */
  | NotAligned; /* Single mode: samples exist but none align with cursor */

/* Backwards compatibility alias */
let rm_opaques = Env.remove_opaques;

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
 * been called but have not yet returned. */
module Cursor = {
  open OptUtil.Syntax;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    stack: Probe.call_stack,
    index: int,
    pinned_stack: option(Probe.call_stack),
    indicated_call: option(Id.t),
    time: option(float),
    iter: int,
    step_range: option((int, int)),
  };

  let init: t = {
    stack: [],
    index: (-1),
    pinned_stack: None,
    indicated_call: None,
    time: None,
    iter: 0,
    step_range: None,
  };

  let trimmed_stack = (cursor: t): Probe.call_stack =>
    ListUtil.slice(0, cursor.index + 1, cursor.stack |> List.rev) |> List.rev;

  /* If the cursor is on a call, and the provided call stack is
   * downstream of that call, return how many aps downstream it is */
  let depth_in_indicated_calls_stack =
      (cursor: t, call_stack: Probe.call_stack): option(int) => {
    let* cur_ap = cursor.indicated_call;
    ListUtil.suffix_at_depth([cur_ap] @ trimmed_stack(cursor), call_stack);
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

  let is_below = ListUtil.suffix_at_depth;

  let relative_level =
      (cs1: Probe.call_stack, cs2: Probe.call_stack): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Unrelated
    };

  let cur_call =
      (ap_id: option(Id.t), sample: sample): option(Probe.call_stack) => {
    let* ap_id = ap_id;
    Some([ap_id, ...sample.call_stack]);
  };

  let cur_ap = (info: option(Info.t)): option(Id.t) =>
    switch (info) {
    | Some(
        InfoExp({term: {term: Ap(_, {term: Constructor(_), _}, _), _}, _}),
      )
    | Some(
        InfoExp({
          term:
            {
              term:
                Probe({term: Ap(_, {term: Constructor(_), _}, _), _}, _),
              _,
            },
          _,
        }),
      ) => Option.None
    | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
    | Some(
        InfoExp({term: {term: Probe({term: Ap(_), _} as ap, _), _}, _}),
      ) =>
      Some(Exp.rep_id(ap))
    | _ => None
    };

  let relation =
      (~trimmed: bool, ~ap_id: option(Id.t), cursor: t, sample: sample)
      : relation => {
    let this = sample.call_stack;
    let cursor_stack = trimmed ? trimmed_stack(cursor) : cursor.stack;
    {
      is_call_cursor: cursor_stack == this,
      is_more_precise_than_cursor:
        List.length(cursor.stack) > List.length(sample.call_stack),
      relative_level_to_cursor: relative_level(cursor_stack, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(ap_id, sample);
        is_below(cur_call, cursor_stack);
      },
      is_below_indicated_call: {
        let* cur_ap = cursor.indicated_call;
        is_below([cur_ap] @ cursor_stack, this);
      },
      is_before_cursor: sample.iter - cursor.iter,
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
  /* Filter samples by pinned call stack */
  let filter_by_pin =
      (
        ~ap_id: option(Id.t),
        ~pinned: option(Probe.call_stack),
        samples: list(t),
      )
      : list(t) =>
    switch (pinned) {
    | Some(pinned_stack) =>
      List.filter(
        (sample: t) =>
          ListUtil.hd_opt(pinned_stack) == ap_id
          || ListUtil.is_suffix_of(pinned_stack, sample.call_stack),
        samples,
      )
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
      (~cursor_stack: Probe.call_stack, samples: list(t)): option(t) =>
    List.fold_left(
      (best: option((t, int)), sample: t) => {
        let score =
          ListUtil.common_suffix_length(cursor_stack, sample.call_stack);
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
    | ([f1, ..._], [f2, ..._]) => f1 == f2
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

  /* Determine why no samples are shown.
   * Returns None if samples ARE shown, Some(status) if empty. */
  let get_empty_status =
      (~num_total: int, ~num_shown: int): option(empty_status) =>
    if (num_shown > 0) {
      None;
    } else if (num_total == 0) {
      Some(HiddenByPin);
    } else {
      Some(NotAligned);
    };

  /* Select samples to display based on cursor position and window mode.
   * Pure function - offset is passed in and new offset returned. */
  let select =
      (
        ~mode: Window.mode,
        ~offset: int,
        ~ap_id: option(Id.t),
        ~pinned: option(Probe.call_stack),
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
