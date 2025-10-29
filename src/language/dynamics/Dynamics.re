open Util;
open OptUtil.Syntax;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * closure gathering for probe projectors */

/* Intercepts a probe form and adds in static semantic information
 * to guide dynamic information gathering  */
let instrument_exp = (m: Statics.Map.t, id: Id.t, _: Probe.t): Probe.t => {
  refs: Statics.Map.refs_in(m, id),
};

let instrument_pat = (m: Statics.Map.t, id: Id.t, _: Probe.t): Probe.t => {
  refs: Statics.Map.bound_in(m, id),
};

module SampledEnv = {
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
    | _ => Val(d |> DHExp.strip_ascriptions |> Exp.substitute_closures(env))
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
};

/* A probe sample records an elided value and environment,
 * in the above senses, along with a `stack` which records
 * partial information about the execution trace prior to
 * the creation of the sample */
module Sample = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    id: int, /* Primary ID (unique-ish) */
    syntax_id: Id.t, /* Syntax ID of probed expression */
    value: DHExp.t, /* Value of expression */
    env: SampledEnv.t, /* (Filtered) Environment Values  */
    call_stack: Probe.call_stack, /* Call stacks as ap ids */
    time: float /* Time of evaluatation */
  };

  let mk =
      (
        syntax_id: Id.t,
        value: DHExp.t,
        env: Environment.t(Exp.t),
        call_stack: Probe.call_stack,
        pr: Probe.t,
      ) => {
    /* Below hash provides a coarse-grained identification of
     * samples currently used to keep display-length data between
     * similar runs. May want to alter this or simply used a fresh
     * UUID depending on future desiderata */
    id: Hashtbl.hash((call_stack, syntax_id)),
    syntax_id,
    value,
    env: SampledEnv.filter(env, pr.refs),
    call_stack,
    time: JsUtil.timestamp(),
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
};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    stack: Probe.call_stack,
    index: int,
    pinned_stack: option(Probe.call_stack),
    indicated_call: option(Id.t),
  };

  let init: t = {
    stack: [],
    index: (-1),
    pinned_stack: None,
    indicated_call: None,
  };

  let trimmed_stack = (dyn_cursor: t) =>
    ListUtil.slice(0, dyn_cursor.index + 1, dyn_cursor.stack |> List.rev)
    |> List.rev;

  /* If the dynamic cursor is on a call, and the provided
   * call stack is downstream of that call, return how many
   * aps downstream it is */
  let depth_in_indicated_calls_stack =
      (dyn_cursor: t, call_stack: Probe.call_stack): option(int) => {
    open OptUtil.Syntax;
    let* cur_ap = dyn_cursor.indicated_call;
    ListUtil.suffix_at_depth(
      [cur_ap] @ trimmed_stack(dyn_cursor),
      call_stack,
    );
  };

  type relative_level =
    | Above(int)
    | Below(int)
    | Same
    | Unrelated;

  /* How is the current sample related to the dynamic cursor? */
  type relation = {
    /* Is the current sample at the dynamic cursor? */
    is_call_cursor: bool,
    is_more_precise_than_cursor: bool,
    relative_level_to_cursor: relative_level,
    /* Is the current sample at a call directly above the dynamic cursor? */
    is_call_above_call_cursor: option(int),
    /* Is the current sample below the dynamic cursor, and if so, by how much? */
    is_below_indicated_call: option(int),
    /* Is the current sample a call directly below the dynamic cursor, and if so, by how much? */
  };

  let is_below = ListUtil.suffix_at_depth;

  let relative_level = (cs1, cs2): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Unrelated
    };

  let cur_call = (ap_id: option(Id.t), sample: Sample.t) => {
    let* ap_id = ap_id;
    let dyn = sample.call_stack;
    Some([ap_id, ...dyn]);
  };

  let cur_ap = (info: option(Info.t)) =>
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
      (ap_id: option(Id.t), dyn_cursor: t, sample: Sample.t): relation => {
    let this = sample.call_stack;
    let cursor = trimmed_stack(dyn_cursor);
    {
      is_call_cursor: cursor == this,
      is_more_precise_than_cursor:
        List.length(dyn_cursor.stack) > List.length(sample.call_stack),
      relative_level_to_cursor: relative_level(cursor, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(ap_id, sample);
        is_below(cur_call, cursor);
      },
      is_below_indicated_call: {
        let* cur_ap = dyn_cursor.indicated_call;
        is_below([cur_ap] @ cursor, this);
      },
    };
  };

  let is_related = relation =>
    switch (relation.relative_level_to_cursor) {
    | Above(_)
    | Below(_) => true
    | Same => true
    | Unrelated => false
    };
};

module Info = {
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    dyn_cursor: Cursor.t,
  };

  let init = {
    samples: [],
    dyn_cursor: Cursor.init,
  };

  let is_in = (di: t): option(Sample.t) =>
    //TODO(andrew): maybe should use call_cursor suffix trimmed to index?
    List.find_opt(
      (sample: Sample.t) =>
        Cursor.trimmed_stack(di.dyn_cursor) == sample.call_stack,
      di.samples,
    );

  let first_cursor_sample = (ap_id: option(Id.t), di: t): option(Sample.t) => {
    let find_cursor =
      List.find_opt(
        sample =>
          Cursor.relation(ap_id, di.dyn_cursor, sample).is_call_cursor,
        di.samples,
      );
    switch (find_cursor) {
    | Some(sample) => Some(sample)
    | None => None
    };
  };
};

module Map = {
  /* Just a wrapping around the Probe map (for now) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Sample.Map.t;
  let empty: t = Sample.Map.empty;
  let mk: t => t = Fun.id;
  let lookup = Sample.Map.lookup;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  probe_map: Sample.Map.t,
  test_results: TestResults.t,
};
