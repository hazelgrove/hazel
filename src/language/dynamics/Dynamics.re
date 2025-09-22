open Util;
open OptUtil.Syntax;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * closure gathering for probe projectors */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type call_stack = Probe.call_stack;

module Probe = {
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

    /* Selectively elide dynamic information not currently
     * being used in the live probe UI, for (putative, unbenchmarked)
     * performance purposes for worker de/serialization */
    let elide = (env: Environment.t, d: DHExp.t) =>
      switch ((d |> DHExp.strip_ascriptions).term) {
      | Fun(_)
      | FixF(_)
      | Closure(_) => Opaque
      | _ =>
        Val(d |> DHExp.strip_ascriptions |> Exp.substitute_closures(env))
      };

    let mk_entry = (env: Environment.t, {name, id, _}: Binding.t) =>
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

    let filter = (env: Environment.t, bound_in: Binding.s) =>
      List.filter_map(mk_entry(env), bound_in);
  };

  /* A probe closure records an elided value and environment,
   * in the above senses, along with a `stack` which records
   * partial information about the execution trace prior to
   * the creation of the closure */
  module Closure = {
    [@deriving (show({with_path: false}), sexp, yojson, eq)]
    type t = {
      closure_id: int, /* Primary ID (unique-ish) */
      syntax_id: Id.t, /* Syntax ID of probed expression */
      value: DHExp.t, /* Value of expression */
      env: Env.t, /* (Filtered) Environment Values  */
      call_stack: Probe.call_stack, /* Call stacks as ap ids */
      time: float /* Time of evaluatation */
    };

    let mk =
        (
          syntax_id: Id.t,
          value: DHExp.t,
          env: Environment.t,
          call_stack: Probe.call_stack,
          pr: Probe.t,
        ) => {
      /* Below hash provides a coarse-grained identification of
       * closures currently used to keep display-length data between
       * similar runs. May want to alter this or simply used a fresh
       * UUID depending on future desiderata */
      closure_id: Hashtbl.hash((call_stack, value, pr)),
      syntax_id,
      value,
      env: Env.filter(env, pr.refs),
      call_stack,
      time: JsUtil.timestamp(),
    };
  };

  /* Closures recorded during evaluation, indexed by the
   * syntax ids of their initial expressions */
  module Map = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Id.Map.t(list(Closure.t));

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

  /* Intercepts a probe form and adds in static semantic information
   * to guide dynamic information gathering  */
  let instrument_exp = (m: Statics.Map.t, id: Id.t, _: Probe.t): Probe.t => {
    refs: Statics.Map.refs_in(m, id),
  };

  let instrument_pat = (m: Statics.Map.t, id: Id.t, _: Probe.t): Probe.t => {
    refs: Statics.Map.bound_in(m, id),
  };
};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type call_cursor = {
    stack: call_stack,
    index: int,
  };

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    call_cursor,
    indicated_call: option(Id.t),
    pinned_call: option(call_stack),
  };

  let init: t = {
    call_cursor: {
      stack: [],
      index: (-1),
    },
    indicated_call: None,
    pinned_call: None,
  };

  let trimmed_stack = (call_cursor: call_cursor) =>
    ListUtil.slice(0, call_cursor.index + 1, call_cursor.stack |> List.rev)
    |> List.rev;

  /* If the closure cursor is on a call, and the provided
   * call stack is downstream of that call, return how many
   * aps downstream it is */
  let depth_in_indicated_calls_stack =
      (cc: t, call_stack: call_stack): option(int) => {
    open OptUtil.Syntax;
    let* cur_ap = cc.indicated_call;
    ListUtil.suffix_at_depth(
      [cur_ap] @ trimmed_stack(cc.call_cursor),
      call_stack,
    );
  };

  type relative_level =
    | Above(int)
    | Below(int)
    | Same
    | Unrelated;

  /* How is the current closure related to the closure cursor? */
  type relation = {
    /* Is the current closure the call cursor? */
    is_call_cursor: bool,
    is_more_precise_than_cursor: bool,
    relative_level_to_cursor: relative_level,
    /* Is the current closure a call directly above the call cursor? */
    is_call_above_call_cursor: option(int),
    /* Is the current closure below the call cursor, and if so, by how much? */
    is_below_indicated_call: option(int),
    /* Is the current closure a call directly below the call cursor, and if so, by how much? */
  };

  let is_below = ListUtil.suffix_at_depth;

  let relative_level = (cs1, cs2): relative_level =>
    switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
    | (Some(0), Some(0)) => Same
    | (Some(n), None) => Below(n)
    | (None, Some(n)) => Above(n)
    | (_, _) => Unrelated
    };

  let cur_call = (ap_id: option(Id.t), closure: Probe.Closure.t) => {
    let* ap_id = ap_id;
    let dyn = closure.call_stack;
    Some([ap_id, ...dyn]);
  };

  let relation =
      (ap_id: option(Id.t), cc: t, closure: Probe.Closure.t): relation => {
    let this = closure.call_stack;
    let cursor = trimmed_stack(cc.call_cursor);
    {
      is_call_cursor: cursor == this,
      is_more_precise_than_cursor:
        List.length(cc.call_cursor.stack) > List.length(closure.call_stack),
      relative_level_to_cursor: relative_level(cursor, this),
      is_call_above_call_cursor: {
        let* cur_call = cur_call(ap_id, closure);
        is_below(cur_call, cursor);
      },
      is_below_indicated_call: {
        let* cur_ap = cc.indicated_call;
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
  /* Collected closures for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    closures: list(Probe.Closure.t),
    dyn_cursor: Cursor.t,
  };

  let init = {
    closures: [],
    dyn_cursor: Cursor.init,
  };

  let is_in = (di: t): option(Probe.Closure.t) =>
    //TODO(andrew): maybe should use call_cursor suffix trimmed to index?
    List.find_opt(
      (closure: Probe.Closure.t) =>
        Cursor.trimmed_stack(di.dyn_cursor.call_cursor) == closure.call_stack,
      di.closures,
    );

  let first_cursor_closure =
      (ap_id: option(Id.t), di: t): option(Probe.Closure.t) => {
    let find_cursor =
      List.find_opt(
        closure =>
          Cursor.relation(ap_id, di.dyn_cursor, closure).is_call_cursor,
        di.closures,
      );
    switch (find_cursor) {
    | Some(closure) => Some(closure)
    | None => None
    };
  };
};

module Map = {
  /* Just a wrapping around the Probe map (for now) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Probe.Map.t;
  let empty: t = Probe.Map.empty;
  let mk: t => t = Fun.id;
  let lookup = Probe.Map.lookup;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  probe_map: Probe.Map.t,
  test_results: TestResults.t,
};
