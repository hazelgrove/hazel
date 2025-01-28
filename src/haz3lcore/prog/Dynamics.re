open Util;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * closure gathering for probe projectors */

module Probe = {
  module Env = {
    /* To avoid unnecessary de/serialization from evaluation worker,
     * we refrain from retaining certain large un-educational values,
     * such as closures. Which values are made opaque can be modulated
     * via the below `elide` function */
    [@deriving (show({with_path: false}), sexp, yojson)]
    type elided_value =
      | Opaque
      | Val(DHExp.t);

    /* A probe environment entry is a variable binding
     * along with its corresponding elided value */
    [@deriving (show({with_path: false}), sexp, yojson)]
    type entry = {
      binding: Binding.t,
      value: elided_value,
    };

    /* A probe environment is a summarized version of the
     * dynamic environment of the probed expression */
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(entry);

    /* Selectively elide dynamic information not currently
     * being used in the live probe UI, for (putative, unbenchmarked)
     * performance purposes for worker de/serialization */
    let elide = (env: Environment.t, d: DHExp.t) =>
      switch (d.term) {
      | Fun(_)
      | FixF(_) => Opaque
      | _ => Val(d |> DHExp.strip_casts |> Exp.substitute_closures(env))
      };

    let mk_entry = (env: Environment.t, {name, id, _}: Binding.t) =>
      switch (Environment.lookup(env, name)) {
      | Some(d) =>
        let binding = Binding.{name, id};
        Some({binding, value: elide(env, d)});
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
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      closure_id: Id.t, /* Primary ID (unique) */
      syntax_id: Id.t, /* Syntax ID of probed expression */
      value: DHExp.t, /* Value of expression */
      env: Env.t, /* (Filtered) Environment Values  */
      call_stack: Probe.call_stack, /* Call stacks as ap ids */
      assumptions: list(Exp.t) /* Proof assumptions at expression */
    };

    let mk =
        (
          syntax_id: Id.t,
          value: DHExp.t,
          env: Environment.t,
          call_stack: Probe.call_stack,
          pr: Probe.t,
          assumptions: list(Exp.t),
        ) => {
      closure_id: Id.mk(),
      syntax_id,
      value,
      env: Env.filter(env, pr.refs),
      call_stack,
      assumptions,
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
  let instrument_exp =
      (m: Statics.Map.t, id: Id.t, probe_tag: Probe.tag): Probe.tag =>
    switch (probe_tag) {
    | Paren => Paren
    | Probe(_) => Probe({refs: Statics.Map.refs_in(m, id)})
    };

  let instrument_pat =
      (m: Statics.Map.t, id: Id.t, probe_tag: Probe.tag): Probe.tag =>
    switch (probe_tag) {
    | Paren => Paren
    | Probe(_) => Probe({refs: Statics.Map.bound_in(m, id)})
    };
};

module Info = {
  /* Collected closures for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Probe.Closure.t);
};

module Map = {
  /* Just a wrapping around the Probe map (for now) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Probe.Map.t;
  let empty: t = Probe.Map.empty;
  let mk: t => t = Fun.id;
  let lookup = Probe.Map.lookup;
};
