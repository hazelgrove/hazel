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
    let elide = (env: ClosureEnvironment.t, d: DHExp.t) =>
      switch (d.term) {
      | Fun(_)
      | FixF(_) => Opaque
      | _ =>
        Val(
          d
          |> DHExp.strip_casts
          |> Exp.substitute_closures(ClosureEnvironment.map_of(env)),
        )
      };

    let mk_entry = (env: ClosureEnvironment.t, {name, id, _}: Binding.t) =>
      switch (ClosureEnvironment.lookup(env, name)) {
      | Some(d) => {
          binding: {
            name,
            id,
          },
          value: elide(env, d),
        }
      | None => failwith("Probe: variable not found in environment")
      };

    let mk = (env: ClosureEnvironment.t, refs: Binding.s) =>
      List.map(mk_entry(env), refs);
  };

  /* A probe closure records an elided value and environment,
   * in the above senses, along with a `stack` which records
   * partial information about the execution trace prior to
   * the creation of the closure */
  module Closure = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      closure_id: Id.t, /* Primary ID (Unique) */
      value: DHExp.t,
      env: Env.t,
      stack: Probe.stack,
    };

    let mk = (value: DHExp.t, env: ClosureEnvironment.t, pr: Probe.t) => {
      closure_id: Id.mk(),
      value,
      stack: ClosureEnvironment.stack_of(env),
      env: Env.mk(env, pr.refs),
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
  let instrument =
      (m: Statics.Map.t, id: Id.t, probe_tag: Probe.tag): Probe.tag =>
    switch (probe_tag) {
    | Paren => Paren
    | Probe(_) =>
      Probe({
        refs: Statics.Map.refs_in(m, id),
        stem: Statics.Map.enclosing_abstractions(m, id),
      })
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
