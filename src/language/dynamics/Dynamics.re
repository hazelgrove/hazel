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
    let elide = (env: Environment.t(Exp.t), d: DHExp.t) =>
      switch ((d |> DHExp.strip_ascriptions).term) {
      | Fun(_)
      | FixF(_)
      | Closure(_) => Opaque
      | _ =>
        Val(d |> DHExp.strip_ascriptions |> Exp.substitute_closures(env))
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

  /* A probe closure records an elided value and environment,
   * in the above senses, along with a `stack` which records
   * partial information about the execution trace prior to
   * the creation of the closure */
  module Closure = {
    [@deriving (show({with_path: false}), sexp, yojson)]
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
          env: Environment.t(Exp.t),
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
  let instrument_exp = (m: StaticsBase.Map.t, id: Id.t, _: Probe.t): Probe.t => {
    refs: StaticsBase.Map.refs_in(m, id),
  };

  let instrument_pat = (m: StaticsBase.Map.t, id: Id.t, _: Probe.t): Probe.t => {
    refs: StaticsBase.Map.bound_in(m, id),
  };
};

module TypeInstantiation = {
  /* A type instantiation records when a type variable is instantiated
   * with a concrete type during type application evaluation */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    tpat_id: Id.t, /* ID of the type pattern */
    type_var: string, /* Variable name (e.g., "a") */
    instantiated_type: Typ.t, /* The concrete type (e.g., String) */
    call_stack: list(Id.t), /* Call stack at instantiation time */
    time: float /* Timestamp */
  };
};

module TypeInstMap = {
  /* Type applications recorded during evaluation, indexed by the
   * TPat ids of the type parameters */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(list(TypeInstantiation.t));

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;

  let extend = (id, inst: TypeInstantiation.t, map: t) => {
    Id.Map.update(
      id,
      opt =>
        switch (opt) {
        | Some(a) => Some(a @ [inst])
        | None => Some([inst])
        },
      map,
    );
  };
  let filter_type_instantiations_by_pin =
      (
        pinned_call: option(list(Id.t)),
        closures: list(TypeInstantiation.t),
      )
      : list(TypeInstantiation.t) =>
    switch (pinned_call) {
    | Some(pinned_stack) =>
      List.filter(
        (closure: TypeInstantiation.t) =>
          ListUtil.is_suffix_of(pinned_stack, closure.call_stack),
        closures,
      )
    | None => closures
    };

  let filter_all_by_pin = (pinned_call: option(list(Id.t)), map: t): t =>
    Id.Map.map(
      closures => filter_type_instantiations_by_pin(pinned_call, closures),
      map,
    );
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

  /* Filter closures based on pinned call stack.
   * If no call is pinned, returns all closures.
   * If a call is pinned, returns only closures where the pinned
   * call stack is a suffix of the closure's call stack. */
  let filter_closures_by_pin =
      (pinned_call: option(list(Id.t)), closures: list(Probe.Closure.t))
      : list(Probe.Closure.t) =>
    switch (pinned_call) {
    | Some(pinned_stack) =>
      List.filter(
        (closure: Probe.Closure.t) =>
          ListUtil.is_suffix_of(pinned_stack, closure.call_stack),
        closures,
      )
    | None => closures
    };

  /* Apply closure filtering to all probes in the map */
  let filter_all_by_pin = (pinned_call: option(list(Id.t)), map: t): t =>
    Id.Map.map(
      closures => filter_closures_by_pin(pinned_call, closures),
      map,
    );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  probe_map: Probe.Map.t,
  type_inst_map: TypeInstMap.t,
  test_results: TestResults.t,
};
