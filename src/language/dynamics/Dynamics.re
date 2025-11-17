open Util;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * sample gathering for probe projectors */

/* Intercepts a probe form and adds in static semantic information
 * to guide dynamic information gathering  */
let instrument_exp = (m: StaticsBase.Map.t, id: Id.t, _: Probe.t): Probe.t => {
  refs: StaticsBase.Map.refs_in(m, id),
};
let instrument_pat = (m: Statics.Map.t, id: Id.t, _: Probe.t): Probe.t => {
  refs: Statics.Map.bound_in(m, id),
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
  /* Type instantiations recorded during evaluation, indexed by the
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
      (pinned_call: option(list(Id.t)), closures: list('a)): list('a) =>
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
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    dyn_cursor: DynCursor.t,
  };

  let init = {
    samples: [],
    dyn_cursor: DynCursor.init,
  };

  let is_in = (di: t): option(Sample.t) =>
    List.find_opt(
      (sample: Sample.t) =>
        DynCursor.trimmed_stack(di.dyn_cursor) == sample.call_stack,
      di.samples,
    );

  let first_cursor_sample = (ap_id: option(Id.t), di: t): option(Sample.t) => {
    let find_cursor =
      List.find_opt(
        sample =>
          DynCursor.relation(~trimmed=true, ~ap_id, di.dyn_cursor, sample).
            is_call_cursor,
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
  type_inst_map: TypeInstMap.t,
  test_results: TestResults.t,
};
