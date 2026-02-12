open Util;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * sample gathering for probe projectors */

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
      (sample_cursor: Sample.Cursor.t, closures: list(TypeInstantiation.t))
      : list(TypeInstantiation.t) =>
    switch (sample_cursor.pinned_stack) {
    | Some(pinned_stack) =>
      List.filter(
        (closure: TypeInstantiation.t) =>
          ListUtil.is_suffix_of(pinned_stack, closure.call_stack),
        closures,
      )
    | None => closures
    };

  let filter_by_cursor = (sample_cursor: Sample.Cursor.t, map: t): t =>
    Id.Map.map(
      closures => filter_type_instantiations_by_pin(sample_cursor, closures),
      map,
    );
};

module Info = {
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    sample_cursor: Sample.Cursor.t,
  };

  let init = {
    samples: [],
    sample_cursor: Sample.Cursor.init,
  };

  let is_in = (di: t): option(Sample.t) =>
    List.find_opt(
      (sample: Sample.t) =>
        Sample.Cursor.trimmed_stack(di.sample_cursor) == sample.call_stack,
      di.samples,
    );

  let first_cursor_sample = (ap_id: option(Id.t), di: t): option(Sample.t) => {
    let find_cursor =
      List.find_opt(
        sample =>
          Sample.Cursor.relation(
            ~trimmed=true,
            ~ap_id,
            di.sample_cursor,
            sample,
          ).
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

  /* Filter samples based on pinned call stack.
   * If no call is pinned, returns all samples.
   * If a call is pinned, returns only samples where the pinned
   * call stack is a suffix of the sample's call stack. */
  let filter_samples_by_pin =
      (pinned_call: Sample.Cursor.t, samples: list(Sample.t))
      : list(Sample.t) =>
    switch (pinned_call.pinned_stack) {
    | Some(pinned_stack) =>
      List.filter(
        (sample: Sample.t) =>
          ListUtil.is_suffix_of(pinned_stack, sample.call_stack),
        samples,
      )
    | None => samples
    };

  /* Apply sample filtering to all probes in the map */
  let filter_by_cursor = (pinned_call: Sample.Cursor.t, map: t): t =>
    Id.Map.map(samples => filter_samples_by_pin(pinned_call, samples), map);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  probe_map: Sample.Map.t,
  type_inst_map: TypeInstMap.t,
  test_results: TestResults.t,
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
};

let empty: t = {
  probe_map: Sample.Map.empty,
  type_inst_map: TypeInstMap.empty,
  theorems: [],
  test_results: {
    test_map: [],
    statuses: [],
    hints: [],
    descriptions: [],
    total: 0,
    passing: 0,
    failing: 0,
    unfinished: 0,
  },
};

let filter_by_cursor = (sample_cursor: Sample.Cursor.t, dyn: t): t => {
  {
    probe_map: Map.filter_by_cursor(sample_cursor, dyn.probe_map),
    type_inst_map:
      TypeInstMap.filter_by_cursor(sample_cursor, dyn.type_inst_map),
    test_results: dyn.test_results,
    theorems: dyn.theorems,
  };
};
