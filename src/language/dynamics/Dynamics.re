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
      (sample_focus: Sample.Focus.t, closures: list(TypeInstantiation.t))
      : list(TypeInstantiation.t) =>
    switch (sample_focus.pinned_stack) {
    | Some(pinned_stack) =>
      List.filter(
        (closure: TypeInstantiation.t) =>
          ListUtil.is_suffix_of(
            Sample.ids_of_stack(pinned_stack),
            closure.call_stack,
          ),
        closures,
      )
    | None => closures
    };

  let filter_by_focus = (sample_focus: Sample.Focus.t, map: t): t =>
    Id.Map.map(
      closures => filter_type_instantiations_by_pin(sample_focus, closures),
      map,
    );
};

module Info = {
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    sample_focus: Sample.Focus.t,
  };

  let init = {
    samples: [],
    sample_focus: Sample.Focus.init,
  };

  let is_in = (di: t): option(Sample.t) => {
    let cursor_ids =
      Sample.ids_of_stack(Sample.Focus.effective_stack(di.sample_focus));
    List.find_opt(
      (sample: Sample.t) =>
        Sample.ids_of_stack(sample.call_stack) == cursor_ids,
      di.samples,
    );
  };

  /* Find the sample most aligned with the cursor's call path.
   * Uses the same suffix-first principle as Selection.most_aligned_index
   * but returns the sample directly. */
  let most_aligned_sample = (ap_id: option(Id.t), di: t): option(Sample.t) =>
    Sample.Selection.most_aligned_sample(
      ~ap_id,
      ~cursor=di.sample_focus,
      di.samples,
    );
};

module Map = {
  /* Just a wrapping around the Probe map (for now) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Sample.Map.t;
  let empty: t = Sample.Map.empty;
  let mk: t => t = Fun.id;
  let lookup = Sample.Map.lookup;

  /* Apply pin filtering to all probes in the map, using the centralized
   * Sample.Selection.filter_by_pin helper so filtering matches the
   * semantics used by probe sample selection. */
  let filter_by_focus = (focus: Sample.Focus.t, map: t): t =>
    Id.Map.mapi(
      (ap_id, samples) =>
        Sample.Selection.filter_by_pin(
          ~ap_id=Some(ap_id),
          ~pinned=focus.pinned_stack,
          samples,
        ),
      map,
    );
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

let filter_by_focus = (sample_focus: Sample.Focus.t, dyn: t): t => {
  {
    probe_map: Map.filter_by_focus(sample_focus, dyn.probe_map),
    type_inst_map:
      TypeInstMap.filter_by_focus(sample_focus, dyn.type_inst_map),
    test_results: dyn.test_results,
    theorems: dyn.theorems,
  };
};
