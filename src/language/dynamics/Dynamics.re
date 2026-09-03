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
            CallStack.ids_of_stack(pinned_stack),
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

  let is_in = (di: t): option(Sample.t) => {
    let cursor_stack = Sample.Focus.effective_stack(di.sample_focus);
    List.find_opt(
      (sample: Sample.t) => CallStack.equal(sample.call_stack, cursor_stack),
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

/* Package evaluation results for the live typing pass, which re-runs statics
 * with each probed id's observed values in hand (see LiveTyping.re).
 *
 * Sample values are CLOSED here. A runtime function value is a
 * `Closure(env, body)` whose free variables are bound by `env`, but statics
 * discards a closure's env and resolves the body's variables against whatever
 * is in scope at the site being typed. That silently captures a same-named
 * binder: a sample `fun () -> h` taken inside a `fun h -> ...` gets typed as
 * if its `h` were the parameter rather than the `h` it actually closed over,
 * producing wrong live types and spurious live typing errors.
 * `Substitution.in_exp` resolves each closure against its own env (its
 * postcondition is that no closures remain), so the expression statics sees
 * has no free variables left to capture.
 *
 * Unguarded on purpose: substitution walks the value, but so does the statics
 * run that `LiveTyping.refine_typ_with_dynamics` performs on every sample, and
 * any cheap "does this contain a closure?" test would have to walk it too
 * (`Exp.map_term` rebuilds every node, so it is no cheaper than substituting). */
let to_live_typing_map = (dyn: t): LiveTyping.Map.t =>
  LiveTyping.Map.mk(
    Id.Map.map(
      List.map((s: Sample.t): LiveTyping.sample =>
        {exp: Substitution.in_exp(Environment.empty, s.value)}
      ),
      dyn.probe_map,
    ),
    Id.Map.map(
      List.map((inst: TypeInstantiation.t): LiveTyping.type_instantiation =>
        {
          tpat_id: inst.tpat_id,
          type_var: inst.type_var,
          instantiated_type: inst.instantiated_type,
        }
      ),
      dyn.type_inst_map,
    ),
  );
