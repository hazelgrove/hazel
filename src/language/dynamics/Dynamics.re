open Util;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * sample gathering for probe projectors */

module Info = {
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    dyn_cursor: Sample.Cursor.t,
  };

  let init = {
    samples: [],
    dyn_cursor: Sample.Cursor.init,
  };

  let is_in = (di: t): option(Sample.t) =>
    List.find_opt(
      (sample: Sample.t) =>
        Sample.Cursor.trimmed_stack(di.dyn_cursor) == sample.call_stack,
      di.samples,
    );

  let first_cursor_sample = (ap_id: option(Id.t), di: t): option(Sample.t) => {
    let find_cursor =
      List.find_opt(
        sample =>
          Sample.Cursor.relation(~trimmed=true, ~ap_id, di.dyn_cursor, sample).
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
  test_results: TestResults.t,
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
};
