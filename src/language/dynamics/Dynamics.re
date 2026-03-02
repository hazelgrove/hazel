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
    sample_cursor: Sample.Cursor.t,
  };

  let init = {
    samples: [],
    sample_cursor: Sample.Cursor.init,
  };

  let is_in = (di: t): option(Sample.t) => {
    let cursor_ids =
      Sample.ids_of_stack(Sample.Cursor.trimmed_stack(di.sample_cursor));
    List.find_opt(
      (sample: Sample.t) =>
        Sample.ids_of_stack(sample.call_stack) == cursor_ids,
      di.samples,
    );
  };

  /* Same full-then-intermediate-then-trimmed priority chain as
   * Sample.Selection.first_related_index (see that function's comment). */
  let first_cursor_sample = (ap_id: option(Id.t), di: t): option(Sample.t) => {
    let find_full =
      List.find_opt(
        sample =>
          Sample.Cursor.relation(
            ~trimmed=false,
            ~ap_id,
            di.sample_cursor,
            sample,
          ).
            is_call_cursor,
        di.samples,
      );
    switch (find_full) {
    | Some(_) as result => result
    | None =>
      /* Intermediate: sample whose stack is a suffix of the full cursor
       * stack. Handles 3+ nesting levels. */
      let cursor_len = List.length(di.sample_cursor.call_stack);
      let intermediate =
        List.fold_left(
          (best: option((Sample.t, int)), sample: Sample.t) => {
            let slen = List.length(sample.call_stack);
            if (slen < cursor_len
                && ListUtil.is_suffix_of(
                     ~eq=Sample.equal_stack_frame,
                     sample.call_stack,
                     di.sample_cursor.call_stack,
                   )) {
              switch (best) {
              | Some((_, best_len)) when best_len >= slen => best
              | _ => Some((sample, slen))
              };
            } else {
              best;
            };
          },
          None,
          di.samples,
        )
        |> Option.map(fst);
      switch (intermediate) {
      | Some(_) as result => result
      | None =>
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
        )
      };
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
