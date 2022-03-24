[@deriving sexp]
type t = (
  EvaluatorResult.t,
  DHExp.t,
  HoleClosureInfo.t,
  Delta.t,
  EvalState.t,
);

let mk =
    (
      dr_result: EvaluatorResult.t,
      d_unpostprocessed: DHExp.t,
      hci: HoleClosureInfo.t,
      delta: Delta.t,
      es: EvalState.t,
    )
    : t => (
  dr_result,
  d_unpostprocessed,
  hci,
  delta,
  es,
);

let get_result = ((d, _, _, _, _): t) => d;

let get_dhexp = (r: t) =>
  switch (r |> get_result) {
  | BoxedValue(d)
  | Indet(d) => d
  };

let get_hci = ((_, _, hci, _, _): t) => hci;

let get_delta = ((_, _, _, delta, _): t) => delta;

let get_eval_state = ((_, _, _, _, es): t) => es;

let get_unpostprocessed_dhexp = ((_, d_unpostprocessed, _, _, _): t) => d_unpostprocessed;

let final_dhexp_equals = (r1: EvaluatorResult.t, r2: EvaluatorResult.t): bool => {
  switch (r1, r2) {
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equals(d1, d2)
  | _ => false
  };
};

let fast_equals = ((r1, _, hci1, _, _): t, (r2, _, hci2, _, _): t): bool => {
  /* Check that HoleClosureInstances are equal */
  MetaVarMap.cardinal(hci1) == MetaVarMap.cardinal(hci2)
  && List.for_all2(
       /* Check that all holes are equal */
       ((u1, hcs1), (u2, hcs2)) =>
         u1 == u2
         && List.length(hcs1) == List.length(hcs2)
         && List.for_all2(
              /* Check that all hole closures are equal */
              ((sigma1, _), (sigma2, _)) =>
                EvalEnv.id_of_evalenv(sigma1)
                == EvalEnv.id_of_evalenv(sigma2)
                && List.for_all2(
                     /* Check that variable mappings in evalenv are equal */
                     ((x1, r1), (x2, r2)) =>
                       x1 == x2 && final_dhexp_equals(r1, r2),
                     EvalEnv.alist_of_evalenv(sigma1),
                     EvalEnv.alist_of_evalenv(sigma2),
                   ),
              hcs1,
              hcs2,
            ),
       MetaVarMap.bindings(hci1),
       MetaVarMap.bindings(hci2),
     )
  /* Check that r1, r2 are equal */
  && final_dhexp_equals(r1, r2);
};
