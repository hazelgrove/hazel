[@deriving sexp]
type t = (DHExp.t, HoleClosureInfo.t, EvaluatorResult.t, EvaluatorState.t);

let get_dhexp = ((d, _, _, _): t) => d;
let get_hole_closure_info = ((_, hci, _, _): t) => hci;
let get_eval_state = ((_, _, _, state): t) => state;

let final_dhexp_equals = (r1: EvaluatorResult.t, r2: EvaluatorResult.t): bool => {
  switch (r1, r2) {
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equals(d1, d2)
  | _ => false
  };
};

let fast_equals = ((_, hci1, r1, _): t, (_, hci2, r2, _): t): bool => {
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
                EvalEnv.id_of(sigma1) == EvalEnv.id_of(sigma2)
                && List.for_all2(
                     /* Check that variable mappings in evalenv are equal */
                     ((x1, r1), (x2, r2)) =>
                       x1 == x2 && final_dhexp_equals(r1, r2),
                     EvalEnv.to_list(sigma1),
                     EvalEnv.to_list(sigma2),
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
