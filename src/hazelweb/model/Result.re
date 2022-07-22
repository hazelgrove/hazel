[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t, EvaluatorState.t);

let get_dhexp = ((d, _, _, _): t) => d;
let get_hole_closure_info = ((_, hci, _, _): t) => hci;
let get_eval_state = ((_, _, _, state): t) => state;

let fast_equals = ((_, hci1, r1, _): t, (_, hci2, r2, _): t): bool => {
  /* Check that HoleInstanceInstances are equal */
  MetaVarMap.cardinal(hci1) == MetaVarMap.cardinal(hci2)
  && List.for_all2(
       /* Check that all holes are equal */
       ((u1, hcs1), (u2, hcs2)) =>
         u1 == u2
         && List.length(hcs1) == List.length(hcs2)
         && List.for_all2(
              /* Check that all hole closures are equal */
              ((sigma1, _), (sigma2, _)) =>
                ClosureEnvironment.id_of(sigma1)
                == ClosureEnvironment.id_of(sigma2)
                && List.for_all2(
                     /* Check that variable mappings in ClosureEnvironment are equal */
                     ((x1, d1), (x2, d2)) =>
                       x1 == x2 && DHExp.fast_equals(d1, d2),
                     ClosureEnvironment.to_list(sigma1),
                     ClosureEnvironment.to_list(sigma2),
                   ),
              hcs1,
              hcs2,
            ),
       MetaVarMap.bindings(hci1),
       MetaVarMap.bindings(hci2),
     )
  /* Check that r1, r2 are equal */
  && EvaluatorResult.fast_equal(r1, r2);
};
