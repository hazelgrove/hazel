[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t, EvaluatorState.t);

let get_dhexp = ((d, _, _, _): t) => d;
let get_hole_instance_info = ((_, hii, _, _): t) => hii;
let get_eval_state = ((_, _, _, state): t) => state;

/* FIXME: Clean this up. */
let fast_equals = ((_, hii1, r1, _): t, (_, hii2, r2, _): t): bool => {
  /* Check that HoleInstanceInstances are equal */
  MetaVarMap.cardinal(hii1) == MetaVarMap.cardinal(hii2)
  && List.for_all2(
       /* Check that all holes are equal */
       ((u1, his1), (u2, his2)) =>
         u1 == u2
         && List.length(his1) == List.length(his2)
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
              his1,
              his2,
            ),
       MetaVarMap.bindings(hii1),
       MetaVarMap.bindings(hii2),
     )
  /* Check that r1, r2 are equal */
  && EvaluatorResult.fast_equal(r1, r2);
};
