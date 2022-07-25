[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t, EvaluatorState.t);

let get_dhexp = ((d, _, _, _): t) => d;
let get_hole_instance_info = ((_, hii, _, _): t) => hii;
let get_eval_state = ((_, _, _, state): t) => state;

let fast_equal_hii = (hii1, hii2) => {
  let fast_equal_his = (his1, his2) =>
    List.equal(
      ((sigma1, _), (sigma2, _)) =>
        ClosureEnvironment.id_equal(sigma1, sigma2)
        /* Check that variable mappings in ClosureEnvironment are equal */
        && List.equal(
             ((x1, d1), (x2, d2)) => x1 == x2 && DHExp.fast_equal(d1, d2),
             ClosureEnvironment.to_list(sigma1),
             ClosureEnvironment.to_list(sigma2),
           ),
      his1,
      his2,
    );

  MetaVarMap.equal(fast_equal_his, hii1, hii2);
};

let fast_equal = ((_, hii1, r1, _): t, (_, hii2, r2, _): t): bool =>
  fast_equal_hii(hii1, hii2) && EvaluatorResult.fast_equal(r1, r2);
