[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  result: EvaluatorResult.t,
  state: EvaluatorState.t,
  hii: HoleInstanceInfo.t,
  elab: DHExp.t,
};

/* A dummy value for quick error passthrough */
let init = (err: string): t => {
  result: Indet(BoundVar(err)),
  state: EvaluatorState.init,
  hii: HoleInstanceInfo.empty,
  elab: BoundVar(err),
};

let get_dhexp = (r: t) => EvaluatorResult.unbox(r.result);
let get_state = (r: t) => r.state;
let get_hii = (r: t) => r.hii;
let get_elab = (r: t) => r.elab;

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

let fast_equal = (r1: t, r2: t): bool =>
  fast_equal_hii(r1.hii, r2.hii)
  && EvaluatorResult.fast_equal(r1.result, r2.result);
