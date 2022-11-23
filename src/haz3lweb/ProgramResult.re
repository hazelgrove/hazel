open Haz3lcore;

module ElaboratorResult = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = DHExp.t;
};

module PostProcessResult = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = EvaluatorResult.t;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (
  ElaboratorResult.t,
  EvaluatorResult.t,
  PostProcessResult.t,
  EvaluatorState.t,
  HoleInstanceInfo.t,
);

let get_elaborator_result = ((r, _, _, _, _): t) => r;
let get_dhexp = ((_, _, r, _, _): t) => EvaluatorResult.unbox(r);
let get_state = ((_, _, _, es, _): t) => es;
let get_hii = ((_, _, _, _, hii): t) => hii;
let get_evaluator_result = ((_, r, _, _, _): t) => EvaluatorResult.unbox(r);
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

let fast_equal =
    ((er1, r1, ppr1, _, hii1): t, (er2, r2, ppr2, _, hii2): t): bool =>
  fast_equal_hii(hii1, hii2)
  && EvaluatorResult.fast_equal(r1, r2)
  && DHExp.fast_equal(er1, er2)
  && EvaluatorResult.fast_equal(ppr1, ppr2);
