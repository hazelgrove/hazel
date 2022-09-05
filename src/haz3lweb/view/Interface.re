open Haz3lcore;

exception DoesNotElaborate;
let elaborate = (map, term): DHExp.t =>
  switch (Haz3lcore.Elaborator.uexp_elab(map, term)) {
  | DoesNotElaborate => raise(DoesNotElaborate)
  | Elaborates(d, _, _) => d
  };

exception EvalError(EvaluatorError.t);
exception PostprocessError(EvaluatorPost.error);
let eval =
  Core.Memo.general(
    ~cache_size_bound=1000,
    Evaluator.evaluate(Environment.empty),
  );
let postprocess = (es: EvaluatorState.t, d: DHExp.t) =>
  es
  |> EvaluatorState.with_eig(eig => {
       let ((hii, d), _) =
         switch (EvaluatorPost.postprocess(d, eig)) {
         | d => d
         | exception (EvaluatorPost.Exception(reason)) =>
           raise(PostprocessError(reason))
         };
       ((d, hii), eig);
     });

let evaluate = (d: DHExp.t): ProgramResult.t =>
  switch (eval(d)) {
  | (es, BoxedValue(d)) =>
    let ((d, hii), es) = postprocess(es, d);
    (BoxedValue(d), es, hii);
  | (es, Indet(d)) =>
    let ((d, hii), es) = postprocess(es, d);
    (Indet(d), es, hii);
  | exception (EvaluatorError.Exception(reason)) => raise(EvalError(reason))
  };

let evaluation_result = (map, term): option(DHExp.t) =>
  switch (term |> elaborate(map) |> evaluate) {
  | (result, _, _) => Some(EvaluatorResult.unbox(result))
  };

type test_results = {
  test_map: TestMap.t,
  statuses: list(TestStatus.t),
  descriptions: list(string),
  total: int,
  passing: int,
  failing: int,
  unfinished: int,
};

let mk_results = (~descriptions=[], test_map: TestMap.t): test_results => {
  test_map,
  statuses: test_map |> List.map(r => r |> snd |> TestMap.joint_status),
  descriptions,
  total: TestMap.count(test_map),
  passing: TestMap.count_status(Pass, test_map),
  failing: TestMap.count_status(Fail, test_map),
  unfinished: TestMap.count_status(Indet, test_map),
};

let test_results = (~descriptions=[], map, term): option(test_results) => {
  switch (term |> elaborate(map) |> evaluate) {
  | (_, state, _) =>
    Some(mk_results(~descriptions, EvaluatorState.get_tests(state)))
  };
};
