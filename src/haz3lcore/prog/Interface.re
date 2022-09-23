exception DoesNotElaborate;
let elaborate = (map, term): DHExp.t =>
  switch (Elaborator.uexp_elab(map, term)) {
  | DoesNotElaborate =>
    print_endline("Interface.elaborate EXCEPTION");
    //HACK(andrew): supress exceptions for release
    //raise(DoesNotElaborate)
    InvalidText(0, 0, "EXCEPTION");
  | Elaborates(d, _, _) => d
  };

exception EvalError(EvaluatorError.t);
exception PostprocessError(EvaluatorPost.error);
let evaluate =
  Core.Memo.general(
    ~cache_size_bound=1000,
    Evaluator.evaluate(Environment.empty),
  );
let postprocess = (es: EvaluatorState.t, d: DHExp.t) => {
  let ((d, hii), es) =
    es
    |> EvaluatorState.with_eig(eig => {
         let ((hii, d), eig) =
           switch (EvaluatorPost.postprocess(d, eig)) {
           | d => d
           | exception (EvaluatorPost.Exception(reason)) =>
             raise(PostprocessError(reason))
           };
         ((d, hii), eig);
       });
  let (tests, es) =
    es
    |> EvaluatorState.with_eig(eig => {
         let (eig, tests) =
           EvaluatorState.get_tests(es)
           |> List.fold_left_map(
                (eig, (k, instance_reports)) => {
                  let (eig, instance_reports) =
                    instance_reports
                    |> List.fold_left_map(
                         (eig, (d, status)) =>
                           switch (EvaluatorPost.postprocess(d, eig)) {
                           | ((_, d), eig) => (eig, (d, status))
                           | exception (EvaluatorPost.Exception(reason)) =>
                             raise(PostprocessError(reason))
                           },
                         eig,
                       );
                  (eig, (k, instance_reports));
                },
                eig,
              );
         (tests, eig);
       });
  ((d, hii), EvaluatorState.put_tests(tests, es));
};

let evaluate = (d: DHExp.t): ProgramResult.t =>
  switch (evaluate(d)) {
  | (es, BoxedValue(d)) =>
    let ((d, hii), es) = postprocess(es, d);
    (BoxedValue(d), es, hii);
  | (es, Indet(d)) =>
    let ((d, hii), es) = postprocess(es, d);
    (Indet(d), es, hii);
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    //raise(EvalError(reason))
    print_endline("Interface.evaluate EXCEPTION");
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  | exception _ =>
    print_endline("Other evaluation exception raised (stack overflow?)");
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  };

let get_result = (map, term): ProgramResult.t =>
  term |> elaborate(map) |> evaluate;

let evaluation_result = (map, term): option(DHExp.t) =>
  switch (get_result(map, term)) {
  | (result, _, _) => Some(EvaluatorResult.unbox(result))
  };

include TestResults;

let test_results = (~descriptions=[], map, term): option(test_results) => {
  switch (get_result(map, term)) {
  | (_, state, _) =>
    Some(mk_results(~descriptions, EvaluatorState.get_tests(state)))
  };
};
