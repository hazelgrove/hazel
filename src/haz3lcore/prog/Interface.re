exception DoesNotElaborate;
let elaborate = (map, term): DHExp.t =>
  switch (Elaborator.uexp_elab(map, term)) {
  | DoesNotElaborate =>
    let error = "Internal error: Elaboration returns None";
    print_endline("Interface.elaborate: " ++ error);
    InvalidText(-666, -666, error);
  | Elaborates(d, _, _) => d
  };

exception EvalError(EvaluatorError.t);
exception PostprocessError(EvaluatorPost.error);
let evaluate =
  Core.Memo.general(
    ~cache_size_bound=1000,
    Evaluator.evaluate(Builtins.env_init),
  );

// let postprocess = (es: EvaluatorState.t, d: DHExp.t) => {
//   let ((d, hii), es) =
//     es
//     |> EvaluatorState.with_eig(eig => {
//          let ((hii, d), eig) =
//            switch (EvaluatorPost.postprocess(d, eig)) {
//            | d => d
//            | exception (EvaluatorPost.Exception(reason)) =>
//              raise(PostprocessError(reason))
//            };
//          ((d, hii), eig);
//        });
//   let (tests, es) =
//     es
//     |> EvaluatorState.with_eig(eig => {
//          let (eig, tests) =
//            EvaluatorState.get_tests(es)
//            |> List.fold_left_map(
//                 (eig, (k, instance_reports)) => {
//                   let (eig, instance_reports) =
//                     instance_reports
//                     |> List.fold_left_map(
//                          (eig, (d, status)) =>
//                            switch (EvaluatorPost.postprocess(d, eig)) {
//                            | ((_, d), eig) => (eig, (d, status))
//                            | exception (EvaluatorPost.Exception(reason)) =>
//                              raise(PostprocessError(reason))
//                            },
//                          eig,
//                        );
//                   (eig, (k, instance_reports));
//                 },
//                 eig,
//               );
//          (tests, eig);
//        });
//   ((d, hii), EvaluatorState.put_tests(tests, es));
// };

let evaluate = (d: DHExp.t): ProgramResult.t => {
  let result =
    try(evaluate(d)) {
    | EvaluatorError.Exception(reason) =>
      let error = "Internal exception: " ++ EvaluatorError.show(reason);
      print_endline("Interface.evaluate: " ++ error);
      (EvaluatorState.init, Indet(InvalidText(-666, -666, error)));
    | exn =>
      let error = "System exception: " ++ Printexc.to_string(exn);
      print_endline("Interface.evaluate: " ++ error);
      (EvaluatorState.init, Indet(InvalidText(-666, -666, error)));
    };
  // TODO(cyrus): disabling post-processing for now, it has bad performance characteristics when you have deeply nested indet cases (and probably other situations) and we aren't using it in the UI for anything
  switch (result) {
  | (es, BoxedValue(_) as r) =>
    // let ((d, hii), es) = postprocess(es, d);
    (r, es, HoleInstanceInfo.empty)
  | (es, Indet(_) as r) =>
    // let ((d, hii), es) = postprocess(es, d);
    (r, es, HoleInstanceInfo.empty)
  };
};

let step = Core.Memo.general(~cache_size_bound=1000, EvaluatorStep.step);

let step = (obj: EvaluatorStep.EvalObj.t): ProgramResult.t => {
  let (es, d) = step(obj);
  switch (d) {
  | BoxedValue(d) => (BoxedValue(d), es, HoleInstanceInfo.empty)
  | Indet(d) => (Indet(d), es, HoleInstanceInfo.empty)
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    //raise(EvalError(reason))
    print_endline("Interface.step EXCEPTION");
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
};

let init = (d: DHExp.t): ProgramResult.t => {
  let (env, es) =
    Builtins.env_init
    |> ClosureEnvironment.of_environment
    |> EvaluatorState.with_eig(_, EvaluatorState.init);
  (Indet(Closure(env, d)), es, HoleInstanceInfo.empty);
};

let decompose =
  Core.Memo.general(~cache_size_bound=1000, EvaluatorStep.decompose);

let decompose = (d: DHExp.t): list(EvaluatorStep.EvalObj.t) => {
  let (_, objs) = decompose(d);
  objs;
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
