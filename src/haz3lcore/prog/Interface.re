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
    Evaluator.evaluate(Builtins.builtins_as_environment),
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
  let result = evaluate(d);

  // TODO(cyrus): disabling post-processing for now, it has bad performance characteristics when you have deeply nested indet cases (and probably other situations) and we aren't using it in the UI for anything
  switch (result) {
  | (es, BoxedValue(_) as r) =>
    // let ((d, hii), es) = postprocess(es, d);
    (r, es, HoleInstanceInfo.empty)
  | (es, Indet(_d) as r) =>
    // let ((d, hii), es) = postprocess(es, d);
    (r, es, HoleInstanceInfo.empty)
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    print_endline("Interface.evaluate EXCEPTION:");
    print_endline(
      Sexplib.Sexp.to_string_hum(EvaluatorError.sexp_of_t(_reason)),
    );
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  | exception exn =>
    print_endline("Other evaluation exception raised (stack overflow?)");
    Printexc.to_string(exn) |> print_endline;
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  };
};

let step =
  Core.Memo.general(
    ~cache_size_bound=1000,
    EvaluatorStep.step(Builtins.builtins_as_environment),
  );

let step = (obj: EvaluatorStep.EvalObj.t): ProgramResult.t => {
  let (es, d) = step(obj);
  switch (d) {
  | Step(d)
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

let nop = (d: DHExp.t): ProgramResult.t => {
  let es = EvaluatorState.init;
  (BoxedValue(d), es, HoleInstanceInfo.empty);
};

let decompose =
  Core.Memo.general(
    ~cache_size_bound=1000,
    EvaluatorStep.decompose(Builtins.builtins_as_environment),
  );

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
