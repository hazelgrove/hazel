open Haz3lcore;
open Sexplib.Std;

exception DoesNotElaborate;
let elaborate = (map, term): DHExp.t =>
  switch (Haz3lcore.Elaborator.uexp_elab(map, term)) {
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

let step =
  Core.Memo.general(~cache_size_bound=1000, (d, ind) =>
    EvaluatorStep.step(
      Environment.empty,
      d,
      ind,
      EvaluatorStep.default_option,
    )
  );

// let step =
//   Core.Memo.general(
//     ~cache_size_bound=1000,
//     EvaluatorStep.step(Environment.empty, _, EvaluatorStep.default_option),
//   );

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
  // {result: BoxedValue(d), record: [], context: [], state: es, hii};
  | (es, Indet(d)) =>
    let ((d, hii), es) = postprocess(es, d);
    (Indet(d), es, hii);
  // {result: Indet(d), record: [], context: [], state: es, hii};
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    //raise(EvalError(reason))
    print_endline("Interface.evaluate EXCEPTION");
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  // {
  //   result: Indet(InvalidText(0, 0, "EXCEPTION")),
  //   record: [],
  //   context: [],
  //   state: EvaluatorState.init,
  //   hii: HoleInstanceInfo.empty,
  // };
  | exception _ =>
    print_endline("Other evaluation exception raised (stack overflow?)");
    (
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  // {
  //   result: Indet(InvalidText(0, 0, "EXCEPTION")),
  //   record: [],
  //   context: [],
  //   state: EvaluatorState.init,
  //   hii: HoleInstanceInfo.empty,
  // };
  };

let step = (d: DHExp.t, ind: int): ProgramResult.t => {
  let (es, d) = step(d, ind);
  switch (d) {
  | Step(d)
  | Pause(d)
  | BoxedValue(d) => (BoxedValue(d), es, HoleInstanceInfo.empty)
  // {
  //   result: BoxedValue(d),
  //   record: [],
  //   context: objs,
  //   state: es,
  //   hii: HoleInstanceInfo.empty,
  // }
  | Indet(d) => (BoxedValue(d), es, HoleInstanceInfo.empty)
  // {
  //   result: Indet(d),
  //   record: [],
  //   context: objs,
  //   state: es,
  //   hii: HoleInstanceInfo.empty,
  // }
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
};

// let step = (obj: EvaluatorStep.EvalObj.t): ProgramResult.t => {
//   let (es, (d, _)) = step(obj);
//   switch (d) {
//   | Step(d)
//   | Pause(d)
//   | BoxedValue(d) => (BoxedValue(d), es, HoleInstanceInfo.empty)
//   // {
//   //   result: BoxedValue(d),
//   //   record: [],
//   //   context: objs,
//   //   state: es,
//   //   hii: HoleInstanceInfo.empty,
//   // }
//   | Indet(d) => (BoxedValue(d), es, HoleInstanceInfo.empty)
//   // {
//   //   result: Indet(d),
//   //   record: [],
//   //   context: objs,
//   //   state: es,
//   //   hii: HoleInstanceInfo.empty,
//   // }
//   | exception (EvaluatorError.Exception(_reason)) =>
//     //HACK(andrew): supress exceptions for release
//     //raise(EvalError(reason))
//     print_endline("Interface.evaluate EXCEPTION");
//     (
//       Indet(InvalidText(0, 0, "EXCEPTION")),
//       EvaluatorState.init,
//       HoleInstanceInfo.empty,
//     );
//   | exception _ =>
//     print_endline("Other evaluation exception raised (stack overflow?)");
//     (
//       Indet(InvalidText(0, 0, "EXCEPTION")),
//       EvaluatorState.init,
//       HoleInstanceInfo.empty,
//     );
//   };
// };

let get_result = (map, term): ProgramResult.t =>
  term |> elaborate(map) |> evaluate;

let get_step = (map, term): ProgramResult.t =>
  term |> elaborate(map) |> evaluate;

let evaluation_result = (map, term): option(DHExp.t) =>
  Some(ProgramResult.get_dhexp(get_result(map, term)));

let evaluation_step = (map, term): option(DHExp.t) =>
  Some(ProgramResult.get_dhexp(get_result(map, term)));

[@deriving (show({with_path: false}), sexp, yojson)]
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
  Some(
    mk_results(
      ~descriptions,
      EvaluatorState.get_tests(
        ProgramResult.get_state(get_result(map, term)),
      ),
    ),
  );
};
