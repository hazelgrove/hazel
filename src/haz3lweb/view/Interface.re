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
let origin_evaluate =
  Core.Memo.general(
    ~cache_size_bound=1000,
    Evaluator.evaluate(Environment.empty),
  );

let fill_resume_evaluate =
  Core.Memo.general(
    ~cache_size_bound=1000,
    FillResume.fill_resume_evaluate(Environment.empty),
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

let evaluate =
    (~d_prev=None, ~d_prev_result=None, d0: DHExp.t): ProgramResult.t => {
  let state =
    switch (d_prev) {
    | Some(d_prev') =>
      d_prev' |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
      FillResume.diff_of_DHExp(d_prev', d0);
    | _ =>
      "None" |> JsUtil.log;
      None;
    };
  switch (d_prev_result) {
  | Some(d_prev_result') =>
    d_prev_result' |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log
  | _ => "None" |> JsUtil.log
  };
  d0 |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
  switch (state) {
  | Some(state) =>
    state
    |> FillResume.FillResumeState.sexp_of_t
    |> Sexplib0.Sexp.to_string
    |> JsUtil.log
  | None => "None" |> JsUtil.log
  };
  let evaluate_result = fill_resume_evaluate(d0, d_prev, d_prev_result);
  switch (evaluate_result) {
  | (_, BoxedValue(d))
  | (_, Indet(d)) =>
    //d0 |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
    //es |> EvaluatorState.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
    d |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log
  };

  switch (evaluate_result) {
  | (es, BoxedValue(de)) =>
    let ((d, hii), es) = postprocess(es, de);
    d |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
    (d0, BoxedValue(de), BoxedValue(d), es, hii);
  | (es, Indet(de)) =>
    let ((d, hii), es) = postprocess(es, de);
    d |> DHExp.sexp_of_t |> Sexplib0.Sexp.to_string |> JsUtil.log;
    (d0, Indet(de), Indet(d), es, hii);
  | exception (EvaluatorError.Exception(_reason)) =>
    //HACK(andrew): supress exceptions for release
    print_endline("Interface.evaluate EXCEPTION:");
    print_endline(
      Sexplib.Sexp.to_string_hum(EvaluatorError.sexp_of_t(_reason)),
    );
    (
      d0,
      Indet(InvalidText(0, 0, "EXCEPTION")),
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  | exception exn =>
    print_endline("Other evaluation exception raised (stack overflow?)");
    Printexc.to_string(exn) |> print_endline;
    (
      d0,
      Indet(InvalidText(0, 0, "EXCEPTION")),
      Indet(InvalidText(0, 0, "EXCEPTION")),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  };
};

let get_result = (map, term): ProgramResult.t =>
  term |> elaborate(map) |> evaluate;

let evaluation_result = (map, term): option(DHExp.t) =>
  switch (get_result(map, term)) {
  | (_, _, result, _, _) => Some(EvaluatorResult.unbox(result))
  };

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
  switch (get_result(map, term)) {
  | (_, _, _, state, _) =>
    Some(mk_results(~descriptions, EvaluatorState.get_tests(state)))
  };
};
