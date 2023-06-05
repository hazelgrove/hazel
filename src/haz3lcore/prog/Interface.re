exception DoesNotElaborate;
let elaborate = (map, term): DHExp.t => {
  print_endline("Interface.elaborate: starting");
  switch (Elaborator.uexp_elab(map, term)) {
  | DoesNotElaborate =>
    print_endline("Interface.elaborate: Elaborate returns None");
    InvalidText(0, 0, "ELAB_ERROR");
  | Elaborates(d, _, _) => d
  };
};

let elaborate_editor = (~ctx_init: Ctx.t, editor: Editor.t): DHExp.t => {
  print_endline(
    "Interface.elaborate_editor: ctx_init:" ++ Ctx.show(ctx_init),
  );
  let (term, _) = MakeTerm.from_zip_for_sem(editor.state.zipper);
  let info_map = Statics.mk_map_ctx(ctx_init, term);
  elaborate(info_map, term);
};

exception EvalError(EvaluatorError.t);
exception PostprocessError(EvaluatorPost.error);
let evaluate = Evaluator.evaluate; /*
  Core.Memo.general(~cache_size_bound=1000, (env, dhexp) =>
    Evaluator.evaluate(env, dhexp)
  );*/

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

let evaluate =
    (~memo=true, ~env=Environment.empty, d: DHExp.t): ProgramResult.t => {
  let result = memo ? evaluate(env, d) : Evaluator.evaluate(env, d);
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
    print_endline("Interface.eval EXCEPTION:");
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

let eval_to_result = (~env=Environment.empty, map, term): ProgramResult.t =>
  term |> elaborate(map) |> evaluate(~env);

let eval_to_dhexp = (map, term): option(DHExp.t) =>
  switch (eval_to_result(map, term)) {
  | (result, _, _) => Some(EvaluatorResult.unbox(result))
  };

include TestResults;
