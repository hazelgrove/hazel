module Statics = {
  let mk_map =
    Core.Memo.general(~cache_size_bound=1000, e => {
      Statics.uexp_to_info_map(
        ~ctx=Builtins.ctx_init,
        ~ancestors=[],
        e,
        Id.Map.empty,
      )
      |> snd
    });
  let mk_map = (core: CoreSettings.t, exp) =>
    core.statics ? mk_map(exp) : Id.Map.empty;

  let mk_map_and_info_ctx =
    Core.Memo.general(~cache_size_bound=1000, (ctx, e) => {
      Statics.uexp_to_info_map(~ctx, ~ancestors=[], e, Id.Map.empty)
    });
  let mk_map_and_info_ctx = (core: CoreSettings.t, ctx, exp) =>
    core.statics
      ? {
        let (info, map) = mk_map_and_info_ctx(ctx, exp);
        (Some(info), map);
      }
      : (None, Id.Map.empty);

  let mk_map_ctx =
    Core.Memo.general(~cache_size_bound=1000, (ctx, e) => {
      Statics.uexp_to_info_map(~ctx, ~ancestors=[], e, Id.Map.empty) |> snd
    });
  let mk_map_ctx = (core: CoreSettings.t, ctx, exp) =>
    core.statics ? mk_map_ctx(ctx, exp) : Id.Map.empty;
};

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

exception DoesNotElaborate;
let elaborate = (~settings: CoreSettings.t, map, term): DHExp.t =>
  switch (settings.statics && settings.elaborate) {
  | false => InvalidText(Id.invalid, -666, "Statics disabled: No elaboration")
  | true =>
    switch (elaborate(map, term)) {
    | DoesNotElaborate =>
      let error = "Internal error: Elaboration returns None";
      print_endline("Interface.elaborate: " ++ error);
      InvalidText(Id.invalid, -666, error);
    | Elaborates(d, _, _) => d
    }
  };

let elaborate_editor =
    (~settings: CoreSettings.t, ~ctx_init: Ctx.t, editor: Editor.t): DHExp.t => {
  let (term, _) = MakeTerm.from_zip_for_sem(editor.state.zipper);
  let info_map = Statics.mk_map_ctx(settings, ctx_init, term);
  elaborate(~settings, info_map, term);
};

exception EvalError(EvaluatorError.t);
exception PostprocessError(EvaluatorPost.error);

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
    (~settings: CoreSettings.t, ~env=Builtins.env_init, d: DHExp.t)
    : ProgramResult.t => {
  switch () {
  | _ when !settings.statics =>
    let error = "Statics disabled: No elaboration or evaluation";
    (
      Indet(InvalidText(Id.invalid, -666, error)),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    );
  | _ when !settings.dynamics => (
      Indet(d),
      EvaluatorState.init,
      HoleInstanceInfo.empty,
    )
  | _ =>
    let result =
      try(Evaluator.evaluate(env, d)) {
      | EvaluatorError.Exception(reason) =>
        let error = "Internal exception: " ++ EvaluatorError.show(reason);
        print_endline("Interface.evaluate: " ++ error);
        (EvaluatorState.init, Indet(InvalidText(Id.invalid, -666, error)));
      | exn =>
        let error = "System exception: " ++ Printexc.to_string(exn);
        print_endline("Interface.evaluate: " ++ error);
        (EvaluatorState.init, Indet(InvalidText(Id.invalid, -666, error)));
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
};

let eval_z =
    (
      ~settings: CoreSettings.t,
      ~ctx_init: Ctx.t,
      ~env_init: Environment.t,
      z: Zipper.t,
    )
    : ProgramResult.t => {
  let (term, _) = MakeTerm.from_zip_for_sem(z);
  let info_map = Statics.mk_map_ctx(settings, ctx_init, term);
  let d = elaborate(~settings, info_map, term);
  evaluate(~settings, ~env=env_init, d);
};

let eval_d2d = (~settings: CoreSettings.t, d: DHExp.t): DHExp.t =>
  //NOTE: assumes empty init ctx, env
  switch (evaluate(~settings, d)) {
  | (result, _, _) => EvaluatorResult.unbox(result)
  };

let eval_u2d = (~settings: CoreSettings.t, map, term): DHExp.t =>
  //NOTE: assumes empty init ctx, env
  term |> elaborate(~settings, map) |> eval_d2d(~settings);

include TestResults;
