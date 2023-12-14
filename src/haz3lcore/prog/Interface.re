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

let dh_err = (error: string): DHExp.t =>
  InvalidText(Id.invalid, -666, error);

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

exception DoesNotElaborate;
let elaborate = (~settings: CoreSettings.t, map, term): DHExp.t =>
  switch (settings.statics) {
  | false => dh_err("Statics disabled: No elaboration")
  | true =>
    switch (elaborate(map, term)) {
    | DoesNotElaborate => dh_err("Internal error: Elaboration returns None")
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
  let err_wrap = (error): (EvaluatorState.t, EvaluatorResult.t) => (
    EvaluatorState.init,
    Indet(dh_err(error)),
  );
  let result =
    switch () {
    | _ when !settings.statics =>
      err_wrap("Statics disabled: No elaboration or evaluation")
    | _ when !settings.dynamics =>
      err_wrap("Dynamics disabled: No evaluation")
    | _ =>
      try(Evaluator.evaluate(env, d)) {
      | EvaluatorError.Exception(reason) =>
        err_wrap("Internal exception: " ++ EvaluatorError.show(reason))
      | exn => err_wrap("System exception: " ++ Printexc.to_string(exn))
      }
    };
  // TODO(cyrus): disabling post-processing for now, it has bad performance characteristics when you have deeply nested indet cases (and probably other situations) and we aren't using it in the UI for anything
  switch (result) {
  | (es, BoxedValue(_) as r)
  | (es, Indet(_) as r) =>
    // let ((d, hii), es) = postprocess(es, d);
    (r, es)
  };
};

let init = (d: DHExp.t): ProgramResult.t => {
  let es = EvaluatorState.init;
  let env = ClosureEnvironment.of_environment(Builtins.env_init);
  (Indet(Closure(env, d)), es);
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
  | (result, _) => EvaluatorResult.unbox(result)
  };

let eval_u2d = (~settings: CoreSettings.t, map, term): DHExp.t =>
  //NOTE: assumes empty init ctx, env
  term |> elaborate(~settings, map) |> eval_d2d(~settings);

let evaluate_with_history =
    (~settings: CoreSettings.t, map, term): list(DHExp.t) => {
  switch () {
  | _ when !settings.statics => []
  | _ when !settings.dynamics => []
  | _ =>
    term |> elaborate(~settings, map) |> EvaluatorStep.evaluate_with_history
  };
};
