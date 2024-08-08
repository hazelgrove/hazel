let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

exception DoesNotElaborate;
let elaborate = (~settings: CoreSettings.t, map, term): DHExp.t =>
  switch () {
  | _ when !settings.statics => dh_err("Statics disabled")
  | _ when !settings.dynamics && !settings.elaborate =>
    dh_err("Dynamics & Elaboration disabled")
  | _ =>
    switch (elaborate(map, term)) {
    | DoesNotElaborate => dh_err("Elaboration returns None")
    | Elaborates(d, _, _) => d
    }
  };

let eval_term =
    (
      ~settings: CoreSettings.t,
      ~ctx_init: Ctx.t,
      ~env_init: Environment.t,
      term: Exp.t,
    )
    : ProgramResult.t(ProgramResult.inner) => {
  let info_map = Statics.mk_map_ctx(settings, ctx_init, term);
  let d = elaborate(~settings, info_map, term);
  Evaluator.evaluate(~settings, ~env=env_init, d);
};

let eval_z =
    (
      ~settings: CoreSettings.t,
      ~ctx_init: Ctx.t,
      ~env_init: Environment.t,
      z: Zipper.t,
    )
    : ProgramResult.t(ProgramResult.inner) => {
  let (term, _) = MakeTerm.from_zip_for_sem(z);
  let info_map = Statics.mk_map_ctx(settings, ctx_init, term);
  let d = elaborate(~settings, info_map, term);
  Evaluator.evaluate(~settings, ~env=env_init, d);
};
