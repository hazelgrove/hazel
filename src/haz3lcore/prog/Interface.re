module CoreStatics = Statics;

module Statics = {
  let mk_map' =
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
    core.statics ? mk_map'(exp) : Id.Map.empty;

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
