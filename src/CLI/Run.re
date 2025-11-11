open Language;

let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      ~ty_env=Environment.empty,
      fst(
        Elaborator.elaborate(
          ~probe_unknowns=false,
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );
