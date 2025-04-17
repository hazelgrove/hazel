open Semantics;

let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );
