open Haz3lcore;
open Test_Editing;
module Fresh = Language.IdTagged.FreshGrammar;

let mk_statics = (z: Zipper.t) =>
  Language.(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z).term,
    )
  );

let apply_actions = (init: string, actions: list(Action.t)): Zipper.t =>
  perform(perform(Zipper.init(), mk(init)), actions);
