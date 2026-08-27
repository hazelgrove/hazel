open Haz3lcore;
open Language;

/* Shared harness for the generated config slides (Colors, Shortcuts).

   Both are built as terms rather than parsed from committed .hz text, so both
   need the same things pinned: that the built-in source satisfies the type it
   is analyzed against, that the analysis is engaged at all, and — for Colors,
   whose applier reads the evaluated value — that it still evaluates. */

let zipper_of = (source: PersistentZipper.t): Zipper.t =>
  PersistentZipper.unpersist(source, ~root=Exp);

let statics = (~ana: Typ.t, source: PersistentZipper.t) => {
  let term = MakeTerm.from_zip_for_sem(zipper_of(source), ~root=Exp).term;
  Statics.mk(~ana, CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
};

let error_count = (~ana: Typ.t, source: PersistentZipper.t): int =>
  statics(~ana, source) |> fst |> Statics.Map.error_ids |> List.length;

/* The slide's value, as a config applier sees it. */
let evaluate = (~ana: Typ.t, source: PersistentZipper.t): Exp.t => {
  let (_, elab) = statics(~ana, source);
  let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
  result;
};
