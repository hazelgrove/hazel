module T = {
  type t('a) = IdGen.t => ('a, IdGen.t);
  let return = a =>
    fun
    | id_gen => (a, id_gen);
  let map = Monads.MapDefinition.Define_using_bind;
  let bind = (cmd: t('a), f: 'a => t('b)): t('b) =>
    fun
    | id_gen => {
        let (a, id_gen') = cmd(id_gen);
        f(a, id_gen');
      };
};
include T;
include Monads.Make(T);

let next_hole = (): t(MetaVar.t) =>
  fun
  | {next_tile, next_hole} =>
    (next_hole, {next_tile, next_hole: next_hole + 1});

let next_tile = (): t(MetaVar.t) =>
  fun
  | {next_hole, next_tile} =>
    (next_tile, {next_hole, next_tile: next_tile + 1});
