module Meta = {
  type t = {
    next_hole: MetaVar.t,
    next_tile: MetaVar.t,
  };

  let init = {next_hole: 0, next_tile: 0};
};

module T = {
  type t('a) = Meta.t => ('a, Meta.t);
  let return = a =>
    fun
    | meta => (a, meta);
  let map = Monads.MapDefinition.Define_using_bind;
  let bind = (edit_state, f) =>
    fun
    | meta => {
        let (a, meta') = edit_state(meta);
        f(a, meta');
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
