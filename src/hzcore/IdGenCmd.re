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
  | {next_hole, _} as id_gen => (
      next_hole,
      {...id_gen, next_hole: next_hole + 1},
    );

let next_err = (): t(MetaVar.t) =>
  fun
  | {next_err, _} as id_gen => (
      next_err,
      {...id_gen, next_err: next_err + 1},
    );

let next_tile = (): t(MetaVar.t) =>
  fun
  | {next_tile, _} as id_gen => (
      next_tile,
      {...id_gen, next_tile: next_tile + 1},
    );

let sequence = (cmds: list(t('a))): t(list('a)) =>
  cmds
  |> List.rev
  |> List.fold_right(zip((x, xs) => [x, ...xs]), return([]))
  |> map(List.rev);
