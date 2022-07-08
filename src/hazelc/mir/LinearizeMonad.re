module State = {
  [@deriving sexp]
  type t = {t_gen: TmpVarGen.t};

  let init = {t_gen: TmpVarGen.init};

  let next_tmp = state => {
    let (x, t_gen) = TmpVarGen.next(state.t_gen);
    (x, {t_gen: t_gen});
  };

  let next_tmp_named = (x, state) => {
    let (x', t_gen) = TmpVarGen.next_named(x, state.t_gen);
    (x', {t_gen: t_gen});
  };
};

include StateMonad.Make(State);
open Syntax;

let init = State.init;

let next_tmp = {
  let* state = get;
  let (x, state) = State.next_tmp(state);
  let* _ = put(state);
  x |> return;
};

let next_tmp_named = x => {
  let* state = get;
  let (x', state) = State.next_tmp_named(x, state);
  let* _ = put(state);
  x' |> return;
};
