open Mir_anf;

module LabelGen = Label_.Gen.Make(Label);
module State = {
  [@deriving sexp]
  type t = {
    t_gen: TmpVarGen.t,
    l_gen: LabelGen.t,
  };

  let init = {t_gen: TmpVarGen.init, l_gen: LabelGen.init};

  let next_tmp = state => {
    let (x, t_gen) = TmpVarGen.next(state.t_gen);
    (x, {...state, t_gen});
  };

  let next_tmp_named = (x, state) => {
    let (x', t_gen) = TmpVarGen.next_named(x, state.t_gen);
    (x', {...state, t_gen});
  };

  let next_label = state => {
    let (l, l_gen) = LabelGen.next(state.l_gen);
    (l, {...state, l_gen});
  };
};

include Util.StateMonad.Make(State);
open Syntax;

let sequence = ms => {
  let rec sequence' = (ms, acc) => {
    switch (ms) {
    | [] => acc
    | [m, ...ms] =>
      let* x = m;
      sequence'(ms, acc >>| (acc => [x, ...acc]));
    };
  };

  sequence'(ms, [] |> return) >>| List.rev;
};

let init = State.init;

let next_tmp = {
  let* (x, state) = get >>| State.next_tmp;
  let* _ = state |> put;
  x |> return;
};

let next_tmp_named = x => {
  let* (x', state) = get >>| State.next_tmp_named(x);
  let* _ = state |> put;
  x' |> return;
};

let next_label = {
  let* (l, state) = get >>| State.next_label;
  let* _ = state |> put;
  l |> return;
};
