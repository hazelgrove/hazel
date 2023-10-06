type t = {
  eval: EvaluatorState.t,
  inj: DHExp.t => DHExp.t,
};

let init = {eval: EvaluatorState.init, inj: x => x};

let get_eval = ({eval, _}) => eval;

let put_eval = (eval, state) => {...state, eval};

let with_eval = (f, state) => {
  let (eval, x) = state |> get_eval |> f;
  (x, state |> put_eval(eval));
};

let get_inj = ({inj, _}) => inj;

let put_inj = (inj, state) => {...state, inj};

let with_inj = (f, state) => {
  let (x, inj) = state |> get_inj |> f;
  (x, state |> put_inj(inj));
};
