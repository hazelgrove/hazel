module State: {
  [@deriving sexp]
  type t = {t_gen: TmpVarGen.t};

  let init: t;

  let next_tmp: t => (string, t);

  let next_tmp_named: (string, t) => (string, t);
};

include Monads.MONAD with type t('a) = State.t => (State.t, 'a);

let get: t(State.t);
let put: State.t => t(unit);

let init: State.t;

let next_tmp: t(string);
let next_tmp_named: string => t(string);
