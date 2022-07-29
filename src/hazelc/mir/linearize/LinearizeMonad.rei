module State: {
  [@deriving sexp]
  type t;

  let init: t;

  let next_tmp: t => (string, t);

  let next_tmp_named: (string, t) => (string, t);

  let next_label: t => (AnfLabel.t, t);
};

include Monads.MONAD with type t('a) = State.t => (State.t, 'a);

let get: t(State.t);
let put: State.t => t(unit);

let sequence: list(t('a)) => t(list('a));

let init: State.t;

let next_tmp: t(string);
let next_tmp_named: string => t(string);
let next_label: t(AnfLabel.t);
