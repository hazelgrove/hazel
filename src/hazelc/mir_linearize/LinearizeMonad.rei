open Mir_anf;

module State: {
  [@deriving sexp]
  type t;

  let init: t;

  let next_tmp: t => (Ident.t, t);
  let next_tmp_named: (Ident.t, t) => (Ident.t, t);

  let next_label: t => (Label.t, t);
};

include Util.Monads.MONAD with type t('a) = State.t => (State.t, 'a);

let get: t(State.t);
let put: State.t => t(unit);

let sequence: list(t('a)) => t(list('a));

let init: State.t;

let next_tmp: t(Ident.t);
let next_tmp_named: Ident.t => t(Ident.t);

let next_label: t(Label.t);
