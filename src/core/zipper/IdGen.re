[@deriving (show({with_path: false}), sexp, yojson)]
type state = Id.t;
//open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type t('a) = state => ('a, state);

let get: t(state) = state => (state, state);
let put: state => t(unit) = (new_state, _old_state) => ((), new_state);

let fresh: t(Id.t) = id => (id, id + 1);

let return: 'a => t('a) = (a, s) => (a, s);

let id: (state, 'a) => ('a, state) = (s, a) => (a, s);

let map: ('a => 'b, t('a)) => t('b) =
  (f, id_gen, id) => {
    let (a, id) = id_gen(id);
    (f(a), id);
  };

let bind: (t('a), 'a => t('b)) => t('b) =
  (id_gen, f, id) => {
    let (a, id) = id_gen(id);
    f(a, id);
  };

module Syntax = {
  let ( let* ) = bind;
  let (let+) = (ig, f) => map(f, ig);
};
