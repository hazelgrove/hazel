open Util;

include StateMonad.Make(ElaboratorState);

// type state = ElaboratorState.t;

// type t('a) = StateMonad.t(option('a));

// let some = x => StateMonad.return(Some(x));

// let none = StateMonad.return(None);

// let bind: (t('a), 'a => t('b)) => t('b) =
//   (m, f) =>
//     StateMonad.bind(m, o =>
//       switch (o) {
//       | Some(x) => f(x)
//       | None => none
//       }
//     );

// let map = (m, f) =>
//   StateMonad.map(m, o =>
//     switch (o) {
//     | Some(x) => Some(f(x))
//     | None => None
//     }
//   );

// module Syntax = {
//   let ( let* ) = bind;
//   let (let+) = map;

//   let (>>=) = bind;
//   let (>>|) = map;
// };

// let get: t(state) = s => (s, Some(s));

// let put: state => t(unit) = (x, _) => (x, Some());

// let modify: (state => state) => t(unit) = f => bind(get, s => put(f(s)));

// let modify' = f =>
//   bind(
//     get,
//     s => {
//       let (x, s) = f(s);
//       bind(put(s), _ => some(x));
//     },
//   );

// let sequence = ms => {
//   let rec sequence' = (ms, acc) => {
//     switch (ms) {
//     | [] => acc
//     | [m, ...ms] =>
//       bind(m, x => sequence'(ms, map(acc, acc => [x, ...acc])))
//     };
//   };

//   map(sequence'(ms, some([])), List.rev);
// };

let get_id = map(get, ElaboratorState.get_id);
let put_id = id => modify(ElaboratorState.put_id(id));
let with_id = f => modify'(ElaboratorState.with_id(f));
