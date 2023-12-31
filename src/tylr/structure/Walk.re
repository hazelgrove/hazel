// module Eq = {
//   type t('token, 'cell) = Chain.t('cell, 'token);
//   // [input] {output}
//   // input: [(] [)]
//   // output: {<>}

//   type t'('token, 'cell) = Chain.t('token, 'cell);
//   // [input] {output}
//   // input: [(] [)]
//   // output: { (<>) }
// };

// module Neq = {
//   type t('token, 'cell) = {
//     top: option(Terr.t('token, 'cell)),
//     mid: option(Terr.t('token, 'cell)),
//     bot: Eq.t('token, 'cell),
//   };
// };

// module Eq = {
//   type t('step) = list('step);
// };
// module Neq = {
//   type t('step) = Chain.t(Eq.t('step), unit);
// };
// type t('step) =
//   | Eq(Eq.t('step))
//   | Neq(Neq.t('step));

type t('step) = Chain.t(list('step), unit);
