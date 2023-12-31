// precedence-extended BNF
type t('sym) = Sort.Map.t(Prec.Table.t(Regex.t('sym)));

module Mold = {
  type t('atom) = {
    sort: Sort.t,
    prec: Prec.t,
    rctx: RCtx.t('atom),
  };
};

// module Zipper = {
//   type t('focus, 'atom) = {
//     sort: Sort.t,
//     prec: Prec.t,
//     zipper: Regex.Zipper.t('focus, 'atom),
//   };
// };
