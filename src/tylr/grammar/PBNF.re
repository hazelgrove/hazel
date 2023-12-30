// precedence-extended BNF
type t('atom) = Sort.Map.t(Prec.Table.t(Regex.t('atom)));

module Zipper = {
  type t('focus, 'atom) = {
    sort: Sort.t,
    prec: Prec.t,
    zipper: Regex.Zipper.t('focus, 'atom),
  };
};
