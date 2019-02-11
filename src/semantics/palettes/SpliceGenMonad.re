type t('a) = SpliceInfo.t(UHExp.t) => ('a, SpliceInfo.t(UHExp.t));
let return = x => psi => (x, psi);
let bind = (x, f) => psi => {
  let (a, psi') = x(psi);
  f(a)(psi'); 
};
