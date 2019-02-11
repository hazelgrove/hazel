  type t('a) = PaletteSpliceInfo.t(UHExp.t) => ('a, PaletteSpliceInfo.t(UHExp.t));
  let return = x => (psi => (x, psi));
  let bind = (x, f) => (psi => 
  { let (a, psi') = x(psi);
    f(a)(psi'); });

