type t('a) = SpliceInfo.t(UHExp.t) => ('a, SpliceInfo.t(UHExp.t));
let return = x => psi => (x, psi);
let bind = (cmd, f) => psi => {
  let (a, psi') = cmd(psi);
  f(a)(psi'); 
};

let exec = (cmd, psi, u_gen) => {
  let (a, psi) = cmd(psi);
  (a, psi, u_gen);
};
