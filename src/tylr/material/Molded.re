module Base = {
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let map = (f, m) => {...m, mtrl: f(m)};

module Sort = {
  type t = Base.t(Mtrl.Sort.t);
};
module Label = {
  type t = Base.t(Mtrl.Label.t);

  // let same = (s: t) =>
  //   Mtrl.Sort.eq(s.mtrl, s.mold.)
  // let winner = (l: t, r: t): option(t) => {
  //   open OptUtil.Syntax;
  //   let* () = OptUtil.of_bool(Sort.eq(l.mold.sort, r.mold.sort));
  //   let l_eq = Mtrl.Sort.eq(l.sort)
  //   let (l_eq, r_eq) = Sort.(eq(s_l, m_l.sort), eq(s_r, m_r.sort));
  //   if (l_eq && r_eq) {
  //     Precs.leq(m_l.sort, m_l.prec, m_r.prec)
  //     ? Some(l) : Some(r)
  //   } else if (l_eq) {
  //   }
  // }
};

module Sym = {
  type t = Base.t(Mtrl.Sym.t);
  let t = map(Sym.t);
  let nt = map(Sym.nt);
};
