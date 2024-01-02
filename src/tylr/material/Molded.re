module Base = {
  type t('focus) = ('focus, Mold.t);
};
include Base;

module Label = {
  type t = Base.t(MLabel.t);
};
module Sort = {
  type t = Base.t(MSort.t);
};
module Sym = {
  type t = Base.t(MSym.t);
  let t = ((mlbl, mold): t) => (Sym.T(mlbl), mold);
  let nt = ((msrt, mold): t) => (Sym.NT(msrt), mold);
};
