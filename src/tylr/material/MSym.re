include Sym;

module Base = {
  type t = Sym.t(MLabel.t, MSort.t);
};
include Base;

module Molded = {
  include Molded;
  type t = Molded.t(Base.t);

  let t = ((mlbl, mold): t) => (Sym.T(mlbl), mold);
  let nt = ((msrt, mold): t) => (Sym.NT(msrt), mold);
};
