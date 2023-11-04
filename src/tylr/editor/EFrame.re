module Open = {
  type t = (ESlope.Dn.t, ESlope.Up.t);
};
module Closed = {
  type t = (ETerr.R.t, ETerr.L.t);
};
