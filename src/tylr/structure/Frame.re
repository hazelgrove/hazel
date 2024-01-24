open Util;

module Open = {
  type t = (Slope.Dn.t, Slope.Up.t);
  let cons = (~onto: Dir.t, terr: Terr.t, (dn, up)) =>
    switch (onto) {
    | L => ([terr, ...dn], up)
    | R => (dn, [terr, ...up])
    };
  let cat = ((dn', up'), (dn, up)) => Slope.(cat(dn', dn), cat(up', up));
};

module Closed = {
  type t = (Terr.R.t, Terr.L.t);
};
