include Slope;

type t = Slope.t(Piece.t, EMeld.t);

module Dn = {
  include Slope.Dn;

  // L2R: dn slot
  let rec roll = (~slot=Slot.Empty, dn: t) =>
    switch (dn) {
    | [] => slot
    | [hd, ...tl] =>
      let m = EMeld.mk(~l=hd.slot, hd.wald, ~r=slot);
      roll(tl, ~slot=Full(m));
    };
};

module Up = {
  include Slope.Up;

  let rec roll = (~slot=Slot.Empty, up: t) =>
    switch (up) {
    | [] => slot
    | [hd, ...tl] =>
      let m = EMeld.mk(~l=slot, hd.wald, ~r=hd.slot);
      roll(~slot=Full(m), tl);
    };
};
