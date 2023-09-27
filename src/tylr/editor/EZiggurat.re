type t = Ziggurat.t(Piece.t, ESlot.t);

let clear = (z: t): list(EWald.t) =>
  ESlope.Up.clear(z.up)
  @ [EWald.clear(z.top)]
  @ ESlope.Dn.clear(z.dn);