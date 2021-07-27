type t =
  | Op(TileStep.t)
  | Pre(TileStep.t, t')
  | Post(t', TileStep.t)
  | Bin(t', TileStep.t, t')
  | NTup(AltList.t(t, TileStep.t));
