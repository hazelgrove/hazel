type elem('tile) =
  | Tile('tile)
  | Grout(Grout.t);

type t('tile) = list(elem('tile));

type frame('tile) = ListFrame.t(elem('tile));

include List;
