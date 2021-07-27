type elem('tile) =
  | Tile('tile)
  | Grout(Grout.t);

type t('tile) = list(elem('tile));

include List;
