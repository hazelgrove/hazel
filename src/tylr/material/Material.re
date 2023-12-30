[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('g, 't) =
  | Space
  | Grout('g)
  | Tile('t);
