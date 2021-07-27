type t('tile) = (Skel.t, Tiles.t('tile));

let get:
  (
    'op => 'out,
    ('pre, 't) => 'out,
    ('t, 'post) => 'out,
    ('t, 'bin, 't) => 'out,
    list('t) => 'out,
    t(Tile_base.t('op, 'pre, 'post, 'bin)) as 't
  ) =>
  'out;
