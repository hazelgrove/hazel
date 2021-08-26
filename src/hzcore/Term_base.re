type t('tile) = (Skel.t, list('tile));

exception EmptyTuple;

let get =
    (
      get_op: 'op => 'out,
      get_pre: ('pre, 't) => 'out,
      get_post: ('t, 'post) => 'out,
      get_bin: ('t, 'bin, 't) => 'out,
      (skel, tiles): t'(Tile_base.t('op, 'pre, 'post, 'bin)) as 't,
    )
    : 'out => {
  let tile = List.nth(tiles, Skel.root_step(skel'));
  switch (skel) {
  | Op(n) => get_op(Tile_base.get_op(tile))
  | Pre(n, r) => get_pre(Tile_base.get_pre(tile), (r, tiles))
  | Post(l, n) => get_post((l, tiles), Tile_base.get_post(tile))
  | Bin(l, n, r) =>
    get_bin((l, tiles), Tile_base.get_bin(tile), (r, tiles))
  };
};
