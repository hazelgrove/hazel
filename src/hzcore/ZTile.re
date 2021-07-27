type t = (ChildStep.t, Path.caret_step, Tile.t);

let get = (get_typ, get_pat, get_exp, ztile) =>  {
  let (child, caret, tile) = ztile;
  switch (tile) {
  | Typ(tile) => get_typ((child, caret, tile))
  | Pat(tile) => get_pat((child, caret, tile))
  | Exp(tile) => get_exp((child, caret, tile))
  };
};

let delete = d =>
  get(
    ZTile_typ.delete(d),
    ZTile_pat.delete(d),
    ZTile_exp.delete(d),
  );