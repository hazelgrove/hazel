type t = (ChildStep.t, Path.caret_step, Tile.t);

let get: (
  ZTile_typ.t => 'out,
  ZTile_pat.t => 'out,
  ZTile_exp.t => 'out,
  t,
) => 'out;

let delete: (Direction.t, t) => option(ListFrame);
