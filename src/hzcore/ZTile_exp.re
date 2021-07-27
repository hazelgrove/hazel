open Term_exp;

type t = ZTile_base.t(Term_exp.tile);

let delete = (d: Direction.t, ztile: t) => {
  let (child_step, caret_step, tile) = ztile;
  tile
  |> Tile_base.get(
    fun
    | OpHole(_)
  )
};