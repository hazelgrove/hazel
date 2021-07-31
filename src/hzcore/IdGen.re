type t = {
  next_hole: MetaVar.t,
  // TODO: create TileId
  next_tile: MetaVar.t,
};

let init = {next_hole: 0, next_tile: 0};