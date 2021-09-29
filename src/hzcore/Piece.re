type t =
  | Shard(Shard.t)
  | Tile(Tile.t);

let shard = s => Shard(s);
let tile = t => Tile(t);