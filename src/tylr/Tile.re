// open Sexplib.Std;
open Util;
include Base.Tile;

let mk = (~id=?, mold, token: Token.t) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, token};
};

let length = t => Token.length(t.token);

let split_cursor = (_: t) => failwith("todo split_cursor");
