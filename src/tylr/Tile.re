open Util;

type t = {
  id: Id.t,
  mold: option(Mold.t),
  token: Token.t,
};

let mk = (~id=?, ~mold=?, token: Token.t) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, token};
};

let split_cursor = (_: t) => failwith("todo split_cursor");
