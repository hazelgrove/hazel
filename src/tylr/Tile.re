// open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
  token: Token.t,
};

let mk = (~id=?, mold, token: Token.t) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, token};
};

let length = t => Token.length(t.token);

let split_cursor = (_: t) => failwith("todo split_cursor");

let uncons_char = (t: t): option((t, t)) =>
  StringUtil.uncons(t.token)
  |> Option.map(((hd, tl)) => ({...t, token: hd}, {...t, token: tl}));

let unsnoc_char = (t: t): option((t, t)) =>
  StringUtil.unsnoc(t.token)
  |> Option.map(((tl, hd)) => ({...t, token: tl}, {...t, token: hd}));
