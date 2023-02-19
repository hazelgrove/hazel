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

let uncons_char = (t: t): option((t, t)) => {
  open OptUtil.Syntax;
  let* (hd, tl) = StringUtil.uncons(t.token);
  tl == "" ? None : Some(({...t, token: hd}, {...t, token: tl}));
};

let unsnoc_char = (t: t): option((t, t)) => {
  open OptUtil.Syntax;
  let* (tl, hd) = StringUtil.unsnoc(t.token);
  tl == "" ? None : Some(({...t, token: tl}, {...t, token: hd}));
};

let zip = (l: t, r: t): option(t) =>
  Id.eq(l.id, r.id) ? Some({...l, token: l.token ++ r.token}) : None;

let unzip = (n: int, t: t): option((t, t)) =>
  switch (Token.split(n, t.token)) {
  | ("", _)
  | (_, "") => None
  | (l, r) => Some(({...t, token: l}, {...t, token: r}))
  };
