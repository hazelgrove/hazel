// open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  proto: Proto.t,
  undone: int,
};

let mk = (~undone=0, proto) => {proto, undone};

let get_proto = (f, t) => f(t.proto);
let length = get_proto(Proto.length);

let uncons_char = ({proto, undone}: t) =>
  Proto.uncons_char(proto)
  |> Option.map(((l, r)) =>
       undone < Proto.length(proto)
         ? (mk(l), mk(~undone, r))
         : (mk(~undone=1, l), mk(~undone=undone - 1, r))
     );
let unsnoc_char = ({proto, undone}: t) =>
  Proto.unsnoc_char(proto)
  |> Option.map(((l, r)) =>
       undone > 0
         ? (mk(~undone=undone - 1, l), mk(~undone=1, r)) : (mk(l), mk(r))
     );

let split_cursor = (_: t) => failwith("todo split_cursor");

let unzip = (n: int, t: t): Either.t(Dir.t, (t, t)) =>
  switch (Token.split(n, t.token)) {
  | ("", _) => L(L)
  | (_, "") => L(R)
  | (l, r) => R(({...t, token: l}, {...t, token: r}))
  };
