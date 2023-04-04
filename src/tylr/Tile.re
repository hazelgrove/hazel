open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  proto: Proto.t,
  unfilled: int,
};

let mk = (~unfilled=0, proto) => {proto, unfilled};

// let get_proto = (f, t) => f(t.proto);
let length = t => Proto.length(t.proto);

let is_empty = t => t.unfilled == length(t);
let is_filled = t => t.unfilled == 0;

// assumes client has already checked for same-id zip-ability
let zip = (l, r) => {...l, unfilled: l.unfilled + r.unfilled};

let unzip = (n: int, {proto, unfilled}: t): Either.t(Dir.t, (t, t)) =>
  Proto.unzip(n, proto)
  |> Either.map_r(((l, r)) => {
       let n = Proto.length(r);
       unfilled <= n
         ? (mk(l), mk(~unfilled, r))
         : (mk(~unfilled=unfilled - n, l), mk(~unfilled=n, r));
     });
let uncons_char = t => {
  // treat unfilled portion as single char
  let n = is_empty(t) ? length(t) : 1;
  switch (unzip(n, t)) {
  | L(_) => None
  | R(tt) => Some(tt)
  };
};
let unsnoc_char = t => {
  // treat unfilled portion as single char
  let n = length(t) - (!is_filled(t) ? t.unfilled : 1);
  switch (unzip(n, t)) {
  | L(_) => None
  | R(tt) => Some(tt)
  };
};
