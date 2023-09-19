open Util;

module Base = Meld.Slot;
include Base;

let to_opt =
  fun
  | Empty => None
  | Full(a) => Some(a);

module Molded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(option(Sort.t));
};
module Baked = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(EPath.Marked.t(Meld.Baked.t));
  let has_no_tiles =
    fun
    | Empty => Some("")
    | Full(m) => Meld.Baked.has_no_tiles(m);
};

let empty = None;
let full = (m: Meld.t(_)) => Some(m);

module Profile = {
  type t = option(Meld.Profile.t);

  let mk = (~has_tokens=false, sort: Material.Sorted.t) =>
    Some(Meld.Profile.{sort, has_tokens});

  let has_tokens: t => bool =
    fun
    | None => false
    | Some(p) => p.has_tokens;

  let merge = (l: t, r: t) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None) => l
    | (None, Some(_)) => r
    | (Some(l), Some(r)) =>
      mk(~has_tokens=l.has_tokens || r.has_tokens, Grout())
    };
};

module Path