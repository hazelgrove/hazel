open Sexplib.Std;

module Kid = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pad: bool,
    sort: Sort.t,
  };

  let pat = (~pad=true, ()) => {pad, sort: Pat};

  let exp = (~pad=true, ()) => {pad, sort: Exp};

  let mk = (~pad=true) =>
    fun
    | Sort.Pat => pat(~pad, ())
    | Exp => exp(~pad, ())
    | _ => failwith("todo");
};

// module Kids = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = (option(Kid.t), list(Kid.t), option(Kid.t));

//   let m_length = ((_, m, _)) => List.length(bi);
// };

module Mold = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    prec: Precedence.t,
    sort: Sort.t,
    kids: (option(Kid.t), list(Kid.t), option(Kid.t)),
  };

  let mk =
      (
        ~l: option(Kid.t)=?,
        ~m: list(Kid.t)=[],
        ~r: option(Kid.t)=?,
        sort: Sort.t,
        prec: Precedence.t,
      ) => {
    prec,
    sort,
    kids: (l, m, r),
  };

  let op = (~m=[], sort) => mk(~m, sort);
  let pre = (~m=[], r, sort) => mk(~m, ~r, sort);
  let post = (l, ~m=[], sort) => mk(~l, ~m, sort);
  let bin = (l, ~m=[], r) => mk(~l, ~m, ~r);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // invariant: List.length(label) >= 1
  label: list(Token.t),
  // invariant: List.length(label) == Kids.m_length(mold.kids) + 1
  mold: Mold.t,
};

let pre =
    (
      ~sort: Sort.t,
      ~m: list(Kid.t)=[],
      ~r=Kid.mk(sort),
      label: list(Token.t),
      prec: Precedence.t,
    ) => {
  assert(List.length(label) == List.length(m) + 1);
  {label, mold: Mold.mk(~m, ~r, sort, prec)};
};
let bin =
    (
      ~sort: Sort.t,
      ~l=Kid.mk(sort),
      ~m: list(Kid.t)=[],
      ~r=Kid.mk(sort),
      label: list(Token.t),
      prec: Precedence.t,
    ) => {
  assert(List.length(label) == List.length(m) + 1);
  {label, mold: Mold.mk(~l, ~m, ~r, sort, prec)};
};

let to_shards = ({mold, label}: t): list(Shard.Form.t) => {
  // let lbl = List.mapi((i, t) => (i, t), label);
  let (l_kid, m_kids, r_kid) = mold.kids;
  let outer_nib =
    fun
    | None => Nib.{sort: mold.sort, shape: Convex}
    | Some(kid: Kid.t) =>
      Nib.{sort: kid.sort, shape: Concave({prec: mold.prec, pad: kid.pad})};
  let (l, r) = (outer_nib(l_kid), outer_nib(r_kid));
  let m = i => {
    let kid = List.nth(m_kids, i);
    let sort = kid.sort;
    let shape =
      Nib.Shape.Concave({pad: kid.pad, prec: failwith("sort-specific min")});
    Nib.{sort, shape};
  };
  label
  |> List.mapi((i, token) => {
       let l = i == 0 ? l : m(i - 1);
       let r = i == List.length(label) - 1 ? r : m(i);
       let mold = Shard.Mold.{sort: mold.sort, nibs: (l, r)};
       Shard.Form.{mold, token};
     });
};
