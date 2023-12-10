open Sexplib.Std;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl) = {
    [@hash.ignore]
    id: Id.t,
    mtrl: 'mtrl,
    token: Token.t,
  };
  let mk = (~id=?, ~token=Token.empty, mtrl) => {
    let id = Id.Gen.value(id);
    {id, mtrl, token};
  };
  let is_empty = p => String.eq(p.token, "");
};

module Labeled = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(EMtrl.Labeled.t);
};

module Molded = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(EMtrl.Molded.t);
};
include Molded;

type Effect.t(_) +=
  | Insert(t)
  | Remove(t);

let to_labeled = (p: t): Labeled.t => {
  let mk = m => Labeled.mk(~id=p.id, ~token=p.token, m);
  switch (p.mtrl) {
  | Space => mk(Space)
  | Grout(tips) => mk(Grout(tips))
  | Tile(m) when is_empty(p) => mk(Tile([GMold.label(m)]))
  | Tile(_) => mk(Tile(Labels.with_prefix(p.token)))
  };
};

// let relabel = (p: t): (Material.Labeled)

// well-labeled invariant: for piece p
// !Label.is_empty(p.material.label) ==> is_prefix(p.token, p.material.label)
// exception Ill_labeled;

let clear = (p: t) =>
  switch (p.material) {
  | Space
  | Grout(_) => []
  | Tile(_) => [{...p, token: Token.empty}]
  };

let mk = (~id=?, ~token="", material) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, material, token};
};
let id_ = p => p.id;
let label = p => Material.Labeled.of_molded(p.material);
let sort = p => Material.Sorted.of_molded(p.material);
// let prec = p => Material.map(Mold.prec_, p.material);

let put_label = (_, _) => failwith("todo Piece.put_label");
let put_token = (token, p) => {...p, token};

// None if non constant label
let label_length = p =>
  switch (label(p)) {
  | Grout () => None
  | Tile(lbl) => Label.length(lbl)
  };
let token_length = p => Token.length(p.token);

// todo: review uses and replace with one of above
let length = _ => failwith("todo: Piece.length");

// let is_grout = p => label_length(p) == 0;

// todo: review uses and rename
let is_empty = _ => failwith("todo Piece.is_empty");

// let tip = (side, p) => Mold.tip(side, mold(p));
// let tips = (side, p) => Mold.tips(side, mold(p));
// let convexable = (side, p) => List.mem(Tip.Convex, tips(side, p));

// let mk = (~id=?, ~paths=[], shape: Shape.t) => {
//   let id = id |> OptUtil.get(() => Id.Gen.next());
//   {id, paths, shape};
// };
// let of_grout = (~id=?, ~paths=[], g) => mk(~id?, ~paths, G(g));
// let of_tile = (~id=?, ~paths=[], t) => mk(~id?, ~paths, T(t));

let is_finished = p =>
  switch (p.material) {
  | Grout(_) => false
  | Tile(_) => label_length(p) == Some(token_length(p))
  };

// let is_complete = p =>
//   // assumes well-labeled
//   switch (Label.length(label(p))) {
//   | None => true
//   | Some(0) =>
//     assert(is_grout(p));
//     true;
//   | Some(n) => Token.length(p.token) == n
//   };

// let is_porous = p => Token.is_empty(p.token);

let unzip = (n: int, p: t): Result.t((t, t), Dir.t) => {
  switch (label(p), Token.unzip(n, p.token)) {
  | (Grout (), Error(L)) => Error(n < 1 ? L : R)
  | (Tile(_), Error(L)) => Error(L)
  | (Tile(lbl), Error(R)) when n == Token.length(p.token) =>
    switch (Label.unzip(n, lbl)) {
    | Error(side) =>
      assert(side == R);
      Error(R);
    | Ok((lbl_l, lbl_r)) =>
      let l = put_label(lbl_l, p);
      let r = put_label(lbl_r, {...p, token: Token.empty});
      Ok((l, r));
    }
  | (_, Error(R)) => Error(R)
  | (Grout (), Ok((tok_l, tok_r))) =>
    Ok(({...p, token: tok_l}, {...p, token: tok_r}))
  | (Tile(lbl), Ok((tok_l, tok_r))) =>
    switch (Label.unzip(n, lbl)) {
    | Error(_) => raise(Ill_labeled)
    | Ok((lbl_l, lbl_r)) =>
      let l = put_label(lbl_l, {...p, token: tok_l});
      let r = put_label(lbl_r, {...p, token: tok_r});
      Ok((l, r));
    }
  };
};
