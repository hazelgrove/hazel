open Sexplib.Std;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('lbl) = {
    [@hash.ignore]
    id: Id.t,
    lbl: 'lbl,
    text: string,
  };
  let mk = (~id=?, ~text="", lbl) => {
    let id = Id.Gen.value(id);
    {id, lbl, text};
  };
  let is_empty = p => String.equal(p.text, "");
};

module Labeled = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(list(Label.t)));
  // let mk: (~id: _=?, ~text: string=?, _) => t = mk;
};

module Molded = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Molded.Label.t);

  let to_labeled = (p: t): Labeled.t => {
    let mk = Labeled.mk(~id=p.id, ~text=p.text);
    switch (p.lbl.mtrl) {
    | Space => mk(Mtrl.Space)
    | Grout => mk(Grout)
    | Tile(lbl) when is_empty(p) => mk(Mtrl.Tile([lbl]))
    | Tile(_) => mk(Tile(Labels.completions(p.text)))
    };
  };
};
include Molded;

// let clear = (p: t) =>
//   switch (p.material) {
//   | Space
//   | Grout(_) => []
//   | Tile(_) => [{...p, token: Token.empty}]
//   };

let id_ = (tok: t) => tok.id;
// let label = p => Mtrl.Labeled.of_molded(p.lbl);
let sort = (tok: t) => tok.lbl.mold.sort;
// let prec = p => Mtrl.map(Mold.prec_, p.material);

// let put_label = (_, _) => failwith("todo Piece.put_label");
let put_text = (text, tok: t) => {...tok, text};

// None if non constant label
// let label_length = p =>
//   switch (label(p)) {
//   | Grout () => None
//   | Tile(lbl) => Label.length(lbl)
//   };
// let token_length = p => Token.length(p.text);

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

// let is_finished = p =>
//   switch (p.material) {
//   | Grout(_) => false
//   | Tile(_) => label_length(p) == Some(token_length(p))
//   };

// let is_complete = p =>
//   // assumes well-labeled
//   switch (Label.length(label(p))) {
//   | None => true
//   | Some(0) =>
//     assert(is_grout(p));
//     true;
//   | Some(n) => Token.length(p.text) == n
//   };

// let is_porous = p => Token.is_empty(p.text);

let unzip = (n: int, tok: t): Result.t((t, t), Dir.t) =>
  switch (tok.lbl.mtrl, StringUtil.unzip_opt(n, tok.text)) {
  | (_, Some(("", _))) => Error(L)
  | (Space | Grout, Some((_, ""))) => Error(R)
  | (Space | Grout, Some((l, r))) =>
    Ok(({...tok, text: l}, {...tok, text: r}))
  | (Space | Grout, None) => raise(Invalid_argument("Token.unzip"))
  | (Tile(lbl), Some((_, ""))) when Label.is_complete(tok.text, lbl) =>
    Error(R)
  | (Tile(lbl), Some((txt_l, txt_r))) =>
    let (l, r) = Label.unzip(String.length(tok.text), lbl);
    let lbl_l = {...tok.lbl, mtrl: Mtrl.Tile(l)};
    let lbl_r = {...tok.lbl, mtrl: Mtrl.Tile(r)};
    let l = {...tok, lbl: lbl_l, text: txt_l};
    let r = {...tok, lbl: lbl_r, text: txt_r};
    Ok((l, r));
  | (Tile(lbl), None) =>
    n > 0 && !Label.is_complete(tok.text, lbl)
      ? Error(R) : raise(Invalid_argument("Token.unzip"))
  };
