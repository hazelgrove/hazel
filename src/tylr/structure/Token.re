open Sexplib.Std;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl, 'mold) = {
    [@hash.ignore]
    id: Id.t,
    mtrl: 'mtrl,
    mold: 'mold,
    text: string,
  };
  let mk = (~id=?, ~text="", mtrl, mold) => {
    let id = Id.Gen.value(id);
    {id, mtrl, mold, text};
  };
};
// include Base;

module Molded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.Labeled.t, Mold.t);
  let mk = (~id=?, ~text="", mtrl: Mtrl.Labeled.t, mold: Mold.t) =>
    Base.mk(~id?, ~text, mtrl, mold);
};
include Molded;

let id_ = (tok: t) => tok.id;
let text_ = (tok: t) => tok.text;
let sort = (tok: t) => tok.mold.sort;
let length = (tok: t) =>
  switch (tok.mtrl) {
  | Tile(Const(c)) => String.length(c)
  | _ => String.length(tok.text)
  };

// let mk = (~id=?, ~text="", mtrl: Mtrl.Labeled.t, mold) => {
//   let id = Id.Gen.value(id);
//   {id, mtrl, mold, text};
// };
// let mk_grout = (~id=?, ~l=false, ~r=false, sort) => {
//   let mold = Mold.of_grout(~l, sort, ~r);
//   mk(~id?, ~text=failwith("grout text"), Mtrl.Grout, mold);
// };
// let mk_space = (~id=?, text) => mk(~id?, ~text, Mtrl.Space, Mold.Space.of_t);
// let cursor = failwith("todo");

let is_empty = (tok: t) => String.equal(tok.text, "");
let is_space = (tok: t) => Mtrl.is_space(tok.mtrl);
let is_grout = (tok: t) => Mtrl.is_grout(tok.mtrl);
let is_ghost = (tok: t) =>
  switch (tok.mtrl) {
  | Space
  | Grout => false
  | Tile(lbl) => !Label.is_complete(tok.text, lbl)
  };

module Space = {
  let mk = (~id=?, ~text="", ()) =>
    Molded.mk(~id?, ~text, Space, Space.Mold.of_t);
  let empty = mk();
  let cursor = failwith("todo");
};

module Unmolded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(list(Label.t)), unit);
  let unmold = (tok: Molded.t): t => {
    let mtrl =
      switch (tok.mtrl) {
      | Space => Mtrl.Space
      | Grout => Grout
      | Tile(lbl) =>
        Tile(is_empty(tok) ? [lbl] : Labels.completions(tok.text))
      };
    Base.mk(~id=tok.id, ~text=tok.text, mtrl, ());
  };
  let defer = (tok: t): Molded.t => Space.mk(~id=tok.id, ~text=tok.text, ());
};

let merge_text = (l: t, r: t) => {
  let _ = failwith("perform effect here?");
  {...l, text: l.text ++ r.text};
};

let merge = (l: t, r: t) =>
  if (Id.eq(l.id, r.id)) {
    assert(Mold.equal(l.mold, r.mold));
    Some({...l, text: l.text ++ r.text});
  } else {
    None;
  };

let split = (n: int, tok: t): Result.t((t, t), Dir.t) =>
  switch (tok.mtrl, StringUtil.unzip_opt(n, tok.text)) {
  | (_, Some(("", _))) => Error(L)
  | (Space | Grout, Some((_, ""))) => Error(R)
  | (Space | Grout, Some((l, r))) =>
    Ok(({...tok, text: l}, {...tok, text: r}))
  | (Space | Grout, None) => raise(Invalid_argument("Token.unzip"))
  | (Tile(lbl), Some((_, ""))) when Label.is_complete(tok.text, lbl) =>
    Error(R)
  | (Tile(_), Some((txt_l, txt_r))) =>
    let l = {...tok, text: txt_l};
    let r = {...tok, text: txt_r};
    Ok((l, r));
  | (Tile(lbl), None) =>
    n > 0 && !Label.is_complete(tok.text, lbl)
      ? Error(R) : raise(Invalid_argument("Token.unzip"))
  };

let pull = (~from: Dir.t, tok: t): option((t, t)) => {
  let n = Dir.pick(from, (1, length(tok) - 1));
  Result.to_option(split(n, tok));
};
