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
};
include Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.t(Molded.Label.t);

let mk = (~id=?, ~text="", lbl) => {
  let id = Id.Gen.value(id);
  {id, lbl, text};
};

let mk_grout = (~id=?, ~l=false, ~r=false, sort) => {
  let lbl = Molded.Label.mk_grout(~l, sort, ~r);
  mk(~id?, ~text=failwith("grout text"), lbl);
};

module Labeled = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(list(Label.t)));
  let unlabel = (tok: t) =>
    mk(~id=tok.id, ~text=tok.text, Molded.Label.space);
};

let is_empty = (tok: t) => String.equal(tok.text, "");

let to_labeled = (p: t): Labeled.t => {
  let mk = mk(~id=p.id, ~text=p.text);
  switch (p.lbl.mtrl) {
  | Space => mk(Mtrl.Space)
  | Grout => mk(Grout)
  | Tile(lbl) when is_empty(p) => mk(Mtrl.Tile([lbl]))
  | Tile(_) => mk(Tile(Labels.completions(p.text)))
  };
};

// let clear = (p: t) =>
//   switch (p.material) {
//   | Space
//   | Grout(_) => []
//   | Tile(_) => [{...p, token: Token.empty}]
//   };

let id_ = (tok: t) => tok.id;
let text_ = (tok: t) => tok.text;
// let label = p => Mtrl.Labeled.of_molded(p.lbl);
let sort = (tok: t) => tok.lbl.mold.sort;

let length = (tok: t) =>
  switch (tok.lbl.mtrl) {
  | Tile(Const(c)) => String.length(c)
  | _ => String.length(tok.text)
  };

let is_space = (tok: t) =>
  switch (tok.lbl.mtrl) {
  | Space => true
  | _ => false
  };
let is_grout = (tok: t) =>
  switch (tok.lbl.mtrl) {
  | Space
  | Tile(_) => false
  | Grout => true
  };
let is_ghost = (tok: t) =>
  switch (tok.lbl.mtrl) {
  | Space
  | Grout => false
  | Tile(lbl) => !Label.is_complete(tok.text, lbl)
  };

let merge_text =
  fun
  | [] => None
  | [hd, ...tl] => {
      // may need to perform effect here
      let text = (hd: t).text ++ String.concat("", List.map(text_, tl));
      Some({...hd, text});
    };

let merge = (l: t, r: t) =>
  if (Id.eq(l.id, r.id)) {
    assert(Mold.equal(l.lbl.mold, r.lbl.mold));
    Some({...l, text: l.text ++ r.text});
  } else {
    None;
  };

let split = (n: int, tok: t): Result.t((t, t), Dir.t) =>
  switch (tok.lbl.mtrl, StringUtil.unzip_opt(n, tok.text)) {
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
