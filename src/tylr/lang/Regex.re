open Util;

module Atom = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t =
    | Tok(Token.Shape.t)
    | Kid(Sort.t);

  let is_tok =
    fun
    | Kid(_) => None
    | Tok(t) => Some(t);
  let is_kid =
    fun
    | Tok(_) => None
    | Kid(s) => Some(s);
};

include Regex;
type t = Regex.t(Atom.t);

module Unzipped = {
  include Regex.Unzipped;
  type t = Regex.Unzipped.t(Atom.t);
};
module Zipper = {
  include Regex.Zipper;
  type t('x) = Regex.Zipper.t('x, Atom.t);
  let move_to_tok = move(~until=Atom.is_tok);
  let move_to_kid = move(~until=Atom.is_kid);
};

// currently assuming:
// (1) no consecutive kids
// (2) no consecutive tokens
// (3) every sort is convex
// only (1) fundamentally necessary
exception Ill_typed;

let tok_shape = (t: Token.Shape.t) => Atom(Atom.Tok(t));
let tok = (s: string) => tok_shape(Const(s));
let kid = s => Atom(Atom.Kid(s));

let kids =
  fold(
    ~atom=a => Option.to_list(Atom.is_kid(a)),
    ~star=Fun.id,
    ~seq=List.concat,
    ~alt=List.concat,
  );

let end_toks = (side: Dir.t, r: t): list(Zipper.t(Token.Shape.t)) =>
  Zipper.enter(~from=side, r, Unzipped.empty)
  |> List.concat_map((z: Zipper.t(Atom.t)) =>
       switch (z) {
       | (Tok(tok), uz) => [(tok, uz)]
       | (Kid(_), _) => Zipper.move(~until=Atom.is_tok, Dir.toggle(side), z)
       }
     );
