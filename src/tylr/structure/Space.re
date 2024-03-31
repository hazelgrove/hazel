open Mtrl;

module Sym = {
  let t = Sym.t(Space);
  let nt = Sym.nt(NT.space);
};

module Regex = {
  include Regex;
  // rhs regex for "space sort"
  let v = aseq(Sym.[nt, t, nt]);
};

module Mold = {
  include Mold;
  // constructors for molds within the "space sort"
  // (not for padding cells in author-specified sorts)
  let mk = (rctx: RCtx.t(_)) => {sort: Space, prec: 0, rctx};
  let of_t = mk([RFrame.aseq_([Sym.nt], [Sym.nt])]);
  let of_nt =
    fun
    | Dir.L => mk([RFrame.aseq_([], [Sym.t, Sym.nt])])
    | R => mk([RFrame.aseq_([Sym.nt, Sym.t], [])]);
};

module Molded = {
  let t = (Mtrl.Space, Mold.of_t);
};

module Token = {
  include Token;
  let cursor = failwith("todo");
};

module Meld = {
  let mk = (tok: Token.t) => {
    assert(Mtrl.is_space(tok.mtrl));
    let l = Cell.mk(Node(Mold.of_nt(L)), Mtrl.Space);
    let r = Cell.mk(Node(Mold.of_nt(R)), Mtrl.Space);
    Meld.M(l, Wald.of_tok(tok), r);
  };
  let cursor = mk(Token.cursor);
};
