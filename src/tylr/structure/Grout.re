open Mtrl;

module Sym = {
  let t = Sym.t(Grout);
  let nt = Sym.nt(Padded.mk(Grout));
};

module Regex = {
  include Regex;
  let pre = Regex.(opt(aseq([Space.Sym.nt, Sym.t]))); // <<?
  let pos = Regex.(opt(aseq([Sym.t, Space.Sym.nt]))); // >>?
  let ins = Regex.(star(aseq([Sym.t, Sym.nt]))); // (>< G)*
  let v = Regex.(seq([pre, atom(Sym.nt), ins, pos])); // <<? G (>< G)* >>?
};

module Mold = {
  include Mold;
  open RFrame;
  // mold constructors for molds of grout that appear in author-specified sorts
  let mk = (rctx: RCtx.t(_), s: Sort.t) =>
    Mold.{sort: Tile(s), prec: 0, rctx};
  module T = {
    let op_ = mk([
      aseq_([Space.Sym.nt], [Space.Sym.nt])
    ]);
    let pre =
      mk([
        aseq_([Space.Sym.nt], []),
        opt_,
        seq_([], Regex.[atom(Sym.nt), ins, pos]),
      ]);
    let pos =
      mk([
        aseq_([], [Space.Sym.nt]),
        opt_,
        seq_(Regex.[pre, atom(Sym.nt), ins], []),
      ]);
    let in_ =
      mk([aseq_([], [Sym.nt]), star_, seq_([Regex.pre], [Regex.pos])]);
  };
  module NT = {
    let pad_l =
      mk([
        aseq_([], [Sym.t]),
        opt_,
        seq_([], Regex.[atom(Sym.nt), ins, pos]),
      ]);
    let pad_r =
      mk([
        aseq_([Sym.t], []),
        opt_,
        seq_(Regex.[pre, atom(Sym.nt), ins], []),
      ]);
    let kid_hd = mk([seq_([Regex.pre], Regex.[ins, pos])]);
    let kid_tl =
      mk([aseq_([Sym.t], []), star_, seq_([Regex.pre], [Regex.pos])]);
  };
};

module Token = {
  let text = failwith("todo: grout text");
  let mk = (~id=?, mold: Sort.t => Mold.t, s: Sort.t) =>
    Token.mk(~id?, ~text, Mtrl.Grout, mold(s));
  let op_ = (~id=?) => mk(~id?, Mold.T.op_);
  let pre = (~id=?) => mk(~id?, Mold.T.pre);
  let pos = (~id=?) => mk(~id?, Mold.T.pos);
  let in_ = (~id=?) => mk(~id?, Mold.T.in_);
};

module Cell = {
  let put_hd = s => Cell.put(Node(Mold.NT.kid_hd(s)), Mtrl.Grout);
  let put_tl = s => Cell.put(Node(Mold.NT.kid_tl(s)), Mtrl.Grout);
  let pad_l = s => Cell.empty(Node(Mold.NT.pad_l(s)), Mtrl.Space);
  let pad_r = s => Cell.empty(Node(Mold.NT.pad_r(s)), Mtrl.Space);
};

