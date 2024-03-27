open Mtrl;

module Sym = {
  let t = Sym.t(Grout);
  let nt = Sym.nt(NT.grout);
};

module Regex = {
  include Regex;
  let pre = Regex.(opt(aseq([Space.Sym.nt, Sym.t]))); // <<?
  let pos = Regex.(opt(aseq([Sym.t, Space.Sym.nt]))); // >>?
  let ins = Regex.(star(aseq([Sym.t, Sym.nt]))); // (>< G)*
  let v = Regex.(seq([pre, atom(Sym.nt), ins, pos])); // <<? G (>< G)* >>?
};

module Mold = {
  // mold constructors for molds of grout that appear in author-specified sorts
  let mk = (rctx: RCtx.t(_), s: Sort.t) =>
    Mold.{sort: Tile(s), prec: 0, rctx};
  let pre =
    mk(
      RFrame.[
        aseq_([Space.Sym.nt], []),
        opt_,
        seq_([], Regex.[atom(Sym.nt), ins, pos]),
      ],
    );
  let pos =
    mk(
      RFrame.[
        aseq_([], [Space.Sym.nt]),
        opt_,
        seq_(Regex.[pre, atom(Sym.nt), ins], []),
      ],
    );
  let ins =
    mk(
      RFrame.[
        aseq_([], [Sym.nt]),
        star_,
        seq_([Regex.pre], [Regex.pos]),
      ],
    );
  let kid_l = mk(RFrame.[seq_([Regex.pre], Regex.[ins, pos])]);
  let kid_r =
    mk(
      RFrame.[aseq_([Sym.t], []), star_, seq_([Regex.pre], [Regex.pos])],
    );
};
