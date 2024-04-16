module Sym = {
  include Sym;
  type t = Sym.t(Label.t, Sort.t);
};
module Regex = {
  include Regex;
  type t = Regex.t(Sym.t);
};
open Regex;

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (srt: Sort.t) => Regex.atom(Sym.nt(srt));

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));

module Typ = {
  let sort = Sort.of_str("Typ");
  let tbl = [];
};

module Pat = {
  let sort = Sort.of_str("Pat");
  let tbl = [];
};

module Exp = {
  let sort = Sort.of_str("Exp");
  let exp = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([exp, Star(seq([c(","), exp]))]);

  // let rule = seq([tokc("|"), p, tokc("=>"), e]);

  // let statement = seq([e, tokc(";")]);
  // let block = Star(statement);

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      // todo: seq([tokc("("), opt(comma_sep), tokc(")")]),
      seq([c("("), exp, c(")")]),
      // todo: seq([tokc("["), opt(comma_sep), tokc("]")]),
      seq([c("["), exp, c("]")]),
      // seq([tokc("case"), e, Star(rule), tokc("end")]),
    ]);

  let tokc_alt = ss => alt(List.map(c, ss));
  let add_op = tokc_alt(["+", "+.", "-", "-."]);
  let mult_op = tokc_alt(["*", "*.", "/", "/."]);
  let neg_op = tokc_alt(["-", "-."]);

  let tbl = [
    p(~a=L, seq([exp, add_op, exp])),
    p(~a=L, seq([exp, mult_op, exp])),
    p(seq([neg_op, exp])),
    p(operand),
  ];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
