open Regex;

let p = (~a: option(Dir.t)=?, g: t(Sort.t)) => (g, a);

module Typ = {
  let t = [];
};

module Pat = {
  let t = [];
};

module Exp = {
  let e = kid(Sort.Exp);

  [@warning "-32"]
  let comma_sep = seq([e, Star(seq([tokc(","), e]))]);

  // let rule = seq([tokc("|"), p, tokc("=>"), e]);

  // let statement = seq([e, tokc(";")]);
  // let block = Star(statement);

  let operand =
    alt([
      tok(Int_lit),
      tok(Float_lit),
      tok(Id_lower),
      // todo: seq([tokc("("), opt(comma_sep), tokc(")")]),
      seq([tokc("("), e, tokc(")")]),
      // todo: seq([tokc("["), opt(comma_sep), tokc("]")]),
      seq([tokc("["), e, tokc("]")]),
      // seq([tokc("case"), e, Star(rule), tokc("end")]),
    ]);

  let tokc_alt = ss => alt(List.map(tokc, ss));
  let add_op = tokc_alt(["+", "+.", "-", "-."]);
  let mult_op = tokc_alt(["*", "*.", "/", "/."]);
  let neg_op = tokc_alt(["-", "-."]);

  let t = [
    p(~a=L, seq([e, add_op, e])),
    p(~a=L, seq([e, mult_op, e])),
    p(seq([neg_op, e])),
    p(operand),
  ];
};

let v = Sort.[(Typ, Typ.t), (Pat, Pat.t), (Exp, Exp.t)];
