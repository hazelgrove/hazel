open Gram;

let p = (~a: option(Dir.t)=?, g: t) => (g, None);

module Typ = {
  let t = failwith("todo");
};

module Pat = {
  let t = failwith("todo");
};

module Exp = {
  let e = kid(Exp);

  let operand =
    alt([
      tok_shape(Int_lit),
      tok_shape(Float_lit),
      tok_shape(Alphanum_lower),
      seq([tok("("), opt(e), tok(")")]),
      seq([tok("["), opt(e), tok("]")]),
    ]);

  let tok_alt = ss => alt(List.map(tok, ss));

  let add_op = tok_alt(["+", "+.", "-", "-."]);

  let mult_op = tok_alt(["*", "*.", "/", "/."]);

  let neg_op = tok_alt("-", "-.");

  let t = [
    p(seq([e, add_op, e])),
    p(seq([e, mult_op, e])),
    p(seq([neg_op, e])),
    p(operand),
  ];
};

let t = Sort.[(Typ, Typ.t), (Pat, Pat.t), (Exp, Exp.t)];
