open Gram;

module Sort = {
  type t =
    | Exp
    | Pat
    | Typ;

  let to_int =
    fun
    | Exp => 0
    | Pat => 1
    | Typ => 2;

  let compare = (s1, s2) => Int.compare(to_int(s1), to_int(s2));

  let root = Exp;
};

let p = (~a: option(Dir.t)=?, g: t(Sort.t)) => (g, a);

module Typ = {
  let t = failwith("todo");
};

module Pat = {
  let t = failwith("todo");
};

module Exp = {
  let e = kid(Sort.Exp);

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

  let neg_op = tok_alt(["-", "-."]);

  let t = [
    p(seq([e, add_op, e])),
    p(seq([e, mult_op, e])),
    p(seq([neg_op, e])),
    p(operand),
  ];
};

let t = Sort.[(Typ, Typ.t), (Pat, Pat.t), (Exp, Exp.t)];
