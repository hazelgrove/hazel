open Gram;

module Sort = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp
    | Pat
    | Typ;

  let root = Exp;
  let to_int =
    fun
    | Exp => 0
    | Pat => 1
    | Typ => 2;
  let of_int =
    fun
    | 0 => Exp
    | 1 => Pat
    | 2 => Typ
    | _ => raise(Invalid_argument(""));
  let compare = (s1, s2) => Int.compare(to_int(s1), to_int(s2));
  let lca = (s1, s2) => of_int(Int.min(to_int(s1), to_int(s2)));

  let to_string =
    fun
    | Typ => "Typ"
    | Pat => "Pat"
    // | Rul => "Rul"
    | Exp => "Exp";

  let of_string =
    fun
    | "Typ" => Typ
    | "Pat" => Pat
    | "Exp" => Exp
    | _ => raise(Invalid_argument("Sort.of_string: unrecognized sort"));
};

let p = (~a: option(Dir.t)=?, g: t(Sort.t)) => (g, a);

module Typ = {
  let t = [];
};

module Pat = {
  let t = [];
};

module Exp = {
  let e = kid(Sort.Exp);

  let comma_sep = seq([e, Star(seq([tok(","), e]))]);
  let operand =
    alt([
      tok_shape(Int_lit),
      tok_shape(Float_lit),
      tok_shape(Id_lower),
      // todo: seq([tok("("), opt(comma_sep), tok(")")]),
      seq([tok("("), e, tok(")")]),
      // todo: seq([tok("["), opt(comma_sep), tok("]")]),
      seq([tok("["), e, tok("]")]),
    ]);

  let tok_alt = ss => alt(List.map(tok, ss));
  let add_op = tok_alt(["+", "+.", "-", "-."]);
  let mult_op = tok_alt(["*", "*.", "/", "/."]);
  let neg_op = tok_alt(["-", "-."]);

  let t = [
    p(~a=L, seq([e, add_op, e])),
    p(~a=L, seq([e, mult_op, e])),
    p(seq([neg_op, e])),
    p(operand),
  ];
};

let t = Sort.[(Typ, Typ.t), (Pat, Pat.t), (Exp, Exp.t)];
