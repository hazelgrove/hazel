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

module Grammar = {
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
};
