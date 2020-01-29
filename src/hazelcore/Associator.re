let rec make_skel_str' =
        (
          string_of_op: 'op => string,
          seq: Seq.t('operand, 'op),
          counter: ref(int),
          ph_map: Hashtbl.t(int, 'operand),
        )
        : string =>
  switch (seq) {
  | S(hd, E) =>
    let n = counter^;
    Hashtbl.add(ph_map, n, hd);
    string_of_int(n);
  | S(hd, A(op, seq)) =>
    let n = counter^;
    counter := n + 1;
    Hashtbl.add(ph_map, n, hd);
    let skel_str = make_skel_str'(string_of_op, seq, counter, ph_map);
    let op_str = string_of_op(op);
    string_of_int(n) ++ op_str ++ skel_str;
  };

let make_skel_str = (seq: Seq.t('operand, 'op), string_of_op: 'op => string) => {
  let counter = ref(0);
  let ph_map = Hashtbl.create(8);
  let skel_str = make_skel_str'(string_of_op, seq, counter, ph_map);
  (skel_str, ph_map);
};

module Typ = {
  let string_of_op: UHTyp.operator => string =
    fun
    | Sum => "|"
    | Prod => ","
    | Arrow => "->";

  let parse = s => {
    let lexbuf = Lexing.from_string(s);
    SkelTypParser.skel_typ(SkelTypLexer.read, lexbuf);
  };

  let associate = (seq: UHTyp.seq) => {
    let (skel_str, _) = make_skel_str(seq, string_of_op);
    parse(skel_str);
  };
};

module Pat = {
  let string_of_op: UHPat.operator => string =
    fun
    | Space => "_"
    | Comma => ","
    | Cons => "::";

  let parse = s => {
    let lexbuf = Lexing.from_string(s);
    SkelPatParser.skel_pat(SkelPatLexer.read, lexbuf);
  };

  let associate = (seq: UHPat.seq) => {
    let (skel_str, _) = make_skel_str(seq, string_of_op);
    parse(skel_str);
  };
};

module Exp = {
  let string_of_op: UHExp.operator => string =
    fun
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | LessThan => "<"
    | GreaterThan => ">"
    | Equals => "="
    | Space => "_"
    | Comma => ","
    | Cons => "::"
    | And => "&"
    | Or => "|";

  let parse = s => {
    let lexbuf = Lexing.from_string(s);
    SkelExprParser.skel_expr(SkelExprLexer.read, lexbuf);
  };

  let associate = (seq: UHExp.seq) => {
    let (skel_str, _) = make_skel_str(seq, string_of_op);
    parse(skel_str);
  };
};
