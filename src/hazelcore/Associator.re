let parse_expr = s => {
  let lexbuf = Lexing.from_string(s);
  SkelExprParser.skel_expr(SkelExprLexer.read, lexbuf);
};
let parse_pat = s => {
  let lexbuf = Lexing.from_string(s);
  SkelPatParser.skel_pat(SkelPatLexer.read, lexbuf);
};
let parse_typ = s => {
  let lexbuf = Lexing.from_string(s);
  SkelTypParser.skel_typ(SkelTypLexer.read, lexbuf);
};
let string_of_expr_op: UHExp.operator => string =
  fun
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | LessThan => "<"
  | Space => "_"
  | Comma => ","
  | Cons => "::"
  | And => "&"
  | Or => "|";
let string_of_pat_op: UHPat.operator => string =
  fun
  | Space => "_"
  | Comma => ","
  | Cons => "::";
let string_of_ty_op: UHTyp.operator => string =
  fun
  | Sum => "|"
  | Prod => ","
  | Arrow => "->";
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
let associate_exp = (seq: UHExp.seq) => {
  let (skel_str, _) = make_skel_str(seq, string_of_expr_op);
  parse_expr(skel_str);
};
let associate_pat = (seq: UHPat.seq) => {
  let (skel_str, _) = make_skel_str(seq, string_of_pat_op);
  parse_pat(skel_str);
};
let associate_ty = (seq: UHTyp.seq) => {
  let (skel_str, _) = make_skel_str(seq, string_of_ty_op);
  parse_typ(skel_str);
};
