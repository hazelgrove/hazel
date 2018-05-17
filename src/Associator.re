open Semantics.Core;

let parse_expr s => {
  let lexbuf = Lexing.from_string s;
  SkelExprParser.skel_expr SkelExprLexer.read lexbuf
};

let parse_typ s => {
  let lexbuf = Lexing.from_string s;
  SkelTypParser.skel_typ SkelTypLexer.read lexbuf
};

let string_of_expr_op (op: UHExp.op) =>
  switch op {
  | UHExp.Plus => "+"
  | UHExp.Times => "*"
  | UHExp.Space => "_"
  };

let string_of_ty_op (op: UHTyp.op) =>
  switch op {
  | UHTyp.Sum => "|"
  | UHTyp.Arrow => "->"
  };

let rec make_skel_str'
        (string_of_op: 'op => string)
        (seq: OperatorSeq.opseq 'tm 'op)
        (counter: ref int)
        (ph_map: Hashtbl.t int 'tm) =>
  switch seq {
  | OperatorSeq.ExpOpExp e1 op e2 =>
    let n = !counter;
    counter := n + 2;
    Hashtbl.add ph_map n e1;
    Hashtbl.add ph_map (n + 1) e2;
    let op_str = string_of_op op;
    string_of_int n ^ op_str ^ string_of_int (n + 1)
  | OperatorSeq.SeqOpExp seq' op e =>
    let skel_str = make_skel_str' string_of_op seq' counter ph_map;
    let op_str = string_of_op op;
    let n = !counter;
    counter := n + 1;
    Hashtbl.add ph_map n e;
    skel_str ^ op_str ^ string_of_int n
  };

let make_skel_str
    (seq: OperatorSeq.opseq 'tm 'op)
    (string_of_op: 'op => string) => {
  let counter = ref 0;
  let ph_map = Hashtbl.create 8;
  let skel_str = make_skel_str' string_of_op seq counter ph_map;
  (skel_str, ph_map)
};

let rec associate_exp (seq: UHExp.opseq) => {
  let (skel_str, _) = make_skel_str seq string_of_expr_op;
  parse_expr skel_str
};

let rec associate_ty (seq: UHTyp.opseq) => {
  let (skel_str, _) = make_skel_str seq string_of_ty_op;
  parse_typ skel_str
};
