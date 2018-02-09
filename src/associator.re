open Semantics.Core;

let parse s => {
  let lexbuf = Lexing.from_string s;
  Skelparser.skel Skellexer.read lexbuf
};

let string_of_op (op: UHExp.op) =>
  switch op {
  | UHExp.Plus => "+"
  | UHExp.Times => "*"
  | UHExp.Space => "_"
  };

let rec make_skel_str' (seq: UHExp.opseq) (counter: ref int) (ph_map: Hashtbl.t int UHExp.t) =>
  switch seq {
  | UHExp.ExpOpExp e1 op e2 =>
    let n = !counter;
    counter := n + 2;
    Hashtbl.add ph_map n e1;
    Hashtbl.add ph_map (n + 1) e2;
    let op_str = string_of_op op;
    string_of_int n ^ op_str ^ string_of_int (n + 1)
  | UHExp.SeqOpExp seq' op e =>
    let skel_str = make_skel_str' seq' counter ph_map;
    let op_str = string_of_op op;
    let n = !counter;
    counter := n + 1;
    Hashtbl.add ph_map n e;
    skel_str ^ op_str ^ string_of_int n
  };

let make_skel_str (seq: UHExp.opseq) => {
  let counter = ref 0;
  let ph_map = Hashtbl.create 8;
  let skel_str = make_skel_str' seq counter ph_map;
  (skel_str, ph_map)
};

let rec associate (seq: UHExp.opseq) => {
  let (skel_str, ph_map) = make_skel_str seq;
  parse skel_str
};
