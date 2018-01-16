open Semantics.Core;

let parse s => {
  let lexbuf = Lexing.from_string s;
  Skelparser.skel Skellexer.read lexbuf
};

let string_of_op (op: AHExp.op) =>
  switch op {
  | AHExp.Plus => "+"
  | AHExp.Times => "*"
  | AHExp.Space => "_"
  };

let rec make_skel_str' (seq: UHExp.opseq) (counter: ref int) (ph_map: Hashtbl.t int UHExp.t) =>
  switch seq {
  | UHExp.BareExp ue =>
    let n = !counter;
    counter := n + 1;
    Hashtbl.add ph_map n ue;
    string_of_int n
  | UHExp.SeqOpExp seq' op ue =>
    let skel_str = make_skel_str' seq' counter ph_map;
    let op_str = string_of_op op;
    let n = !counter;
    counter := n + 1;
    Hashtbl.add ph_map n ue;
    skel_str ^ op_str ^ string_of_int n
  };

let make_skel_str (seq: UHExp.opseq) => {
  let counter = ref 0;
  let ph_map = Hashtbl.create 8;
  let skel_str = make_skel_str' seq counter ph_map;
  (skel_str, ph_map)
};

let rec associate (ue: UHExp.t) =>
  switch ue {
  | UHExp.Asc ue1 ty => AHExp.Asc (associate ue1) ty
  | UHExp.Var x => AHExp.Var x
  | UHExp.Let x ue1 ue2 => AHExp.Let x (associate ue1) (associate ue2)
  | UHExp.Lam x ue1 => AHExp.Lam x (associate ue1)
  | UHExp.Ap ue1 ue2 => AHExp.Ap (associate ue1) (associate ue2)
  | UHExp.NumLit n => AHExp.NumLit n
  | UHExp.Inj side ue1 => AHExp.Inj side (associate ue1)
  | UHExp.Case ue1 (x, ue2) (y, ue3) =>
    AHExp.Case (associate ue1) (x, associate ue2) (y, associate ue3)
  | UHExp.EmptyHole u => AHExp.EmptyHole u
  | UHExp.NonEmptyHole u ue1 => AHExp.NonEmptyHole u (associate ue1)
  | UHExp.OpSeq seq =>
    let (skel_str, ph_map) = make_skel_str seq;
    let skel = parse skel_str;
    ahexp_of skel ph_map
  }
and ahexp_of skel ph_map =>
  switch skel {
  | Skel.Placeholder n =>
    let ue = Hashtbl.find ph_map n;
    associate ue
  | Skel.BinOp (op, skel1, skel2) =>
    let ae1 = ahexp_of skel1 ph_map;
    let ae2 = ahexp_of skel2 ph_map;
    AHExp.BinOp op ae1 ae2
  };
