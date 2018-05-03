open Semantics.Core;

open LangUtil;

/* TODO: use Formatter's fancy box stuff for lines and indentation */
let ensure_well_typed_for_serialization uhexp =>
  switch (UHExp.syn () Ctx.empty uhexp) {
  | None => raise (IllFormed uhexp)
  | _ => uhexp
  };

let ensure_well_typed_for_parsing uhexp =>
  switch (UHExp.fix_and_renumber_holes () uhexp) {
  | None => raise (IllFormed uhexp)
  | Some ((e, t), mv) => (e, t, mv)
  };

let string_of_tyop op =>
  switch op {
  | UHTyp.Arrow => "->"
  | UHTyp.Sum => "|"
  };

let string_of_expop op =>
  switch op {
  | UHExp.Plus => "+"
  | UHExp.Times => "*"
  | UHExp.Space => " "
  };

let hz_serialize fmtr uhexp => {
  let print s => Format.pp_print_string fmtr s;
  let printf fmt arg1 => Format.fprintf fmtr fmt arg1;
  let printf2 fmt arg1 arg2 => Format.fprintf fmtr fmt arg1 arg2;
  let print_int n => Format.pp_print_int fmtr n;
  let rec print_opseq print_term print_op seq =>
    switch seq {
    | OperatorSeq.ExpOpExp tm1 op tm2 =>
      print_term tm1;
      print_op op;
      print_term tm2
    | OperatorSeq.SeqOpExp seq' op tm2 =>
      print_opseq print_term print_op seq';
      print_op op;
      print_term tm2
    };
  let rec print_htyp tau =>
    switch tau {
    | UHTyp.Parenthesized tau1 =>
      print "(";
      print_htyp tau1;
      print ")"
    | UHTyp.Num => print "num"
    | UHTyp.Hole => print "{}"
    | UHTyp.OpSeq skel seq =>
      print_opseq print_htyp (GeneralUtil.compose print string_of_tyop) seq
    };
  let rec print_uhexp e =>
    switch e {
    | UHExp.Parenthesized e' =>
      print "(";
      print_uhexp e';
      print ")"
    | UHExp.Tm _ tm =>
      switch tm {
      | UHExp.Asc e' tau =>
        print_uhexp e';
        print " : ";
        print_htyp tau
      | UHExp.Var x => print x
      | UHExp.Let x e1 e2 =>
        printf "let %s = " x;
        print_uhexp e1;
        print " in\n";
        print_uhexp e2
      | UHExp.Lam x e' =>
        printf2 "%s%s." lamSym x;
        print_uhexp e'
      | UHExp.NumLit n => print_int n
      | UHExp.Inj side e' =>
        printf "inj[%s](" (string_of_side side);
        print_uhexp e';
        print ")"
      | UHExp.Case e' (vL, eL) (vR, eR) =>
        print "case ";
        print_uhexp e';
        printf2 "\nL(%s) %s " vL caseArrowSym;
        print_uhexp eL;
        printf2 "\nR(%s) %s " vR caseArrowSym;
        print_uhexp eR
      | UHExp.EmptyHole u => print "{}"
      | UHExp.OpSeq skel seq =>
        print_opseq print_uhexp (GeneralUtil.compose print string_of_expop) seq
      }
    };
  print_uhexp (ensure_well_typed_for_serialization uhexp)
};

let string_of_uhexp hexp => {
  let buf = Buffer.create 32;
  let fmtr = Format.formatter_of_buffer buf;
  hz_serialize fmtr hexp;
  Format.pp_print_flush fmtr ();
  Buffer.contents buf
};

let parse' lexbuf => HZParse.parse_uhexp HZLex.read lexbuf;

let hz_parse i_channel =>
  try (ensure_well_typed_for_parsing (parse' (Lexing.from_channel i_channel))) {
  | HZParse.Error => raise (InvalidSyntax "Syntax error")
  };

let uhexp_of_string s =>
  try (
    switch (ensure_well_typed_for_parsing (parse' (Lexing.from_string s))) {
    | (e, _, _) => e
    }
  ) {
  | HZParse.Error => raise (InvalidSyntax s)
  };
