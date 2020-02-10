open Extraction_declear;

//==============================
// UHPat.re
//==============================

let rec uhpat_translater = (~t: UHPat.t): option(string) =>
  switch (t) {
  | EmptyHole(_) => None
  | Wild(a) =>
    switch (a) {
    | NotInHole => Some("_")
    | _ => None
    }
  | Var(a, b, c) =>
    switch (a, b) {
    | (NotInHole, NotInVarHole) => Some(c)
    | _ => None
    }
  | NumLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_int(b))
    | _ => None
    }
  | BoolLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_bool(b))
    | _ => None
    }
  | ListNil(a) =>
    switch (a) {
    | NotInHole => Some("[]")
    | _ => None
    }
  | Parenthesized(t) =>
    switch (uhpat_translater(~t)) {
    | None => None
    | Some(s) => Some("(" ++ s ++ ")")
    }
  //FIXME: currently we use polymorphic type ('a) for it in "Let" assignment,
  // better to reconstruct for a type
  // (though the inference is good for that)
  // in Hazel, a type Num | Bool can't have value 1,
  //   it should be type inj[L or R, Bool](1)
  // in ocaml, sum type should be directly give value by constructor
  //   and need a type declaration for sum type
  // Hence change "x : (A | B) = inj[L](val)" where val:A into
  //   "x : 'a = val" temporarily, :'a can be ignored
  //   here we ignore 'a, because imagine assignment is legal
  | Inj(a, _b, c) =>
    switch (a) {
    | NotInHole => uhpat_translater(~t=c)
    | _ => None
    }
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    | BinOp(NotInHole, _, _, _) => uhpat_opseq_translater(~opseq)
    | _ => None
    }
  }
and uhpat_opseq_translater = (~opseq): option(string) =>
  switch (opseq) {
  | ExpOpExp(tm1, op, tm2) =>
    option_string_concat(
      ~strs=[
        uhpat_translater(~t=tm1),
        Some(uhpat_op_translater(~op)),
        uhpat_translater(~t=tm2),
      ],
    )
  | SeqOpExp(seq, op, tm) =>
    option_string_concat(
      ~strs=[
        uhpat_opseq_translater(~opseq=seq),
        Some(uhpat_op_translater(~op)),
        uhpat_translater(~t=tm),
      ],
    )
  }
and uhpat_op_translater = (~op: UHPat.op): string =>
  switch (op) {
  | Comma => ", "
  | Space => " "
  | Cons => " :: "
  };