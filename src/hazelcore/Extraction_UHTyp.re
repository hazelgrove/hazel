open Extraction_declear;


//==============================
//  UHTyp.re
//==============================

// translate type constructor into ocaml type
//opseq is when using bi-argument operations like plus,
//skel.t use BinOp to indicate the operation(op) and two placeholder
//  in UHTyp, it's one the type, like Num -> Num
//  It's like skel.t will always receive NotInHole, so ignore it
//opseq is the operation, ExpOpExp is to do calculation,
//  SeqOpExp is to do sequence operation, like 1+2+3
let rec uhtyp_translater = (~t: UHTyp.t): option(string) =>
  switch (t) {
  | Hole => None
  //   | Hole => Some("'a")
  // //FIXME: Here is a version with type inference
  // //  such as (\lambda x:?.x+1) 1 can return 2, but leave a ? hole as "Hole"
  // //ocaml can do type inference, so just change hole to 'a
  | Num => Some("int")
  | Bool => Some("bool")
  | Unit => Some("()") //written as (), actually is unit
  | List(a) =>
    option_string_concat(~strs=[uhtyp_translater(~t=a), Some(" list")])
  | Parenthesized(a) =>
    option_string_concat(
      ~strs=[Some("("), uhtyp_translater(~t=a), Some(")")],
    )
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    // Since skeleton is consistant with opseq, decline skel_t
    | BinOp(NotInHole, _, _, _) => uhtyp_opseq_translater(~opseq)
    | _ => None
    }
  }
and uhtyp_opseq_translater = (~opseq): option(string) =>
  switch (opseq) {
  | ExpOpExp(tm1, op, tm2) =>
    option_string_concat(
      ~strs=[
        uhtyp_translater(~t=tm1),
        Some(uhtyp_op_translater(~op)),
        uhtyp_translater(~t=tm2),
      ],
    )
  | SeqOpExp(seq, op, tm) =>
    option_string_concat(
      ~strs=[
        uhtyp_opseq_translater(~opseq=seq),
        Some(uhtyp_op_translater(~op)),
        uhtyp_translater(~t=tm),
      ],
    )
  }
and uhtyp_op_translater = (~op: UHTyp.op): string =>
  switch (op) {
  | Arrow => " -> "
  | Prod => " * " //int*int in ocaml
  | Sum => " | "
  };
