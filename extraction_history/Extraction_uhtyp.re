open Extraction_declear;
open Extraction_tool;

//This file will extract UHTyp

// this is the translation to string of symbols
// let uhtyp_operator_trans = (~ope1:UHTyp.operand, ~op : UHTyp.operator, ~ope2:UHTyp.operand) : extract_t =>
//     switch(op){
//         | Arrow => (Some("->"), UNK)
//         | Prod => (Some("*"), UNK)
//         | Sum => (Some("|"), UNK)
//     };

let rec uhtyp_trans = (~t: UHTyp.t): extract_t =>
  switch (t) {
  | OpSeq(_oprand, a) => uhtyp_seq_trans(~t=a)
  }
and uhtyp_seq_trans = (~t: Seq.t('operand, 'operator)): extract_t =>
  switch (t) {
  //I don't think oprand is necessary
  | S(operand, affix_e) =>
    switch (affix_e) {
    | E => uhtyp_operand_trans(~ope=operand) //only an operand
    | A(operator, seqt) =>
      uhtyp_const(
        ~ope1=operand,
        ~op=operator,
        ~ope2=uhtyp_seq_trans(~t=seqt),
      )
    }
  }
and uhtyp_operand_trans = (~ope: UHTyp.operand): extract_t =>
  switch (ope) {
  | Hole => (None, HOLE)
  | Unit => (Some("()"), Unit)
  | Num => (Some("int"), Number)
  | Bool => (Some("bool"), Bool)
  | Parenthesized(t) =>
    extract_t_concat(
      ~le=[(Some("("), UNK), uhtyp_trans(~t), (Some(")"), UNK)],
    )
  | List(t) =>
    extract_t_concat(~le=[uhtyp_trans(~t), (Some(" list"), UNK)])
  }
and uhtyp_const =
    (~ope1: UHTyp.operand, ~op: UHTyp.operator, ~ope2: extract_t): extract_t => {
  let (s1, p1) = uhtyp_operand_trans(~ope=ope1);
  let (s2, p2) = ope2;
  switch (op) {
  | Arrow => (
      option_string_concat(~strs=[s1, Some("->"), s2]),
      ARROW(p1, p2),
    )
  | Sum => (option_string_concat(~strs=[s1, Some("|"), s2]), SUM(p1, p2))
  | Prod => (
      option_string_concat(~strs=[s1, Some("*"), s2]),
      PROD(p1, p2),
    )
  };
};
