//============================
// FIXME: Get Rid of it!!!
//  Opseq destruction
// I don't want to do so,
//    but... I need to extract argument for a dependent type
//    at least for now
//============================

// UHPat.t -> operand
let uhpat_operand = (~t: UHPat.t): UHPat.operand =>
  switch (t) {
  | opseq =>
    switch (opseq) {
    | OpSeq.OpSeq(_, b) =>
      switch (b) {
      | S(op, _) => op
      }
    }
  };

let uhtyp_opseq_operand = (~op: UHTyp.opseq): UHTyp.operand =>
  switch (op) {
  | OpSeq.OpSeq(_, b) =>
    switch (b) {
    | S(a, _) => a
    }
  };

let uhtyp_operand = (~t: UHTyp.t): UHTyp.operand =>
  switch (t) {
  | opseq => uhtyp_opseq_operand(~op=opseq)
  };
