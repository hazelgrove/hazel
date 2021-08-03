module type SUBJECT_EXP = {
  // MetaVarGen used to number type errors
  let elab:
    (FrameInfo_exp.t, Term_exp.t, MetaVarGen.t) =>
    (SubjectInfo_exp.t, MetaVarGen.t);

  let syn: (FrameInfo_exp.t, Term_exp.t) => Type.t;
};

module type FRAME_EXP = {
  let prod_n: (int, int, FrameInfo_exp.t as 'f) => 'f;

  let fun_body: (Term_pat.t, FrameInfo_exp.t as 'f) => 'f;

  let let_def: (Term_pat.t, FrameInfo_exp.t as 'f) => 'f;
  let let_body: (Term_pat.t, Term_exp.t, FrameInfo_exp.t) => 'f;

  let ap_fn: (FrameInfo_exp.t as 'f) => 'f;
  let ap_arg: (Term_exp.t, FrameInfo_exp.t as 'f) => 'f;

  let case_scrut: (FrameInfo_exp.t as 'f) => 'f;
  let case_clauses: (Term_exp.t, list(Term_exp.rule), FrameInfo_exp.t as 'f) => list('f);

  let mk: (~init: FrameInfo_exp.t as 'f=?, Frame_exp.t) => 'f;
};

module type FRAME_PAT = {
  let fun_pat: FrameInfo_exp.t => FrameInfo_pat.t;

  let let_pat: FrameInfo_exp.t => FrameInfo_pat.t;

  let case_pat: (Term_exp.t, FrameInfo_exp.t) => FrameInfo_pat.t;

  let mk: (~init: FrameInfo_exp.t=?, Frame_pat.t) => FrameInfo_pat.t;
};

module type SUBJECT = {
  module Pat: SUBJECT_PAT;
  module Exp: SUBJECT_EXP;
};
module type FRAME = {
  module Exp: FRAME_EXP;
  module Pat: FRAME_PAT;
};
