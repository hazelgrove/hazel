let mk = (~err: AbbrevErrStatus.t, llp: LLPat.t, lle: LLExp.t): UHExp.line =>
  UHExp.letline(~err, LLPat.to_uhpat(llp), LLExp.to_uhexp(lle));
