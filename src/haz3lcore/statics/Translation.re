let translate_to_roc = (t: TermBase.UExp.t): TermRoc.UExp.t =>
  switch (t) {
  | _ => {ids: [], term: Bool(false)}
  };
