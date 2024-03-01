type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let matches: (TermBase.UPat.t, DExp.t) => match_result;
