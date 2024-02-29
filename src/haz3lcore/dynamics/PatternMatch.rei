type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let matches: (TermBase.UPat.t, DHExp.t) => match_result;
