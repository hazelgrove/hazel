type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let matches: (DHPat.t, DHExp.t) => match_result;
