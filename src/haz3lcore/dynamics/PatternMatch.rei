type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let matches: (Pat.t, DHExp.t) => match_result;
