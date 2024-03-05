type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | IndetMatch;

let matches: (Pat.t, DExp.t) => match_result;
